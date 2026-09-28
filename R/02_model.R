# ==============================================================================
# R/02_model.R
# Build Stan inputs for one (season, week) snapshot and fit the Power model.
# ==============================================================================

#' Model and sampler settings used everywhere (edit here, not at call sites)
model_config <- function() {
  list(
    stan_file = "stan/power.stan",
    chains = 4, iter_warmup = 500, iter_sampling = 1000, adapt_delta = 0.9,
    retry_adapt_delta = 0.99, retry_warmup = 1000, retry_sampling = 2000,
    # Priors are centred on full-season posterior means pooled over 2014-2025, with sd = 2x the
    # between-season sd (analysis/tune_priors.R -> analysis/structural_pooled.csv). They only
    # matter in the first weeks; by mid-season each season's own data dominates.
    hfa_mean = 2.5, hfa_sd = 0.7,
    tier_mean = c(-27.3, -54), tier_sd = c(3.7, 10.2),      # FCS, non-D1 offsets vs. FBS
    b_prev_mean = 0.75, b_prev_sd = 0.13,                    # year-to-year carry-over
    sigma_team_mean = c(7.6, 7.6, 13.6), sigma_team_sd = c(1.6, 2.5, 5.1),
    sigma_conf_prior = c(4.4, 3.9),
    lambda_prior = c(0.755, 0.07), kappa_prior = c(0.77, 0.06),
    sigma_eff_prior = c(14.1, 1.1), sigma_m_prior = c(8.1, 1.0),
    nu_prior = c(2, 0.1), nu_lower = 1,                      # Gamma(shape, rate) truncated to nu > nu_lower
    half_life_weeks = NULL,          # NULL = every game weighted equally
    # Model variants (the published model uses all four; the ablations switch one off)
    use_eff = TRUE,                  # efficiency likelihood
    student_t = TRUE,                # Student-t margin (FALSE = normal)
    conf_level = TRUE,               # conference effects (FALSE = teams pooled to the tier only)
    carry_over = TRUE,               # last season's ratings as preseason information
    benchmark_rank = 12L,            # Resume benchmark: the #12 FBS team (playoff bubble)
    min_plays = 20L,                 # both offences need this many non-garbage plays for eff
    summary_draws = 1000L,           # thinned draws used for Resume / predictions (memory)
    gates = list(rhat = 1.01, ess = 400, divergences = 0, ebfmi = 0.2)
  )
}

tier_of <- function(class) {
  dplyr::case_when(class == "fbs" ~ 1L, class == "fcs" ~ 2L, TRUE ~ 3L)
}

#' Team table for a season: every team on the schedule, with tier and conference
season_teams <- function(games) {
  long <- dplyr::bind_rows(
    games |> dplyr::transmute(team_id = home_id, team = home_team, conference = home_conference, class = home_class, week_seq),
    games |> dplyr::transmute(team_id = away_id, team = away_team, conference = away_conference, class = away_class, week_seq)
  )
  long |>
    dplyr::arrange(dplyr::desc(week_seq)) |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      team = dplyr::first(team),
      conference = dplyr::first(stats::na.omit(conference)) %||% NA_character_,
      class = dplyr::first(stats::na.omit(class)) %||% NA_character_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      tier = tier_of(class),
      conference = dplyr::case_when(
        tier == 3L ~ "Non-D1",
        is.na(conference) & tier == 1L ~ "FBS Independents",
        is.na(conference) ~ "FCS Independents",
        TRUE ~ conference
      ),
      # Independents are not a conference: each gets its own conference effect, drawn from
      # the tier like any conference. Non-D1 teams stay pooled in one node.
      conf_key = ifelse(grepl("Independents$", conference), paste(tier, conference, team_id), paste(tier, conference))
    ) |>
    dplyr::arrange(tier, team) |>
    dplyr::mutate(idx = dplyr::row_number())
}

#' Per-game model observations (regulation margin, efficiency margin)
game_observations <- function(games, min_plays = 20L) {
  games |>
    dplyr::mutate(
      margin = dplyr::coalesce(as.numeric(reg_margin), as.numeric(home_points - away_points)),
      has_eff = !is.na(home_ppa) & !is.na(away_ppa) &
        dplyr::coalesce(home_plays, 0L) >= min_plays & dplyr::coalesce(away_plays, 0L) >= min_plays,
      eff = ifelse(has_eff, home_ppa * home_plays - away_ppa * away_plays, 0),
      home_adv = ifelse(neutral_site, 0, 1),
      home_win = as.integer(home_points > away_points)
    )
}

#' Preseason carry-over: last season's final rating relative to the mean of the team's tier
#' (FBS / FCS / non-D1) among teams with a previous rating; 0 when unknown. Measuring it against
#' the tier (not the conference) carries conference strength over too, so preseason
#' cross-conference predictions are informative; mu_conf then absorbs only the change.
#' The tier is this season's, so a team moving up (FCS -> FBS) keeps its absolute level:
#' its old FCS rating sits far below the FBS mean and its d_i is strongly negative.
#' @return list(dev, sd): d_i and the posterior sd of last season's rating (0 when unknown)
prev_deviation <- function(teams, prior) {
  if (is.null(prior) || nrow(prior) == 0) return(list(dev = rep(0, nrow(teams)), sd = rep(0, nrow(teams))))
  if (!"rating_sd" %in% names(prior)) prior$rating_sd <- 0
  x <- teams |>
    dplyr::left_join(prior |> dplyr::select(team_id, prev = rating, prev_sd = rating_sd), by = "team_id") |>
    dplyr::group_by(tier) |>
    dplyr::mutate(dev = prev - mean(prev, na.rm = TRUE)) |>
    dplyr::ungroup()
  list(dev = dplyr::coalesce(x$dev, 0), sd = ifelse(is.na(x$dev), 0, dplyr::coalesce(x$prev_sd, 0)))
}

#' Everything needed to fit and summarise one snapshot (season, week_seq)
#' @param season_data list from read_season()
#' @param week week_seq to fit through
#' @param prior tibble(team_id, rating) from the previous season's final snapshot, or NULL
prepare_week_input <- function(season_data, week, prior = NULL, cfg = model_config()) {
  games <- season_data$games
  teams <- season_teams(games)
  confs <- teams |> dplyr::distinct(conf_key, tier) |> dplyr::arrange(tier, conf_key) |>
    dplyr::mutate(conf_idx = dplyr::row_number())
  teams <- teams |> dplyr::left_join(confs |> dplyr::select(conf_key, conf_idx), by = "conf_key") |>
    dplyr::select(idx, team_id, team, conference, tier, conf_key, conf_idx)

  obs <- game_observations(games, cfg$min_plays) |>
    dplyr::mutate(home = match(home_id, teams$team_id), away = match(away_id, teams$team_id))
  # Keep only the columns the fit and summaries use, so a snapshot's input (and therefore its
  # cached fit) doesn't change when later weeks' results or lines arrive
  played <- obs |>
    dplyr::filter(completed, week_seq <= .env$week, !is.na(home_points), !is.na(away_points)) |>
    dplyr::select(game_id, week_seq, home, away, home_adv, home_points, away_points, margin, has_eff, eff)
  future <- obs |>
    dplyr::filter(week_seq > .env$week) |>
    dplyr::select(game_id, week_seq, home, away, home_adv)

  weight <- if (is.null(cfg$half_life_weeks)) rep(1, nrow(played)) else
    0.5^((week - played$week_seq) / cfg$half_life_weeks)
  prev <- prev_deviation(teams, if (isFALSE(cfg$carry_over)) NULL else prior)

  stan_data <- list(
    N_teams = nrow(teams), N_conf = nrow(confs),
    team_conf = teams$conf_idx, conf_tier = confs$tier, team_tier = teams$tier,
    prev_dev = prev$dev, prev_sd = prev$sd,
    N_games = nrow(played), home = played$home, away = played$away,
    home_adv = played$home_adv, margin = played$margin,
    has_eff = as.integer(played$has_eff & !isFALSE(cfg$use_eff)), eff = if (isFALSE(cfg$use_eff)) rep(0, nrow(played)) else played$eff,
    weight = weight,
    hfa_mean = cfg$hfa_mean, hfa_sd = cfg$hfa_sd,
    tier_mean = cfg$tier_mean, tier_sd = cfg$tier_sd,
    b_prev_mean = cfg$b_prev_mean, b_prev_sd = cfg$b_prev_sd,
    sigma_team_mean = cfg$sigma_team_mean, sigma_team_sd = cfg$sigma_team_sd,
    sigma_conf_prior = cfg$sigma_conf_prior, lambda_prior = cfg$lambda_prior, kappa_prior = cfg$kappa_prior,
    sigma_eff_prior = cfg$sigma_eff_prior, sigma_m_prior = cfg$sigma_m_prior,
    nu_prior = cfg$nu_prior %||% c(2, 0.1), nu_lower = cfg$nu_lower %||% 1,
    student_t = as.integer(!isFALSE(cfg$student_t)), conf_level = as.integer(!isFALSE(cfg$conf_level)),
    compute_ll = 0L
  )

  list(
    season = season_data$season, week = as.integer(week),
    week_label = if (week == 0) "Preseason" else unique(games$week_label[games$week_seq == week])[1],
    last_regular_week = max(games$week_seq[games$season_type == "regular"]),
    teams = teams, played = played, future = future,
    has_prior = !is.null(prior) && nrow(prior) > 0 && !isFALSE(cfg$carry_over),
    stan_data = stan_data
  )
}

#' Summaries of sampler health for the gates
sampler_health <- function(fit, cfg = model_config()) {
  vars <- c("r", "hfa", "sigma_m", "nu", "lambda", "sigma_eff", "kappa", "b_prev", "mu_tier", "sigma_team", "sigma_conf")
  s <- posterior::summarise_draws(fit$draws(variables = vars), "rhat", "ess_bulk", "ess_tail")
  d <- fit$diagnostic_summary(quiet = TRUE)
  g <- cfg$gates
  out <- tibble::tibble(
    max_rhat = max(s$rhat, na.rm = TRUE),
    min_ess_bulk = min(s$ess_bulk, na.rm = TRUE),
    min_ess_tail = min(s$ess_tail, na.rm = TRUE),
    divergences = sum(d$num_divergent),
    min_ebfmi = min(d$ebfmi)
  )
  out$passed <- out$max_rhat <= g$rhat && out$min_ess_bulk >= g$ess && out$min_ess_tail >= g$ess &&
    out$divergences <= g$divergences && out$min_ebfmi > g$ebfmi
  out
}

#' Fit the Power model; retry once with a longer, more careful warmup and more draws if gates fail.
#' @param strict stop() if the gates still fail after the retry (used for published snapshots)
fit_power <- function(input, cfg = model_config(), compute_ll = FALSE, strict = FALSE, seed = 2026) {
  mod <- cmdstanr::cmdstan_model(cfg$stan_file)
  data <- input$stan_data
  data$compute_ll <- as.integer(compute_ll)
  run <- function(warmup, adapt_delta, sampling) {
    mod$sample(
      data = data, chains = cfg$chains, parallel_chains = cfg$chains,
      iter_warmup = warmup, iter_sampling = sampling, adapt_delta = adapt_delta,
      seed = seed, refresh = 0, show_messages = FALSE, show_exceptions = FALSE
    )
  }
  t0 <- Sys.time()
  fit <- run(cfg$iter_warmup, cfg$adapt_delta, cfg$iter_sampling)
  health <- sampler_health(fit, cfg)
  attempts <- 1L
  if (!health$passed) {
    # Longer warmup, smaller steps, and twice the draws (low-ESS misses are usually just short chains)
    fit <- run(cfg$retry_warmup, cfg$retry_adapt_delta, cfg$retry_sampling)
    health <- sampler_health(fit, cfg)
    attempts <- 2L
  }
  health$attempts <- attempts
  health$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  msg <- sprintf("[Power] %d week %d: gates %s (R-hat %.3f, ESS %.0f/%.0f, div %d, E-BFMI %.2f)",
                 input$season, input$week, if (health$passed) "passed" else "FAILED",
                 health$max_rhat, health$min_ess_bulk, health$min_ess_tail, health$divergences, health$min_ebfmi)
  if (!health$passed && strict) stop(msg)
  if (!health$passed) warning(msg) else message(msg)
  list(fit = fit, health = health)
}

#' Posterior draws needed downstream, as plain matrices/vectors (so the CmdStan CSVs can be deleted)
extract_draws <- function(fit, compute_ll = FALSE) {
  get <- function(v) posterior::as_draws_matrix(fit$draws(variables = v))
  scalars <- get(c("hfa", "sigma_m", "sigma_marg", "nu", "lambda", "sigma_eff", "kappa", "b_prev", "sigma_conf", "mu_tier", "sigma_team"))
  out <- list(
    r = unclass(get("r")),
    par = as.data.frame(unclass(scalars))
  )
  if (compute_ll) {
    out$log_lik <- unclass(get("log_lik"))
    out$lprior <- as.numeric(unclass(get("lprior")))
  }
  out
}
