# ==============================================================================
# R/02_model.R
# Build Stan inputs for one (season, week) snapshot and fit the Power model.
# ==============================================================================

#' Model and sampler settings used everywhere (edit here, not at call sites)
model_config <- function() {
  list(
    stan_file = "stan/power.stan",
    chains = 4, iter_warmup = 500, iter_sampling = 1000, adapt_delta = 0.9,
    # The retry lengthens warmup and sampling but keeps adapt_delta moderate: a much smaller step
    # size makes chains stall in the neck of the random-walk funnel (small tau)
    retry_adapt_delta = 0.95, retry_warmup = 1000, retry_sampling = 2000,
    # Every structural prior is pooled from vague-prior refits of each complete season's final week
    # (target prior_refits -> pool_priors() -> priors_from_pool(), with the `pool` rule below):
    # centred on the recency-weighted mean of the per-season posterior means, with the
    # random-effects predictive sd for a new season. Pooled 2026-10-11 from the 2015-2025 refits
    # under this model with the scale-t margin family (the margin_comparison choice since the
    # refits use a wide tau prior); check_build.R warns when the refits drift from these values.
    # The tier offsets' sd is floored at one season's posterior sd: a full season pins the offset
    # only to ~2 points, so the refits can't show season-to-season change below that, and the
    # unfloored rule (the sd of the mean, 0.49) left 2026's FBS-FCS gap stuck while FBS teams
    # beat FCS ones by 7 points more than predicted in weeks 0-3 (2026-10-23 variants: no cost
    # on 2015-2025, MAE -0.34 on 2026's early FBS-FCS games)
    pool = list(half_life = 3, sd_rule = "predictive", sd_mult = 2, sd_floor = 1,
                floor_vars = c("mu_tier[2]", "mu_tier[3]")),
    hfa_mean = 2.44, hfa_sd = 0.369,
    tier_mean = c(-28.4, -60.4), tier_sd = c(2.15, 4.85),      # FCS, non-D1 offsets vs. FBS
    b_prev_mean = 0.591, b_prev_sd = 0.0386,                    # year-to-year carry-over
    sigma_team_mean = c(5.00, 4.98, 11.4), sigma_team_sd = c(1.25, 0.476, 1.71),
    sigma_conf_prior = c(2.43, 0.330),
    lambda_prior = c(0.787, 0.0188), kappa_prior = c(0.747, 0.0228),
    sigma_eff_prior = c(14.0, 0.169), sigma_m_prior = c(6.60, 0.401),
    nu_prior = c(56.2, 1.83), nu_lower = 1,                    # Gamma(shape, rate) truncated to nu > nu_lower
    # Margin family: "t" (Student-t), "normal", "mixture" (two-normal scale mixture) or "scale_t"
    # (t whose scale grows with the expected margin). The published family is picked by the
    # margin_comparison target (choose_margin_family()); this is the incumbent it must beat.
    margin_family = "t",
    sigma_gap_prior = c(0, 10), p_wide_prior = c(2, 2), gamma_prior = c(0.282, 0.0144),
    # Weekly random walk of ratings within a season (FALSE = static ratings)
    dynamic = TRUE,
    # Weekly step sd of r and t (normal truncated at 0). One season identifies tau_r only weakly
    # (posterior sd ~0.33), and a prior with much mass near 0 makes a funnel with the steps
    tau_r_prior = c(1.57, 0.280), tau_t_prior = c(0.989, 0.106),
    # Total model (offence / defence)
    use_total = TRUE,
    base_prior = c(27.2, 0.641), t_tier_mean = c(-0.985, 0.102), t_tier_sd = c(0.816, 4.98),
    sigma_t_team_prior = c(4.05, 0.243), sigma_conf_t_prior = c(0.770, 0.0843), b_prev_t_prior = c(0.480, 0.125),
    lambda_t_prior = c(1.08, 0.107), alpha_es_prior = c(14.3, 1.41), kappa_t_prior = c(0.462, 0.0318),
    sigma_es_prior = c(16.6, 0.538), sigma_tot_prior = c(12.8, 0.324),
    # Preseason terms (stan/power.stan header). Returning production, talent and the rating two
    # seasons back improve weeks 0-3 out of sample (target prior_variants); a separate FCS carry-over
    # and a wider spread after a coaching change did not, and stay off (their priors are unpooled)
    b_prev_tier = FALSE, use_ret = TRUE, use_tal = TRUE, use_prev2 = TRUE, use_coach = FALSE,
    b_ret_prior = c(0.113, 0.0729), c_ret_prior = c(7.04, 4.11), b_tal_prior = c(1.87, 0.355),
    b_prev2_prior = c(0.144, 0.0392), b_prev_low_prior = c(0.62, 0.1), delta_coach_prior = c(0, 0.3),
    # Model variants (the published model uses all of these; the ablations switch one off)
    use_eff = TRUE,                  # efficiency likelihood
    conf_level = TRUE,               # conference effects (FALSE = teams pooled to the tier only)
    carry_over = TRUE,               # last season's ratings as preseason information
    benchmark_rank = 12L,            # Resume benchmark: the #12 FBS team (playoff bubble)
    min_plays = 20L,                 # both offences need this many non-garbage plays for eff
    summary_draws = 1000L,           # thinned draws used for Resume / predictions (memory)
    gates = list(rhat = 1.01, ess = 400, divergences = 0, ebfmi = 0.2)
  )
}

# First season the provider has each preseason input (returning production, talent composite);
# earlier seasons fit the term as 0
PRESEASON_FIRST_SEASON <- c(ret = 2014L, tal = 2015L)

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

#' Per-game model observations (regulation margin, efficiency margin, total and summed efficiency)
game_observations <- function(games, min_plays = 20L) {
  games |>
    dplyr::mutate(
      margin = dplyr::coalesce(as.numeric(reg_margin), as.numeric(home_points - away_points)),
      has_eff = !is.na(home_ppa) & !is.na(away_ppa) &
        dplyr::coalesce(home_plays, 0L) >= min_plays & dplyr::coalesce(away_plays, 0L) >= min_plays,
      eff = ifelse(has_eff, home_ppa * home_plays - away_ppa * away_plays, 0),
      eff_sum = ifelse(has_eff, home_ppa * home_plays + away_ppa * away_plays, 0),
      # Overtime inflates the total, so OT games inform the total model through eff_sum only
      has_total = !dplyr::coalesce(ot, FALSE) & !is.na(home_points) & !is.na(away_points),
      total = ifelse(has_total, as.numeric(home_points + away_points), 0),
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
#' `value`/`sd` name the prior's columns (rating / rating_sd, or total_t / total_t_sd).
#' @return list(dev, sd): d_i and the posterior sd of last season's rating (0 when unknown)
prev_deviation <- function(teams, prior, value = "rating", sd = paste0(value, "_sd")) {
  none <- list(dev = rep(0, nrow(teams)), sd = rep(0, nrow(teams)))
  if (is.null(prior) || nrow(prior) == 0 || !value %in% names(prior)) return(none)
  prior$prev <- prior[[value]]
  prior$prev_sd <- if (sd %in% names(prior)) prior[[sd]] else 0
  x <- teams |>
    dplyr::left_join(prior |> dplyr::select(team_id, prev, prev_sd), by = "team_id") |>
    dplyr::group_by(tier) |>
    dplyr::mutate(dev = prev - mean(prev, na.rm = TRUE)) |>
    dplyr::ungroup()
  list(dev = dplyr::coalesce(x$dev, 0), sd = ifelse(is.na(x$dev), 0, dplyr::coalesce(x$prev_sd, 0)))
}

#' Preseason covariates for FBS teams (0 for other tiers and for teams without the data):
#' returning production share centred within FBS, talent composite standardised within FBS,
#' and a new-head-coach indicator. A term that is switched on must have data: a season with
#' none (or for fewer than `min_share` of FBS teams) stops, and FBS teams that fall back to 0
#' are named in a warning. Seasons before the provider's first (PRESEASON_FIRST_SEASON) are 0.
#' @param preseason tibble(team_id, ret_ppa, talent, coach_change) from read_season(), or NULL
#' @param season the season being fitted (checks coverage against PRESEASON_FIRST_SEASON)
preseason_covariates <- function(teams, preseason, season, cfg = model_config(), min_share = 0.5) {
  n <- nrow(teams)
  fbs <- teams$tier == 1L
  if (is.null(preseason)) preseason <- tibble::tibble(team_id = integer(0), ret_ppa = numeric(0),
                                                      talent = numeric(0), coach_change = integer(0))
  x <- teams |> dplyr::left_join(preseason, by = "team_id")
  check <- function(v, key, label, on) {
    if (!isTRUE(on) || season < PRESEASON_FIRST_SEASON[[key]] || !any(fbs)) return(invisible())
    miss <- fbs & is.na(v)
    if (sum(!miss[fbs]) < min_share * sum(fbs)) {
      stop(sprintf("[preseason] %d: %s for %d of %d FBS teams. Sync it (sync_preseason()) or switch off cfg$use_%s.",
                   season, label, sum(!miss[fbs]), sum(fbs), key), call. = FALSE)
    }
    if (any(miss)) {
      warning(sprintf("[preseason] %d: no %s for %s; fitted as 0 (the FBS mean)",
                      season, label, paste(teams$team[miss], collapse = ", ")), call. = FALSE)
    }
  }
  check(x$ret_ppa, "ret", "returning production", cfg$use_ret)
  check(x$talent, "tal", "talent composite", cfg$use_tal)
  centre <- function(v, scale = FALSE) {
    v[!fbs] <- NA
    ok <- !is.na(v)
    if (sum(ok) < 2) return(rep(0, n))
    out <- v - mean(v[ok])
    if (scale) out <- out / stats::sd(v[ok])
    dplyr::coalesce(out, 0)
  }
  list(ret = centre(x$ret_ppa), tal = centre(x$talent, scale = TRUE),
       coach_new = ifelse(fbs, dplyr::coalesce(as.numeric(x$coach_change), 0), 0))
}

#' Random-walk states: one per (team, week with a game), grouped by team and in week order within
#' a team (Stan sums each team's steps over its block). Week 1 is
#' the time origin (the preseason level r0 is the week-1 rating), so a team's first state steps
#' max(week - 1, 0) weeks from r0, and later states the weeks since its previous game.
#' @return list(states, home_state, away_state, now_state, now_gap); now = the snapshot week
walk_states <- function(played, n_teams, week) {
  now <- max(week, 1L)
  tw <- dplyr::bind_rows(
    tibble::tibble(team = played$home, week_seq = played$week_seq),
    tibble::tibble(team = played$away, week_seq = played$week_seq)
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(team, week_seq) |>
    dplyr::mutate(state = dplyr::row_number()) |>
    dplyr::group_by(team) |>
    dplyr::mutate(prev = dplyr::lag(state, default = 0L), first = dplyr::first(state),
                  gap = pmax(week_seq - dplyr::lag(week_seq, default = 1L), 0L)) |>
    dplyr::ungroup()
  key <- function(tm, wk) match(paste(tm, wk), paste(tw$team, tw$week_seq))
  last <- tw |> dplyr::group_by(team) |> dplyr::slice_max(week_seq, n = 1) |> dplyr::ungroup()
  now_state <- integer(n_teams); now_gap <- rep(now - 1, n_teams)
  now_state[last$team] <- last$state
  now_gap[last$team] <- pmax(now - last$week_seq, 0)
  list(states = tw, home_state = key(played$home, played$week_seq), away_state = key(played$away, played$week_seq),
       now_state = now_state, now_gap = as.numeric(now_gap))
}

#' Everything needed to fit and summarise one snapshot (season, week_seq)
#' @param season_data list from read_season()
#' @param week week_seq to fit through
#' @param prior tibble(team_id, rating, rating_sd, total_t, total_t_sd) from the previous
#'   season's final snapshot, or NULL
#' @param prior2 the same from two seasons back (only its ratings are used), or NULL
prepare_week_input <- function(season_data, week, prior = NULL, cfg = model_config(), prior2 = NULL) {
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
    dplyr::select(game_id, week_seq, home, away, home_adv, home_points, away_points, margin, has_eff, eff,
                  eff_sum, has_total, total)
  future <- obs |>
    dplyr::filter(week_seq > .env$week) |>
    dplyr::select(game_id, week_seq, home, away, home_adv)

  carry <- if (isFALSE(cfg$carry_over)) NULL else prior
  prev <- prev_deviation(teams, carry)
  prev_t <- prev_deviation(teams, carry, "total_t")
  prev2 <- prev_deviation(teams, if (isFALSE(cfg$carry_over)) NULL else prior2)
  cov <- preseason_covariates(teams, season_data$preseason, season_data$season, cfg)
  ws <- walk_states(played, nrow(teams), week)
  use_eff <- !isFALSE(cfg$use_eff)
  fam <- margin_family_code(cfg$margin_family %||% "t")

  stan_data <- list(
    N_teams = nrow(teams), N_conf = nrow(confs),
    team_conf = teams$conf_idx, conf_tier = confs$tier, team_tier = teams$tier,
    prev_dev = prev$dev, prev_sd = prev$sd, prev_dev_t = prev_t$dev, prev_sd_t = prev_t$sd,
    N_states = nrow(ws$states), state_team = ws$states$team, state_first = ws$states$first,
    state_prev = ws$states$prev,   # not read by Stan; used to simulate from the priors
    state_gap = as.numeric(ws$states$gap), now_state = ws$now_state, now_gap = ws$now_gap,
    N_games = nrow(played), home_state = ws$home_state, away_state = ws$away_state,
    home_adv = played$home_adv, margin = played$margin,
    has_eff = as.integer(played$has_eff & use_eff),
    eff = if (use_eff) played$eff else rep(0, nrow(played)),
    eff_sum = if (use_eff) played$eff_sum else rep(0, nrow(played)),
    has_total = as.integer(played$has_total), total = played$total,
    margin_family = fam, dynamic = as.integer(!isFALSE(cfg$dynamic)),
    use_total = as.integer(!isFALSE(cfg$use_total)), conf_level = as.integer(!isFALSE(cfg$conf_level)),
    ret = cov$ret, tal = cov$tal, prev2_dev = prev2$dev, coach_new = cov$coach_new,
    b_prev_tier = as.integer(isTRUE(cfg$b_prev_tier)), use_ret = as.integer(isTRUE(cfg$use_ret)),
    use_tal = as.integer(isTRUE(cfg$use_tal)), use_prev2 = as.integer(isTRUE(cfg$use_prev2)),
    use_coach = as.integer(isTRUE(cfg$use_coach)),
    compute_ll = 0L
  )

  list(
    season = season_data$season, week = as.integer(week),
    week_label = if (week == 0) "Preseason" else unique(games$week_label[games$week_seq == week])[1],
    last_regular_week = max(games$week_seq[games$season_type == "regular"]),
    teams = teams, played = played, future = future,
    has_prior = !is.null(carry) && nrow(carry) > 0,
    stan_data = apply_priors(list(stan_data = stan_data), cfg)$stan_data
  )
}

#' Stan's code for a margin family name
margin_family_code <- function(family) {
  codes <- c(normal = 1L, t = 2L, mixture = 3L, scale_t = 4L)
  if (!family %in% names(codes)) stop("Unknown margin family: ", family)
  codes[[family]]
}

#' Structural parameters of a fit, given its switches (zero-size ones are absent from the output)
model_vars <- function(stan_data) {
  c("hfa", "sigma_m", "lambda", "sigma_eff", "kappa", "b_prev", "mu_tier[2]", "mu_tier[3]",
    "sigma_team[1]", "sigma_team[2]", "sigma_team[3]", "sigma_conf",
    if (stan_data$margin_family %in% c(2L, 4L)) "nu",
    if (stan_data$margin_family == 3L) c("sigma_w", "p_wide"),
    if (stan_data$margin_family == 4L) "gamma_m",
    if (stan_data$dynamic == 1L) "tau_r",
    if (stan_data$use_total == 1L) c("base_raw[1]", "t_tier_raw[1]", "t_tier_raw[2]", "sigma_conf_t_raw[1]",
                                     "sigma_t_team_raw[1]", "b_prev_t_raw[1]", "lambda_t_raw[1]",
                                     "alpha_es_raw[1]", "sigma_es_raw[1]", "kappa_t_raw[1]", "sigma_tot_raw[1]"),
    if (stan_data$use_total == 1L && stan_data$dynamic == 1L) "tau_t_raw[1]",
    if (isTRUE(stan_data$b_prev_tier == 1L)) "b_prev_low_raw[1]",
    if (isTRUE(stan_data$use_ret == 1L)) c("b_ret_raw[1]", "c_ret_raw[1]"),
    if (isTRUE(stan_data$use_tal == 1L)) "b_tal_raw[1]",
    if (isTRUE(stan_data$use_prev2 == 1L)) "b_prev2_raw[1]",
    if (isTRUE(stan_data$use_coach == 1L)) "delta_coach_raw[1]")
}

#' Summaries of sampler health for the gates
sampler_health <- function(fit, cfg = model_config(), stan_data = NULL) {
  vars <- c("r", if (!is.null(stan_data)) model_vars(stan_data) else
    c("hfa", "sigma_m", "nu", "lambda", "sigma_eff", "kappa", "b_prev", "mu_tier", "sigma_team", "sigma_conf"),
    if (!is.null(stan_data) && stan_data$use_total == 1L) "t")
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
  run <- function(warmup, adapt_delta, sampling, seed) {
    mod$sample(
      data = data, chains = cfg$chains, parallel_chains = cfg$chains,
      iter_warmup = warmup, iter_sampling = sampling, adapt_delta = adapt_delta,
      seed = seed, refresh = 0, show_messages = FALSE, show_exceptions = FALSE
    )
  }
  t0 <- Sys.time()
  fit <- run(cfg$iter_warmup, cfg$adapt_delta, cfg$iter_sampling, seed)
  health <- sampler_health(fit, cfg, data)
  attempts <- 1L
  if (!health$passed) {
    # Longer warmup, smaller steps, and twice the draws (low-ESS misses are usually just short chains).
    # A new seed: the same one replays the same initial values, and a chain whose adaptation
    # collapsed there (step size ~1e-4, every draw at max treedepth) collapses again
    fit <- run(cfg$retry_warmup, cfg$retry_adapt_delta, cfg$retry_sampling, seed + 1L)
    health <- sampler_health(fit, cfg, data)
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

#' Scalar parameters every downstream function may use, under fixed names. Parameters a fit does
#' not have get their neutral value (no drift, no mixture, constant scale, no total model).
par_names <- c("hfa", "sigma_m", "sigma_marg", "sigma_w", "sigma_marg_w", "p_wide", "gamma_m", "nu",
               "lambda", "sigma_eff", "kappa", "b_prev", "sigma_conf", "mu_tier[2]", "mu_tier[3]",
               "sigma_team[1]", "sigma_team[2]", "sigma_team[3]", "tau_r",
               "base_raw[1]", "tau_t_raw[1]", "sigma_tot_raw[1]", "sigma_es_raw[1]", "kappa_t_raw[1]",
               "lambda_t_raw[1]", "alpha_es_raw[1]", "b_prev_t_raw[1]", "sigma_t_team_raw[1]",
               "sigma_conf_t_raw[1]", "t_tier_raw[1]", "t_tier_raw[2]",
               "b_prev_low_raw[1]", "b_ret_raw[1]", "c_ret_raw[1]", "b_tal_raw[1]", "b_prev2_raw[1]",
               "delta_coach_raw[1]")

#' Posterior draws needed downstream, as plain matrices/vectors (so the CmdStan CSVs can be deleted).
#' r (and t) are the ratings at the snapshot week; states = TRUE adds the weekly states the
#' games were played at (r_state, t_state) for checks that need each game's expected margin.
extract_draws <- function(fit, compute_ll = FALSE, states = FALSE, family = NULL) {
  get <- function(v) unclass(posterior::as_draws_matrix(fit$draws(variables = v)))
  have <- fit$metadata()$stan_variable_sizes
  present <- function(v) {
    base <- sub("\\[.*", "", v)
    size <- have[[base]]
    !is.null(size) && prod(size) > 0
  }
  par <- as.data.frame(get(Filter(present, par_names)))
  fill <- list(sigma_w = par$sigma_m, sigma_marg_w = par$sigma_marg, p_wide = 0, gamma_m = 0, nu = Inf, tau_r = 0,
               `tau_t_raw[1]` = 0)
  for (nm in names(fill)) if (!nm %in% names(par)) par[[nm]] <- fill[[nm]]
  # Friendlier names for the total model's scalars
  nm_map <- c(base = "base_raw[1]", tau_t = "tau_t_raw[1]", sigma_tot = "sigma_tot_raw[1]", sigma_es = "sigma_es_raw[1]",
              kappa_t = "kappa_t_raw[1]", lambda_t = "lambda_t_raw[1]", alpha_es = "alpha_es_raw[1]",
              b_prev_t = "b_prev_t_raw[1]", sigma_t_team = "sigma_t_team_raw[1]", sigma_conf_t = "sigma_conf_t_raw[1]",
              `t_tier[2]` = "t_tier_raw[1]", `t_tier[3]` = "t_tier_raw[2]",
              b_prev_low = "b_prev_low_raw[1]", b_ret = "b_ret_raw[1]", c_ret = "c_ret_raw[1]",
              b_tal = "b_tal_raw[1]", b_prev2 = "b_prev2_raw[1]", delta_coach = "delta_coach_raw[1]")
  for (nm in names(nm_map)) if (nm_map[[nm]] %in% names(par)) names(par)[names(par) == nm_map[[nm]]] <- nm
  if ("sigma_tot" %in% names(par)) par$sigma_tot_marg <- sqrt(par$sigma_tot^2 + par$kappa_t^2 * par$sigma_es^2)
  out <- list(r = get("r"), par = par, family = family)
  if (present("t")) out$t <- get("t")
  if (states) {
    if (present("r_state")) out$r_state <- get("r_state")
    if (present("t_state")) out$t_state <- get("t_state")
  }
  if (compute_ll) {
    out$log_lik <- get("log_lik")
    out$lprior <- as.numeric(get("lprior"))
  }
  out
}

#' Expected margin (and total, when the fit has one) of each played game at the week it was played,
#' per draw: S x N_games matrices. Needs extract_draws(states = TRUE).
game_means <- function(draws, input) {
  sd_ <- input$stan_data
  if (sd_$N_games == 0) return(list(mu = matrix(0, nrow(draws$r), 0), tt = NULL))
  S <- nrow(draws$r)
  mu <- draws$r_state[, sd_$home_state, drop = FALSE] - draws$r_state[, sd_$away_state, drop = FALSE] +
    draws$par$hfa * matrix(sd_$home_adv, S, sd_$N_games, byrow = TRUE)
  tt <- if (!is.null(draws$t_state)) draws$t_state[, sd_$home_state, drop = FALSE] + draws$t_state[, sd_$away_state, drop = FALSE]
  list(mu = mu, tt = tt)
}
