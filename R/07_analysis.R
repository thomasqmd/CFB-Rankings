# ==============================================================================
# R/07_analysis.R
# Analyses for the paper beyond the published pipeline. Each is a target (see _targets.R):
#   prior_refits     full-season refits under vague priors: the empirical basis of the priors
#   loso_backtest    weeks 0-3 refit under leave-one-season-out priors (leakage from tuning)
#   ablation         rolling backtest of model variants and recency half-lives on the same games
#   baselines        least-squares (Massey) and CFBD pregame Elo predictions of the same games
#   external_ratings published SP+ and FPI ratings (CFBD), end of season
#   final_refits     final-week refits: benchmark sensitivity, per-draw SOR, MCSE, margin-only LOO
#   margin_leakage   how much a blowout margin moves the resume
#   sbc              simulation-based calibration on a real schedule
# and cheap summaries of the published snapshots (resume validation, schedule graph).
# ==============================================================================

#' Short chains for point predictions: no gates, no retry
fast_config <- function(cfg = model_config(), chains = 2L, warmup = 400L, sampling = 400L) {
  cfg$chains <- chains; cfg$iter_warmup <- warmup; cfg$iter_sampling <- sampling
  cfg$gates <- list(rhat = Inf, ess = 0, divergences = Inf, ebfmi = 0)
  cfg
}

structural_vars <- c("hfa", "b_prev", "lambda", "kappa", "sigma_eff", "sigma_m", "nu", "sigma_conf",
                     "sigma_team[1]", "sigma_team[2]", "sigma_team[3]", "mu_tier[2]", "mu_tier[3]")

# ---- Priors ----------------------------------------------------------------------

#' Vague priors on the structural parameters, on the scale of the sport but wide enough that a
#' full season decides: the refits under them are the evidence the published priors rest on
vague_config <- function(cfg = model_config()) {
  cfg$hfa_mean <- 0; cfg$hfa_sd <- 10
  cfg$tier_mean <- c(0, 0); cfg$tier_sd <- c(100, 100)
  cfg$b_prev_mean <- 0.5; cfg$b_prev_sd <- 1
  cfg$sigma_team_mean <- c(0, 0, 0); cfg$sigma_team_sd <- c(25, 25, 25)
  cfg$sigma_conf_prior <- c(0, 25)
  cfg$lambda_prior <- c(0, 2); cfg$kappa_prior <- c(0, 2)
  cfg$sigma_eff_prior <- c(0, 50); cfg$sigma_m_prior <- c(0, 50)
  cfg
}

#' Refit each season's final snapshot under the vague priors
#' @param final_inputs list of final-week inputs, one per complete season
run_prior_refits <- function(final_inputs, cfg = model_config(), workers = 2L, seed = 2026) {
  vcfg <- vague_config(cfg)
  res <- parallel::mclapply(final_inputs, function(input) {
    f <- fit_power(apply_priors(input, vcfg), vcfg, seed = seed)
    d <- posterior::summarise_draws(f$fit$draws(variables = structural_vars), "mean", "sd")
    unlink(f$fit$output_files())
    d |> dplyr::mutate(season = input$season, n_games = nrow(input$played), passed = f$health$passed, .before = 1)
  }, mc.cores = workers, mc.preschedule = FALSE)
  dplyr::bind_rows(res)
}

#' Pool the vague-prior refits into priors: centred on the mean over seasons, sd = 2x the
#' between-season sd. 2014 is left out (no carry-over that season, so conference effects
#' absorb team strength), as is 2020 for home field (no crowds). `drop` removes more seasons
#' (leave-one-season-out).
pool_priors <- function(refits, drop = integer(0), exclude = 2014L, hfa_exclude = 2020L, sd_mult = 2) {
  refits |>
    dplyr::filter(!season %in% c(exclude, drop), !(variable == "hfa" & season %in% hfa_exclude)) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(pooled_mean = mean(mean), between_sd = stats::sd(mean), typical_post_sd = stats::median(sd),
                     min = min(mean), max = max(mean), n_seasons = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(prior_sd = sd_mult * between_sd)
}

#' cfg with its structural priors replaced by pooled ones (nu keeps its own prior)
priors_from_pool <- function(cfg, pooled) {
  m <- stats::setNames(pooled$pooled_mean, pooled$variable)
  s <- stats::setNames(pooled$prior_sd, pooled$variable)
  cfg$hfa_mean <- m[["hfa"]]; cfg$hfa_sd <- s[["hfa"]]
  cfg$tier_mean <- unname(m[c("mu_tier[2]", "mu_tier[3]")]); cfg$tier_sd <- unname(s[c("mu_tier[2]", "mu_tier[3]")])
  cfg$b_prev_mean <- m[["b_prev"]]; cfg$b_prev_sd <- s[["b_prev"]]
  st <- c("sigma_team[1]", "sigma_team[2]", "sigma_team[3]")
  cfg$sigma_team_mean <- unname(m[st]); cfg$sigma_team_sd <- unname(s[st])
  cfg$sigma_conf_prior <- c(m[["sigma_conf"]], s[["sigma_conf"]])
  cfg$lambda_prior <- c(m[["lambda"]], s[["lambda"]]); cfg$kappa_prior <- c(m[["kappa"]], s[["kappa"]])
  cfg$sigma_eff_prior <- c(m[["sigma_eff"]], s[["sigma_eff"]]); cfg$sigma_m_prior <- c(m[["sigma_m"]], s[["sigma_m"]])
  cfg
}

#' Early-week predictions under leave-one-season-out priors: season s's weeks 0-3 are refit
#' with priors pooled from the other seasons only, and predict the following week
#' @param season_inputs list (named by season) of each season's weekly inputs
run_loso_backtest <- function(season_inputs, refits, cfg = model_config(), max_week = 3L, workers = 2L, seed = 2026) {
  jobs <- do.call(rbind, lapply(names(season_inputs), function(s) {
    wk <- vapply(season_inputs[[s]], `[[`, integer(1), "week")
    data.frame(season = s, i = which(wk <= max_week))
  }))
  res <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
    s <- jobs$season[j]
    input <- season_inputs[[s]][[jobs$i[j]]]
    nxt <- dplyr::filter(input$future, week_seq == input$week + 1L)
    if (nrow(nxt) == 0) return(NULL)
    lcfg <- priors_from_pool(cfg, pool_priors(refits, drop = as.integer(s)))
    f <- fit_power(apply_priors(input, lcfg), lcfg, seed = seed)
    draws <- extract_draws(f$fit); unlink(f$fit$output_files())
    predict_games(draws, nxt) |>
      dplyr::mutate(season = as.integer(s), week = input$week, sigma = mean(draws$par$sigma_marg),
                    nu = mean(draws$par$nu), .before = 1)
  }, mc.cores = workers, mc.preschedule = FALSE)
  dplyr::bind_rows(res)
}

# ---- Ablations and baselines --------------------------------------------------------

ablation_variants <- function() {
  list(
    "Published model" = list(),
    "Margin only" = list(use_eff = FALSE),
    "Normal margin" = list(student_t = FALSE),
    "No carry-over" = list(carry_over = FALSE),
    "No conference level" = list(conf_level = FALSE),
    "Half-life 10 weeks" = list(half_life_weeks = 10),
    "Half-life 6 weeks" = list(half_life_weeks = 6),
    "Half-life 4 weeks" = list(half_life_weeks = 4)
  )
}

#' Rolling backtest of each variant: fit through week w with short chains, predict week w + 1.
#' Every variant predicts exactly the same games.
#' @param season_data,priors,weeks lists named by season (priors = final_prior of season - 1)
run_ablation <- function(season_data, priors, weeks, cfg = model_config(), workers = 4L, seed = 2026) {
  variants <- ablation_variants()
  tasks <- do.call(rbind, lapply(names(season_data), function(s) {
    w <- weeks[[s]]
    expand.grid(variant = names(variants), season = s, week = setdiff(w, max(w)), stringsAsFactors = FALSE)
  }))
  res <- parallel::mclapply(seq_len(nrow(tasks)), function(j) {
    tk <- tasks[j, ]
    vcfg <- utils::modifyList(fast_config(cfg), variants[[tk$variant]])
    input <- prepare_week_input(season_data[[tk$season]], tk$week, priors[[tk$season]], vcfg)
    nxt <- dplyr::filter(input$future, week_seq == tk$week + 1L)
    if (nrow(nxt) == 0) return(NULL)
    f <- suppressWarnings(fit_power(input, vcfg, seed = seed))
    draws <- extract_draws(f$fit); unlink(f$fit$output_files())
    predict_games(draws, nxt) |>
      dplyr::mutate(variant = tk$variant, season = as.integer(tk$season), week = tk$week,
                    sigma = mean(draws$par$sigma_marg), nu = mean(draws$par$nu), .before = 1)
  }, mc.cores = workers, mc.preschedule = FALSE)
  dplyr::bind_rows(res)
}

#' Pregame Elo ratings published by CFBD, read from the cached /games responses
cfbd_pregame_elo <- function(seasons, raw_dir = "data/raw") {
  dplyr::bind_rows(lapply(seasons, function(s) {
    files <- file.path(raw_dir, sprintf(c("games_fbs_%d.json", "games_fcs_%d.json"), s))
    raw <- unlist(lapply(files[file.exists(files)], jsonlite::read_json, simplifyVector = FALSE), recursive = FALSE)
    tibble::tibble(
      season = as.integer(s),
      game_id = vapply(raw, \(g) as.character(g$id), ""),
      home_elo = vapply(raw, \(g) num(g$homePregameElo), 0),
      away_elo = vapply(raw, \(g) num(g$awayPregameElo), 0)
    )
  })) |> dplyr::distinct(season, game_id, .keep_all = TRUE)
}

#' Ridge least-squares ratings (Massey) on the games through a week: margin = r_h - r_a +
#' hfa + tier offsets. Team ratings get a weak ridge penalty (a N(0, 16^2)-like prior at a
#' 16-point noise); home field and the tier offsets are unpenalised.
massey_fit <- function(played, teams, lambda = 1) {
  n <- nrow(teams)
  X <- matrix(0, nrow(played), n + 3)
  X[cbind(seq_len(nrow(played)), played$home)] <- 1
  X[cbind(seq_len(nrow(played)), played$away)] <- -1
  X[, n + 1] <- played$home_adv
  th <- teams$tier[played$home]; ta <- teams$tier[played$away]
  X[, n + 2] <- (th == 2L) - (ta == 2L)
  X[, n + 3] <- (th == 3L) - (ta == 3L)
  P <- diag(c(rep(lambda, n), rep(1e-3, 3)))
  beta <- solve(crossprod(X) + P, crossprod(X, played$margin))
  list(r = beta[seq_len(n)] + c(0, beta[n + 2], beta[n + 3])[teams$tier], hfa = beta[n + 1])
}

#' Least-squares and Elo predictions of every game from week 2 on, each from the games before
#' it. Both map to win probabilities with scales fitted on earlier seasons only, so the first
#' season is used for fitting and not scored.
run_baselines <- function(season_data, weeks, raw_dir = "data/raw") {
  seasons <- as.integer(names(season_data))
  massey <- dplyr::bind_rows(lapply(names(season_data), function(s) {
    games <- season_data[[s]]$games
    teams <- season_teams(games)
    obs <- game_observations(games) |>
      dplyr::mutate(home = match(home_id, teams$team_id), away = match(away_id, teams$team_id))
    done <- dplyr::filter(obs, completed, !is.na(home_points), !is.na(away_points))
    w_all <- setdiff(weeks[[s]], c(0L, max(weeks[[s]])))
    dplyr::bind_rows(lapply(w_all, function(w) {
      fit <- massey_fit(dplyr::filter(done, week_seq <= w), teams)
      nxt <- dplyr::filter(done, week_seq == w + 1L)
      tibble::tibble(season = as.integer(s), game_id = nxt$game_id, week_seq = nxt$week_seq,
                     pred_margin = fit$r[nxt$home] - fit$r[nxt$away] + fit$hfa * nxt$home_adv,
                     margin = nxt$home_points - nxt$away_points,
                     home_win = as.integer(nxt$home_points > nxt$away_points))
    }))
  }))
  # Win probability from the least-squares spread: normal with the RMSE of earlier seasons
  massey <- dplyr::bind_rows(lapply(seasons[-1], function(s) {
    past <- dplyr::filter(massey, season < s)
    sig <- sqrt(mean((past$pred_margin - past$margin)^2))
    dplyr::filter(massey, season == s) |> dplyr::mutate(pred_home_wp = stats::pnorm(pred_margin / sig))
  })) |> dplyr::mutate(method = "Least squares (Massey)")

  elo <- cfbd_pregame_elo(seasons, raw_dir) |>
    dplyr::inner_join(dplyr::bind_rows(lapply(season_data, `[[`, "games")) |>
                        dplyr::filter(completed, !is.na(home_points)) |>
                        dplyr::transmute(season, game_id, week_seq, home_adv = ifelse(neutral_site, 0, 1),
                                         margin = home_points - away_points,
                                         home_win = as.integer(home_points > away_points)),
                      by = c("season", "game_id")) |>
    dplyr::filter(!is.na(home_elo), !is.na(away_elo)) |>
    dplyr::mutate(diff = home_elo - away_elo)
  elo <- dplyr::bind_rows(lapply(seasons[-1], function(s) {
    past <- dplyr::filter(elo, season < s)
    lm_fit <- stats::lm(margin ~ 0 + diff + home_adv, data = past)
    glm_fit <- stats::glm(home_win ~ 0 + diff + home_adv, family = stats::binomial(), data = past)
    cur <- dplyr::filter(elo, season == s, week_seq >= 2L)
    cur |> dplyr::mutate(pred_margin = stats::predict(lm_fit, cur),
                         pred_home_wp = stats::predict(glm_fit, cur, type = "response"))
  })) |> dplyr::mutate(method = "CFBD Elo") |>
    dplyr::select(season, game_id, week_seq, pred_margin, margin, home_win, pred_home_wp, method)
  dplyr::bind_rows(massey, elo)
}

#' Published end-of-season SP+ and FPI ratings from CFBD (one cached call per season and system)
run_external_ratings <- function(seasons, teams_meta, raw_dir = "data/raw") {
  get <- function(name, path, s) cfbd_cached(name, s, path, list(year = s), raw_dir = raw_dir)$data
  out <- dplyr::bind_rows(lapply(seasons, function(s) {
    sp <- get("ratings_sp", "ratings/sp", s)
    fpi <- get("ratings_fpi", "ratings/fpi", s)
    dplyr::bind_rows(
      tibble::tibble(season = as.integer(s), system = "SP+",
                     team = vapply(sp, \(x) chr(x$team), ""), rating = vapply(sp, \(x) num(x$rating), 0)),
      tibble::tibble(season = as.integer(s), system = "FPI",
                     team = vapply(fpi, \(x) chr(x$team), ""), rating = vapply(fpi, \(x) num(x$fpi), 0))
    )
  }))
  out |>
    dplyr::filter(!is.na(rating), team != "nationalAverages") |>
    dplyr::mutate(team_id = teams_meta$team_id[match(team, teams_meta$school)])
}

# ---- Final-week refits ----------------------------------------------------------------

#' WAB per draw (S x teams) against the b-th best FBS team; r must be FBS-centred
wab_draws <- function(r, par, teams, played, b = 12L) {
  tg <- team_game_rows(played)
  S <- nrow(r)
  fbs_cols <- which(teams$tier == 1L)
  bench <- apply(r[, fbs_cols, drop = FALSE], 1, function(x) sort(x, decreasing = TRUE)[b])
  venue <- matrix(tg$venue, S, nrow(tg), byrow = TRUE)
  p_bench <- win_prob(bench + par$hfa * venue - r[, tg$opp, drop = FALSE], par$sigma_marg, par$nu)
  m <- matrix(tg$win, S, nrow(tg), byrow = TRUE) - p_bench
  out <- matrix(0, S, nrow(teams))
  agg <- t(rowsum(t(m), tg$team))
  out[, as.integer(colnames(agg))] <- agg
  list(wab = out, p_bench = p_bench, tg = tg)
}

#' Refit each complete season's final week twice (published model; margin-only model), with
#' every draw kept, and compute what the published summaries cannot show
run_final_refits <- function(final_inputs, cfg = model_config(), workers = 2L, seed = 2026) {
  jobs <- expand.grid(i = seq_along(final_inputs), model = c("full", "margin"), stringsAsFactors = FALSE)
  fits <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
    input <- final_inputs[[jobs$i[j]]]
    if (jobs$model[j] == "margin") {
      input$stan_data$has_eff[] <- 0L
      input$stan_data$eff[] <- 0
    }
    f <- fit_power(input, cfg, compute_ll = TRUE, seed = seed)
    ll <- f$fit$draws("log_lik_margin", format = "draws_array")
    loo_m <- loo::loo(ll, r_eff = loo::relative_eff(exp(ll)))
    out <- list(season = input$season, model = jobs$model[j], loo = loo_m, health = f$health)
    if (jobs$model[j] == "full") out <- c(out, final_refit_summaries(f$fit, input))
    unlink(f$fit$output_files())
    out
  }, mc.cores = workers, mc.preschedule = FALSE)
  seasons <- vapply(fits, `[[`, integer(1), "season")
  lapply(stats::setNames(unique(seasons), unique(seasons)), function(s) {
    full <- fits[[which(seasons == s & vapply(fits, `[[`, "", "model") == "full")]]
    marg <- fits[[which(seasons == s & vapply(fits, `[[`, "", "model") == "margin")]]
    cmp <- loo::loo_compare(list(full = full$loo, margin = marg$loo))
    # loo >= 2.10 returns the model names in a column; earlier versions in the row names
    nm <- if ("model" %in% colnames(cmp)) as.character(cmp$model) else rownames(cmp)
    full$loo_compare <- tibble::tibble(
      model = nm, elpd_diff = cmp[, "elpd_diff"], se_diff = cmp[, "se_diff"],
      elpd_loo = c(full = full$loo$estimates["elpd_loo", "Estimate"], margin = marg$loo$estimates["elpd_loo", "Estimate"])[nm])
    full$loo <- NULL
    full$margin_health <- marg$health
    full
  })
}

#' Benchmark sensitivity, per-draw SOR and Monte Carlo errors from one full-draw fit
final_refit_summaries <- function(fit, input) {
  teams <- input$teams
  fbs <- teams$tier == 1L
  arr_r <- fit$draws("r", format = "draws_array")
  n_iter <- posterior::niterations(arr_r); n_ch <- posterior::nchains(arr_r)
  r <- unclass(posterior::as_draws_matrix(arr_r))
  r <- r - rowMeans(r[, fbs, drop = FALSE])
  par <- as.data.frame(unclass(posterior::as_draws_matrix(fit$draws(c("hfa", "sigma_marg", "nu")))))

  # Benchmark rank b = 4, 12, 25
  bench <- dplyr::bind_rows(lapply(c(4L, 12L, 25L), function(b) {
    w <- wab_draws(r, par, teams, input$played, b)$wab[, fbs, drop = FALSE]
    rk <- rank_draws(w)
    tibble::tibble(team_id = teams$team_id[fbs], team = teams$team[fbs], b = b, wab = colMeans(w),
                   rank = rank(-colMeans(w), ties.method = "first"), p_top12 = colMeans(rk <= 12))
  }))

  # SOR from the posterior-mean benchmark probabilities (published) and averaged per draw
  wd <- wab_draws(r, par, teams, input$played, 12L)
  tg <- wd$tg
  thin <- round(seq(1, nrow(r), length.out = 400))
  sor <- dplyr::bind_rows(lapply(which(fbs), function(i) {
    rows <- which(tg$team == i)
    if (!length(rows)) return(NULL)
    wins <- sum(tg$win[rows])
    pb <- wd$p_bench[, rows, drop = FALSE]
    tibble::tibble(team_id = teams$team_id[i],
                   sor_mean_pi = prob_at_least(colMeans(pb), wins),
                   sor_per_draw = mean(apply(pb[thin, , drop = FALSE], 1, prob_at_least, k = wins)),
                   sor_strict = prob_at_least(colMeans(pb), wins + 1L))
  }))

  # Monte Carlo standard errors of the published resume quantities, all draws vs. the
  # 1,000 equally spaced draws the pipeline keeps (every 4th iteration of each chain)
  w12 <- wd$wab[, fbs, drop = FALSE]
  rk12 <- rank_draws(w12)
  as_arr <- function(m, keep) {
    a <- array(m, c(n_iter, n_ch, ncol(m)))
    posterior::as_draws_array(a[keep, , , drop = FALSE])
  }
  mcse_tab <- dplyr::bind_rows(lapply(list(all = seq_len(n_iter), thinned = seq(1, n_iter, by = 4)), function(keep) {
    a_w <- as_arr(w12, keep); a_top <- as_arr((rk12 <= 12) * 1, keep); a_rk <- as_arr(rk12, keep)
    tibble::tibble(
      draws = length(keep) * n_ch,
      wab = posterior::summarise_draws(a_w, "mcse_mean")$mcse_mean,
      p_top12 = posterior::summarise_draws(a_top, "mcse_mean")$mcse_mean,
      rank_q10 = posterior::summarise_draws(a_rk, \(x) posterior::mcse_quantile(x, 0.1))[[2]],
      rank_q90 = posterior::summarise_draws(a_rk, \(x) posterior::mcse_quantile(x, 0.9))[[2]],
      team_id = teams$team_id[fbs]
    )
  }), .id = "set")
  list(bench = bench, sor = sor, mcse = mcse_tab)
}

#' Refit with one blowout's margin replaced by a one-point result (same winner, efficiency
#' dropped) and record how the two teams' WAB and power move
run_margin_leakage <- function(input, cfg = model_config(), n_games = 6L, workers = 2L, seed = 2026) {
  tm <- input$teams
  pl <- input$played
  fbs_g <- which(tm$tier[pl$home] == 1L & tm$tier[pl$away] == 1L)
  picks <- fbs_g[order(-abs(pl$margin[fbs_g]))][seq_len(n_games)]
  runs <- c(list(NULL), as.list(picks))
  res <- parallel::mclapply(runs, function(k) {
    inp <- input
    if (!is.null(k)) {
      inp$stan_data$margin[k] <- sign(pl$margin[k])
      inp$stan_data$has_eff[k] <- 0L
      inp$stan_data$eff[k] <- 0
    }
    f <- fit_power(inp, cfg, seed = seed)
    draws <- extract_draws(f$fit); unlink(f$fit$output_files())
    fbs <- tm$tier == 1L
    r <- draws$r - rowMeans(draws$r[, fbs, drop = FALSE])
    w <- wab_draws(r, draws$par, tm, pl, 12L)$wab
    tibble::tibble(changed = if (is.null(k)) NA_integer_ else k, idx = tm$idx, wab = colMeans(w), power = colMeans(r))
  }, mc.cores = workers, mc.preschedule = FALSE)
  base <- res[[1]]
  dplyr::bind_rows(lapply(res[-1], function(x) {
    k <- x$changed[1]
    win_home <- pl$margin[k] > 0
    ids <- c(winner = if (win_home) pl$home[k] else pl$away[k], loser = if (win_home) pl$away[k] else pl$home[k])
    tibble::tibble(game = k, week = pl$week_seq[k], margin = abs(pl$margin[k]), role = names(ids),
                   team = tm$team[ids], wab_base = base$wab[ids], wab_new = x$wab[ids],
                   power_base = base$power[ids], power_new = x$power[ids],
                   # average |change| over every other FBS team (how far the effect spreads)
                   others_wab = mean(abs(x$wab - base$wab)[tm$tier == 1L & !tm$idx %in% ids]))
  }))
}

# ---- Simulation-based calibration ----------------------------------------------------

#' Simulate a season from the priors on a real schedule, refit, and rank the true values
#' among the posterior draws. Uniform ranks (and nominal coverage) mean the fit recovers
#' what generated the data.
run_sbc <- function(input, cfg = model_config(), n_sims = 100L, workers = 2L, n_rank_draws = 99L) {
  tm <- input$teams
  pl <- input$played
  fbs <- tm$tier == 1L
  scfg <- cfg
  scfg$chains <- 4L; scfg$iter_warmup <- 400L; scfg$iter_sampling <- 250L
  scfg$gates <- list(rhat = Inf, ess = 0, divergences = Inf, ebfmi = 0)
  res <- parallel::mclapply(seq_len(n_sims), function(i) {
    set.seed(1000 + i)
    p <- draw_prior_params(input, cfg)
    mu <- p$r[pl$home] - p$r[pl$away] + p$hfa * pl$home_adv
    he <- input$stan_data$has_eff == 1L
    e <- ifelse(he, p$lambda * mu + stats::rnorm(nrow(pl), 0, p$sigma_eff), 0)
    t_draw <- stats::rt(nrow(pl), p$nu)
    y <- ifelse(he, mu + p$kappa * (e - p$lambda * mu) + t_draw * p$sigma_m, mu + t_draw * p$sigma_marg)
    sim <- input
    sim$stan_data$margin <- y
    sim$stan_data$eff <- e
    sim$played$margin <- y
    sim$played$home_points <- pmax(y, 0); sim$played$away_points <- pmax(-y, 0)
    f <- suppressWarnings(fit_power(sim, scfg, seed = i))
    draws <- extract_draws(f$fit)
    unlink(f$fit$output_files())

    # True quantities (FBS-centred), in the same form as the published ones
    r0 <- p$r - mean(p$r[fbs])
    true_par <- data.frame(hfa = p$hfa, sigma_marg = p$sigma_marg, nu = p$nu)
    w0 <- wab_draws(matrix(r0, 1), true_par, tm, sim$played, 12L)$wab[1, fbs]
    r <- draws$r - rowMeans(draws$r[, fbs, drop = FALSE])
    w <- wab_draws(r, draws$par, tm, sim$played, 12L)$wab[, fbs, drop = FALSE]
    rk_r <- rank_draws(r[, fbs, drop = FALSE]); rk_w <- rank_draws(w)
    rk_r0 <- rank(-r0[fbs], ties.method = "min"); rk_w0 <- rank(-w0, ties.method = "min")
    keep <- round(seq(1, nrow(r), length.out = n_rank_draws))
    # rank statistic with random tie-breaking (ranks are discrete)
    rstat <- function(post, truth) {
      vapply(seq_along(truth), function(j) {
        x <- post[keep, j]
        sum(x < truth[j]) + sum(stats::runif(sum(x == truth[j])) < 0.5)
      }, numeric(1))
    }
    cover <- function(post, truth) {
      lo <- apply(post, 2, stats::quantile, 0.1, names = FALSE)
      hi <- apply(post, 2, stats::quantile, 0.9, names = FALSE)
      truth >= lo & truth <= hi
    }
    scal <- c("hfa", "b_prev", "sigma_m", "lambda", "kappa", "sigma_conf", "nu")
    scal_post <- as.matrix(draws$par[, scal])
    scal_true <- unlist(p[scal])
    dplyr::bind_rows(
      tibble::tibble(sim = i, quantity = scal, team = NA_integer_, rank = rstat(scal_post, scal_true),
                     covered = cover(scal_post, scal_true)),
      tibble::tibble(sim = i, quantity = "Power rating", team = which(fbs), rank = rstat(r[, fbs, drop = FALSE], r0[fbs]),
                     covered = cover(r[, fbs, drop = FALSE], r0[fbs])),
      tibble::tibble(sim = i, quantity = "WAB", team = which(fbs), rank = rstat(w, w0), covered = cover(w, w0)),
      tibble::tibble(sim = i, quantity = "Power rank", team = which(fbs), rank = rstat(rk_r, rk_r0),
                     covered = cover(rk_r, rk_r0)),
      tibble::tibble(sim = i, quantity = "Resume rank", team = which(fbs), rank = rstat(rk_w, rk_w0),
                     covered = cover(rk_w, rk_w0))
    ) |> dplyr::mutate(passed = f$health$passed)
  }, mc.cores = workers, mc.preschedule = FALSE)
  list(season = input$season, week = input$week, n_games = nrow(pl), n_rank_draws = n_rank_draws,
       ranks = dplyr::bind_rows(res))
}

# ---- Summaries of the published snapshots ---------------------------------------------

#' Connected components of the schedule graph at every snapshot, among teams that have played
schedule_components <- function(games_all, params) {
  dplyr::bind_rows(lapply(split(params, params$season), function(ps) {
    s <- ps$season[1]
    g <- dplyr::filter(games_all, season == s, completed, !is.na(home_points))
    dplyr::bind_rows(lapply(ps$week, function(w) {
      gw <- dplyr::filter(g, week_seq <= w)
      if (nrow(gw) == 0) return(tibble::tibble(season = s, week = w, teams = 0L, components = NA_integer_, fbs_components = NA_integer_))
      gr <- igraph::graph_from_data_frame(data.frame(from = gw$home_id, to = gw$away_id), directed = FALSE)
      comp <- igraph::components(gr)
      fbs_ids <- unique(c(gw$home_id[gw$home_class %in% "fbs"], gw$away_id[gw$away_class %in% "fbs"]))
      tibble::tibble(season = s, week = w, teams = igraph::vcount(gr), components = comp$no,
                     fbs_components = length(unique(comp$membership[names(comp$membership) %in% as.character(fbs_ids)])))
    }))
  }))
}

#' The resume against the committee: the snapshot the committee's final ranking follows,
#' against that ranking and the playoff field,
#' plus retrodictive accuracy of each ranking on the season's FBS games
resume_validation <- function(ratings, games_all, last_full) {
  systems <- c(Resume = "resume_rank", SOR = "sor_rank", Power = "power_rank", Colley = "colley_rank", AP = "ap_rank")
  rt <- ratings |>
    dplyr::filter(tier == 1, season <= last_full) |>
    dplyr::group_by(season, week) |>
    dplyr::mutate(sor_rank = ifelse(games > 0, rank(sor_prob, ties.method = "first", na.last = "keep"), NA_integer_)) |>
    dplyr::ungroup()
  out <- lapply(sort(unique(rt$season)), function(s) {
    g <- dplyr::filter(games_all, season == s)
    # The committee's final ranking follows the conference championships, which is the last
    # snapshot with a CFP ranking attached (Army-Navy, a week later, has none)
    cfp_weeks <- unique(rt$week[rt$season == s & !is.na(rt$cfp_rank)])
    if (length(cfp_weeks) == 0) return(NULL)
    pre <- dplyr::filter(rt, season == s, week == max(cfp_weeks))
    playoff <- dplyr::filter(g, season_type == "postseason", grepl("College Football Playoff", notes))
    field <- if (s >= 2024) unique(c(playoff$home_id, playoff$away_id)) else pre$team_id[which(pre$cfp_rank <= 4)]
    n_field <- length(field)
    cmp <- dplyr::bind_rows(lapply(names(systems), function(nm) {
      rk <- pre[[systems[[nm]]]]
      top <- \(k) pre$team_id[which(rk <= k)]
      tibble::tibble(season = s, system = nm, n_field = n_field,
                     field_overlap = length(intersect(top(n_field), field)),
                     top4_overlap = length(intersect(top(4), pre$team_id[which(pre$cfp_rank <= 4)])),
                     top12_overlap = length(intersect(top(12), pre$team_id[which(pre$cfp_rank <= 12)])),
                     rho_cfp25 = suppressWarnings(stats::cor(rk, pre$cfp_rank, method = "spearman", use = "complete.obs")))
    }))
    # Retrodictive accuracy at the final snapshot, FBS vs FBS games of the whole season
    fin <- dplyr::filter(rt, season == s)
    fin <- dplyr::filter(fin, week == max(week))
    fg <- dplyr::filter(g, completed, home_class %in% "fbs", away_class %in% "fbs", home_points != away_points)
    retro <- dplyr::bind_rows(lapply(setdiff(names(systems), "AP"), function(nm) {
      rk <- fin[[systems[[nm]]]][match(c(fg$home_id, fg$away_id), fin$team_id)]
      rh <- rk[seq_len(nrow(fg))]; ra <- rk[-seq_len(nrow(fg))]
      ok <- !is.na(rh) & !is.na(ra)
      tibble::tibble(season = s, system = nm, games = sum(ok),
                     retro_acc = mean(((rh < ra) == (fg$home_points > fg$away_points))[ok]))
    }))
    list(committee = cmp, retro = retro)
  })
  out <- Filter(Negate(is.null), out)
  list(committee = dplyr::bind_rows(lapply(out, `[[`, "committee")),
       retro = dplyr::bind_rows(lapply(out, `[[`, "retro")))
}

#' The CFBD data behind a build: per season, the targets hash of the data target and the
#' row counts, for the paper's reproducibility appendix
data_fingerprint <- function(season_data) {
  dplyr::bind_rows(lapply(season_data, function(d) {
    tibble::tibble(season = d$season, games = nrow(d$games), completed = sum(d$games$completed),
                   with_eff = sum(!is.na(d$games$home_ppa) & !is.na(d$games$away_ppa)),
                   with_line = sum(!is.na(d$games$spread)), poll_rows = nrow(d$polls %||% data.frame()),
                   hash = digest::digest(d, algo = "xxhash64"))
  })) |> dplyr::mutate(built = Sys.time())
}

#' Score game predictions (pred_margin, pred_home_wp, and optionally sigma / nu /
#' pred_margin_sd for the predictive distribution) against results, one row per game
score_predictions <- function(preds, games_all) {
  res <- games_all |>
    dplyr::filter(completed, !is.na(home_points), !is.na(away_points)) |>
    dplyr::transmute(season, game_id, week_seq, margin = home_points - away_points,
                     home_win = as.integer(home_points > away_points), vegas_margin = -spread,
                     fbs_game = home_class %in% "fbs" & away_class %in% "fbs")
  out <- preds |>
    dplyr::select(-dplyr::any_of(c("margin", "home_win", "week_seq"))) |>
    dplyr::inner_join(res, by = c("season", "game_id")) |>
    dplyr::mutate(
      err = pred_margin - margin,
      brier = (pred_home_wp - home_win)^2,
      logloss = -(home_win * log(pmax(pred_home_wp, 1e-6)) + (1 - home_win) * log(pmax(1 - pred_home_wp, 1e-6)))
    )
  if (all(c("sigma", "nu") %in% names(out))) {
    out <- out |> dplyr::mutate(crps = crps_t(margin, pred_margin, sqrt(sigma^2 + dplyr::coalesce(pred_margin_sd, 0)^2), nu))
  }
  out
}
