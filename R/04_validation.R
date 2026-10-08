# ==============================================================================
# R/04_validation.R
# Snapshot runner, model checks (PPC, prior sensitivity, LOO), and the rolling
# out-of-sample backtest against results and closing lines.
# ==============================================================================

#' Fit + summarise one snapshot. Returns only summaries; CmdStan CSVs are deleted.
run_snapshot <- function(input, cfg = model_config()) {
  f <- fit_power(input, cfg)
  draws <- extract_draws(f$fit, family = cfg$margin_family)
  unlink(f$fit$output_files())
  summarise_snapshot(input, draws, f$health, cfg)
}

#' Posterior predictive check on the fitted games: observed statistic vs. 90% predictive interval.
#' Each game's expected margin is taken at the week it was played (draws need states = TRUE).
posterior_predictive_check <- function(draws, input, n_draws = 400, seed = 1) {
  set.seed(seed)
  played <- input$played
  family <- draws$family %||% "t"
  gm <- game_means(draws, input)
  S <- nrow(draws$r)
  pick <- sample.int(S, min(n_draws, S))
  mu <- gm$mu[pick, , drop = FALSE]
  par <- draws$par[pick, ]
  # Scores are integers: round the replicates so boundary statistics (|margin| <= 7, >= 28)
  # compare like with like (a continuous replicate never lands exactly on 7 or 28)
  rep <- round(margin_rng(mu, par, family))
  stat_fns <- list(
    "SD of margin" = function(m) stats::sd(m),
    "Share within 7 pts" = function(m) mean(abs(m) <= 7),
    "Share within 3 pts" = function(m) mean(abs(m) <= 3),
    "Share decided by 28+" = function(m) mean(abs(m) >= 28),
    "Home win rate (true home games)" = function(m) mean(m[played$home_adv == 1] > 0)
  )
  one <- function(nm, fn, reps_mat, obs) {
    reps <- apply(reps_mat, 1, fn)
    tibble::tibble(statistic = nm, observed = fn(obs),
                   rep_lo = stats::quantile(reps, 0.05, names = FALSE),
                   rep_hi = stats::quantile(reps, 0.95, names = FALSE),
                   p_value = mean(reps >= fn(obs)))
  }
  out <- dplyr::bind_rows(lapply(names(stat_fns), \(nm) one(nm, stat_fns[[nm]], rep, played$margin)))
  if (!is.null(gm$tt)) {
    # Totals of games without overtime, from the total's marginal law
    ht <- played$has_total
    tot_mu <- 2 * par$base + gm$tt[pick, ht, drop = FALSE]
    trep <- round(tot_mu + matrix(stats::rnorm(length(tot_mu)), nrow(tot_mu)) * par$sigma_tot_marg)
    tot_fns <- list("Mean total" = mean, "SD of total" = stats::sd)
    out <- dplyr::bind_rows(out, dplyr::bind_rows(lapply(names(tot_fns), \(nm) one(nm, tot_fns[[nm]], trep, played$total[ht]))))
  }
  out
}

#' Full checks for a season's latest snapshot: gates (optionally strict), PPC,
#' priorsense power-scaling sensitivity, and PSIS-LOO.
run_checks <- function(inputs, cfg = model_config(), strict = FALSE) {
  if (length(inputs) == 0) return(NULL)
  input <- inputs[[length(inputs)]]
  if (nrow(input$played) == 0) return(NULL)
  f <- fit_power(input, cfg, compute_ll = TRUE, strict = strict)
  vars <- model_vars(input$stan_data)
  sens <- tryCatch({
    x <- f$fit$draws(variables = c(vars, "lprior", "log_lik"))
    tibble::as_tibble(priorsense::powerscale_sensitivity(x, variable = vars))
  }, error = function(e) tibble::tibble(variable = NA_character_, error = conditionMessage(e)))
  loo_est <- tryCatch({
    l <- loo::loo(f$fit$draws("log_lik", format = "matrix"))
    tibble::tibble(elpd_loo = l$estimates["elpd_loo", "Estimate"], se = l$estimates["elpd_loo", "SE"],
                   p_loo = l$estimates["p_loo", "Estimate"],
                   n_bad_k = sum(loo::pareto_k_values(l) > 0.7))
  }, error = function(e) tibble::tibble(elpd_loo = NA_real_))
  draws <- extract_draws(f$fit, states = TRUE, family = cfg$margin_family)
  ppc <- posterior_predictive_check(draws, input)
  unlink(f$fit$output_files())
  list(season = input$season, week = input$week, health = f$health, ppc = ppc, sensitivity = sens, loo = loo_est)
}

#' Last season's final Power ratings, used as next season's preseason information
final_prior <- function(snaps) {
  last <- snaps[[length(snaps)]]
  last$ratings |> dplyr::select(team_id, rating = power, rating_sd = power_sd,
                                dplyr::any_of(c("total_t", "total_t_sd")))
}

combine_snapshots <- function(...) {
  snaps <- unlist(list(...), recursive = FALSE)
  pick <- function(nm) dplyr::bind_rows(lapply(snaps, `[[`, nm))
  list(ratings = pick("ratings"), params = pick("params"),
       game_resume = pick("game_resume"), predictions = pick("predictions"))
}

#' Attach poll ranks (AP, CFP, Coaches, FCS Coaches) matching each snapshot's games
attach_polls <- function(ratings, season_data_list) {
  polls <- c(ap = "AP Top 25", cfp = "Playoff Committee Rankings", coaches = "Coaches Poll", fcs = "FCS Coaches Poll",
             cbs = "CBS Sports 138")
  keys <- ratings |> dplyr::distinct(season, week)
  looked_up <- dplyr::bind_rows(lapply(seq_len(nrow(keys)), function(i) {
    s <- keys$season[i]; w <- keys$week[i]
    sd <- season_data_list[[as.character(s)]]
    last_reg <- max(sd$games$week_seq[sd$games$season_type == "regular"])
    dplyr::bind_rows(lapply(names(polls), function(nm) {
      p <- poll_for_week(sd$polls, w, last_reg, polls[[nm]])
      if (is.null(p)) return(NULL)
      tibble::tibble(season = s, week = w, team_id = p$team_id, poll = paste0(nm, "_rank"), rank = p$rank)
    }))
  }))
  if (nrow(looked_up) == 0) {
    for (nm in names(polls)) ratings[[paste0(nm, "_rank")]] <- NA_integer_
    return(ratings)
  }
  wide <- looked_up |>
    dplyr::distinct(season, week, team_id, poll, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = poll, values_from = rank)
  for (nm in paste0(names(polls), "_rank")) if (!nm %in% names(wide)) wide[[nm]] <- NA_integer_
  ratings |> dplyr::left_join(wide, by = c("season", "week", "team_id"))
}

#' CRPS of a location-scale Student-t predictive distribution (closed form, nu > 1)
crps_t <- function(y, loc, scale, nu) {
  z <- (y - loc) / scale
  bfrac <- beta(0.5, nu - 0.5) / beta(0.5, nu / 2)^2
  scale * (z * (2 * stats::pt(z, nu) - 1) +
             2 * stats::dt(z, nu) * (nu + z^2) / (nu - 1) -
             2 * sqrt(nu) * bfrac / (nu - 1))
}

#' Spiegelhalter's z test of calibration for probability forecasts
spiegelhalter_z <- function(p, y) {
  sum((y - p) * (1 - 2 * p)) / sqrt(sum((1 - 2 * p)^2 * p * (1 - p)))
}

#' Out-of-sample backtest: each game is predicted by the snapshot fitted through the
#' previous week (no future information), then compared with the result and the line.
#' The predictive distribution of the margin is the snapshot's own (pred_q: quantiles of
#' posterior predictive draws, which include rating uncertainty, the week of random-walk drift
#' and the margin family). Totals are compared with the closing over/under.
run_backtest <- function(predictions, games, params = NULL) {
  played <- games |>
    dplyr::filter(completed, !is.na(home_points), !is.na(away_points)) |>
    dplyr::transmute(season, game_id, week_seq, margin = home_points - away_points,
                     home_win = as.integer(home_points > away_points),
                     vegas_margin = -spread, total = home_points + away_points, vegas_total = over_under,
                     fbs_game = home_class %in% "fbs" & away_class %in% "fbs",
                     d1_game = home_class %in% c("fbs", "fcs") & away_class %in% c("fbs", "fcs"))
  if (!"pred_total" %in% names(predictions)) predictions$pred_total <- NA_real_
  if (!"pred_q" %in% names(predictions)) predictions$pred_q <- vector("list", nrow(predictions))
  bt <- predictions |>
    dplyr::inner_join(played, by = c("season", "game_id")) |>
    dplyr::filter(week_seq == week + 1L) |>
    dplyr::mutate(
      err = pred_margin - margin,
      vegas_err = vegas_margin - margin,
      brier = (pred_home_wp - home_win)^2,
      logloss = -(home_win * log(pmax(pred_home_wp, 1e-6)) + (1 - home_win) * log(pmax(1 - pred_home_wp, 1e-6))),
      correct = (pred_home_wp > 0.5) == (home_win == 1),
      ats_pick_home = pred_margin > vegas_margin,
      ats_result = sign(margin - vegas_margin),
      ats_win = dplyr::if_else(ats_result == 0, NA, (ats_result > 0) == ats_pick_home),
      # Predictive distribution of the margin
      crps = crps_quantiles(margin, pred_q),
      cover50 = margin >= quantile_at(pred_q, 0.25) & margin <= quantile_at(pred_q, 0.75),
      cover80 = margin >= quantile_at(pred_q, 0.10) & margin <= quantile_at(pred_q, 0.90),
      cover95 = margin >= quantile_at(pred_q, 0.025) & margin <= quantile_at(pred_q, 0.975),
      # The line turned into a win probability with the model's own predictive distribution,
      # shifted to centre on the line
      line_wp = 1 - cdf_from_quantiles(pred_margin - vegas_margin, pred_q),
      brier_line = (line_wp - home_win)^2,
      # Totals (final score, overtime included, as the over/under is settled)
      total_err = pred_total - total,
      total_vegas_err = vegas_total - total
    ) |>
    dplyr::select(-pred_q)

  summarise_bt <- function(df) {
    df |> dplyr::summarise(
      games = dplyr::n(),
      brier_model_lined = mean(brier[!is.na(vegas_margin)]),
      mae = mean(abs(err)), rmse = sqrt(mean(err^2)),
      brier = mean(brier), logloss = mean(logloss), accuracy = mean(correct),
      crps = mean(crps, na.rm = TRUE),
      cover50 = mean(cover50, na.rm = TRUE), cover80 = mean(cover80, na.rm = TRUE), cover95 = mean(cover95, na.rm = TRUE),
      games_with_line = sum(!is.na(vegas_margin)),
      mae_model_lined = mean(abs(err[!is.na(vegas_margin)])),
      mae_vegas = mean(abs(vegas_err), na.rm = TRUE),
      brier_line = mean(brier_line, na.rm = TRUE),
      ats_win_rate = mean(ats_win, na.rm = TRUE),
      ats_n = sum(!is.na(ats_win)),
      sharpness = mean(abs(pred_home_wp - 0.5)),
      mae_total = mean(abs(total_err), na.rm = TRUE),
      games_with_ou = sum(!is.na(vegas_total) & !is.na(total_err)),
      mae_total_model_ou = mean(abs(total_err[!is.na(vegas_total)]), na.rm = TRUE),
      mae_total_vegas = mean(abs(total_vegas_err[!is.na(total_err)]), na.rm = TRUE),
      .groups = "drop"
    )
  }
  calib <- function(df) {
    df |>
      dplyr::mutate(bin = cut(pred_home_wp, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) |>
      dplyr::group_by(bin, .add = TRUE) |>
      dplyr::summarise(pred = mean(pred_home_wp), obs = mean(home_win), n = dplyr::n(), .groups = "drop_last")
  }
  calib_tests <- function(df) {
    df |> dplyr::summarise(
      games = dplyr::n(),
      ece = {
        b <- cut(pred_home_wp, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
        sum(tapply(pred_home_wp - home_win, b, \(x) abs(sum(x))), na.rm = TRUE) / dplyr::n()
      },
      spiegelhalter_z = spiegelhalter_z(pred_home_wp, home_win),
      .groups = "drop"
    ) |> dplyr::mutate(spiegelhalter_p = 2 * stats::pnorm(-abs(spiegelhalter_z)))
  }
  late <- function(df) dplyr::filter(df, week_seq >= 5L)
  bt_groups <- bt |> dplyr::mutate(
    matchup = ifelse(fbs_game, "FBS vs FBS", "Other"),
    phase = ifelse(week_seq <= 4L, "Weeks 1-4", "Week 5+"))

  list(
    games = bt,
    overall = dplyr::bind_rows(
      summarise_bt(bt) |> dplyr::mutate(scope = "All games", .before = 1),
      summarise_bt(dplyr::filter(bt, fbs_game)) |> dplyr::mutate(scope = "FBS vs FBS", .before = 1),
      # the predicted week (week_seq) is 5 or later: the snapshot has at least four weeks of games
      summarise_bt(late(dplyr::filter(bt, fbs_game))) |> dplyr::mutate(scope = "FBS vs FBS, week 5+", .before = 1)
    ),
    by_season = bt |> dplyr::filter(fbs_game) |> dplyr::group_by(season) |> summarise_bt(),
    by_week = bt |> dplyr::filter(fbs_game) |> dplyr::group_by(week = week_seq) |> summarise_bt(),
    calibration = calib(bt),
    calibration_by = dplyr::bind_rows(
      calib(dplyr::group_by(bt_groups, split = matchup)),
      calib(dplyr::group_by(bt_groups, split = phase))
    ),
    calibration_tests = dplyr::bind_rows(
      calib_tests(bt) |> dplyr::mutate(split = "All games", .before = 1),
      calib_tests(dplyr::group_by(bt_groups, split = matchup)),
      calib_tests(dplyr::group_by(bt_groups, split = phase))
    ),
    uncertainty = backtest_uncertainty(bt)
  )
}

#' Uncertainty of the headline comparisons. Games in the same week share information (the
#' same snapshot predicts them all), so resampling and standard errors use season-week
#' clusters rather than games.
backtest_uncertainty <- function(bt, B = 2000L, seed = 1) {
  set.seed(seed)
  lined <- dplyr::filter(bt, !is.na(vegas_margin), fbs_game)
  cl <- paste(lined$season, lined$week_seq)
  # Model minus line absolute error, a Diebold-Mariano comparison with cluster-robust SE
  d <- abs(lined$err) - abs(lined$vegas_err)
  dm <- function(d, cl) {
    m <- mean(d); u <- tapply(d - m, cl, sum)
    se <- sqrt(sum(u^2)) / length(d)
    c(mean = m, se = se, z = m / se)
  }
  dm_mae <- dm(d, cl)
  dm_brier <- dm(lined$brier - lined$brier_line, cl)
  # Cluster bootstrap for the scores on all games
  clusters <- split(seq_len(nrow(bt)), paste(bt$season, bt$week_seq))
  boot <- replicate(B, {
    idx <- unlist(clusters[sample.int(length(clusters), replace = TRUE)], use.names = FALSE)
    x <- bt[idx, ]
    c(mae = mean(abs(x$err)), brier = mean(x$brier), logloss = mean(x$logloss), crps = mean(x$crps, na.rm = TRUE))
  })
  ci <- apply(boot, 1, stats::quantile, c(0.025, 0.975))
  ats <- stats::binom.test(sum(lined$ats_win, na.rm = TRUE), sum(!is.na(lined$ats_win)))
  list(
    dm_mae = dm_mae, dm_brier = dm_brier, n_lined = nrow(lined),
    score_ci = tibble::tibble(metric = colnames(ci), lo = ci[1, ], hi = ci[2, ]),
    ats = c(rate = unname(ats$estimate), lo = ats$conf.int[1], hi = ats$conf.int[2], n = sum(!is.na(lined$ats_win)))
  )
}
