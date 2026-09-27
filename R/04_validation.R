# ==============================================================================
# R/04_validation.R
# Snapshot runner, model checks (PPC, prior sensitivity, LOO), and the rolling
# out-of-sample backtest against results and closing lines.
# ==============================================================================

#' Fit + summarise one snapshot. Returns only summaries; CmdStan CSVs are deleted.
run_snapshot <- function(input, cfg = model_config()) {
  f <- fit_power(input, cfg)
  draws <- extract_draws(f$fit)
  unlink(f$fit$output_files())
  summarise_snapshot(input, draws, f$health, cfg)
}

#' Posterior predictive check on the fitted games: observed statistic vs. 90% predictive interval
posterior_predictive_check <- function(draws, played, n_draws = 400, seed = 1) {
  set.seed(seed)
  S <- nrow(draws$r)
  pick <- sample.int(S, min(n_draws, S))
  r <- draws$r[pick, , drop = FALSE]
  par <- draws$par[pick, ]
  mu <- r[, played$home, drop = FALSE] - r[, played$away, drop = FALSE] +
    par$hfa * matrix(played$home_adv, length(pick), nrow(played), byrow = TRUE)
  noise <- matrix(stats::rt(length(mu), df = rep(par$nu, nrow(played))), length(pick)) *
    par$sigma_marg
  # Scores are integers: round the replicates so boundary statistics (|margin| <= 7, >= 28)
  # compare like with like (a continuous replicate never lands exactly on 7 or 28)
  rep <- round(mu + noise)
  stat_fns <- list(
    "SD of margin" = function(m) stats::sd(m),
    "Share within 7 pts" = function(m) mean(abs(m) <= 7),
    "Share decided by 28+" = function(m) mean(abs(m) >= 28),
    "Home win rate (true home games)" = function(m) mean(m[played$home_adv == 1] > 0)
  )
  dplyr::bind_rows(lapply(names(stat_fns), function(nm) {
    fn <- stat_fns[[nm]]
    reps <- apply(rep, 1, fn)
    tibble::tibble(statistic = nm, observed = fn(played$margin),
                   rep_lo = stats::quantile(reps, 0.05, names = FALSE),
                   rep_hi = stats::quantile(reps, 0.95, names = FALSE),
                   p_value = mean(reps >= fn(played$margin)))
  }))
}

#' Full checks for a season's latest snapshot: gates (optionally strict), PPC,
#' priorsense power-scaling sensitivity, and PSIS-LOO.
run_checks <- function(inputs, cfg = model_config(), strict = FALSE) {
  if (length(inputs) == 0) return(NULL)
  input <- inputs[[length(inputs)]]
  if (nrow(input$played) == 0) return(NULL)
  f <- fit_power(input, cfg, compute_ll = TRUE, strict = strict)
  vars <- c("hfa", "b_prev", "sigma_m", "nu", "lambda", "kappa", "sigma_eff", "sigma_team", "sigma_conf", "mu_tier[2]", "mu_tier[3]")
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
  draws <- extract_draws(f$fit)
  ppc <- posterior_predictive_check(draws, input$played)
  unlink(f$fit$output_files())
  list(season = input$season, week = input$week, health = f$health, ppc = ppc, sensitivity = sens, loo = loo_est)
}

#' Last season's final Power ratings, used as next season's preseason information
final_prior <- function(snaps) {
  last <- snaps[[length(snaps)]]
  last$ratings |> dplyr::select(team_id, rating = power)
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

#' Out-of-sample backtest: each game is predicted by the snapshot fitted through the
#' previous week (no future information), then compared with the result and the line.
run_backtest <- function(predictions, games) {
  played <- games |>
    dplyr::filter(completed, !is.na(home_points), !is.na(away_points)) |>
    dplyr::transmute(season, game_id, week_seq, margin = home_points - away_points,
                     home_win = as.integer(home_points > away_points),
                     vegas_margin = -spread,
                     fbs_game = home_class == "fbs" & away_class == "fbs",
                     d1_game = home_class %in% c("fbs", "fcs") & away_class %in% c("fbs", "fcs"))
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
      ats_win = dplyr::if_else(ats_result == 0, NA, (ats_result > 0) == ats_pick_home)
    )

  summarise_bt <- function(df) {
    df |> dplyr::summarise(
      games = dplyr::n(),
      mae = mean(abs(err)), rmse = sqrt(mean(err^2)),
      brier = mean(brier), logloss = mean(logloss), accuracy = mean(correct),
      games_with_line = sum(!is.na(vegas_margin)),
      mae_model_lined = mean(abs(err[!is.na(vegas_margin)])),
      mae_vegas = mean(abs(vegas_err), na.rm = TRUE),
      ats_win_rate = mean(ats_win, na.rm = TRUE),
      .groups = "drop"
    )
  }
  calibration <- bt |>
    dplyr::mutate(bin = cut(pred_home_wp, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(pred = mean(pred_home_wp), obs = mean(home_win), n = dplyr::n(), .groups = "drop")

  list(
    games = bt,
    overall = dplyr::bind_rows(
      summarise_bt(bt) |> dplyr::mutate(scope = "All games", .before = 1),
      summarise_bt(dplyr::filter(bt, fbs_game)) |> dplyr::mutate(scope = "FBS vs FBS", .before = 1),
      summarise_bt(dplyr::filter(bt, week >= 4, fbs_game)) |> dplyr::mutate(scope = "FBS vs FBS, week 5+", .before = 1)
    ),
    by_season = bt |> dplyr::filter(fbs_game) |> dplyr::group_by(season) |> summarise_bt(),
    by_week = bt |> dplyr::filter(fbs_game) |> dplyr::group_by(week = week + 1L) |> summarise_bt(),
    calibration = calibration
  )
}
