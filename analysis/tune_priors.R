# ==============================================================================
# analysis/tune_priors.R: evidence for the prior settings in model_config().
# Needs a completed targets build (reads inputs_<s> and snaps_<s>).
#
#   Rscript analysis/tune_priors.R structural   # A: full-season posteriors, every season
#   Rscript analysis/tune_priors.R halflife     # B: rolling backtest over recency half-lives
#
# A. The structural parameters (tier offsets, scales, carry-over, home field) barely move
#    between seasons, and early-week fits lean on their priors. Each season's final snapshot
#    is refit and the posterior means are pooled across seasons; the priors in model_config()
#    are centred on the pooled mean with sd >= 2x the between-season sd.
#    The final snapshots already have ~800 games, so the old priors have little pull on them.
# B. The candidate half-lives are refit week by week on 2022-2024 (each week predicts the
#    next). Scoring is the same as run_backtest(), using model_config() plus the override.
#
# Results are written to analysis/*.csv and summarised in plan.md.
# ==============================================================================

suppressPackageStartupMessages({ library(targets); library(dplyr) })
for (f in list.files("R", full.names = TRUE)) source(f)
mode <- commandArgs(trailingOnly = TRUE)[1] %||% "structural"
dir.create("analysis", showWarnings = FALSE)
fast <- function(cfg) { cfg$iter_warmup <- 400; cfg$iter_sampling <- 400; cfg$retry_sampling <- 800; cfg }
seasons_done <- 2014:(current_cfb_season() - 1L)
workers <- 2L   # 2 fits x 4 chains = 8 cores

# ---- A. Structural parameters ------------------------------------------------------
if (mode == "structural") {
  cfg <- fast(model_config())
  vars <- c("hfa", "b_prev", "lambda", "kappa", "sigma_eff", "sigma_m", "nu", "sigma_conf",
            "sigma_team[1]", "sigma_team[2]", "sigma_team[3]", "mu_tier[2]", "mu_tier[3]")
  res <- parallel::mclapply(seasons_done, function(s) {
    inputs <- tar_read_raw(paste0("inputs_", s))
    input <- inputs[[length(inputs)]]
    f <- fit_power(input, cfg)
    d <- posterior::summarise_draws(f$fit$draws(variables = vars), "mean", "sd")
    unlink(f$fit$output_files())
    d |> mutate(season = s, n_games = nrow(input$played), .before = 1)
  }, mc.cores = workers)
  res <- bind_rows(res)
  readr::write_csv(res, "analysis/structural_by_season.csv")
  pooled <- res |>
    filter(!(season == 2020 & variable == "hfa")) |>   # 2020: no crowds
    group_by(variable) |>
    summarise(pooled_mean = mean(mean), between_sd = sd(mean), typical_post_sd = mean(sd),
              min = min(mean), max = max(mean), .groups = "drop")
  readr::write_csv(pooled, "analysis/structural_pooled.csv")
  print(as.data.frame(pooled), digits = 3)
}

# ---- B. Recency half-life -------------------------------------------------------------
if (mode == "halflife") {
  candidates <- list(none = NULL, hl10 = 10, hl6 = 6, hl4 = 4)
  test_seasons <- 2022:2024
  jobs <- expand.grid(cand = names(candidates), season = test_seasons, stringsAsFactors = FALSE)
  season_data <- lapply(setNames(test_seasons, test_seasons), function(s) tar_read_raw(paste0("data_", s)))
  priors <- lapply(setNames(test_seasons, test_seasons), function(s) final_prior(tar_read_raw(paste0("snaps_", s - 1L))))

  one_week <- function(cand, s, w) {
    cfg <- fast(model_config()); cfg$half_life_weeks <- candidates[[cand]]
    # Point predictions only: skip the gate-driven retry (short chains rarely reach ESS 400)
    cfg$gates <- list(rhat = Inf, ess = 0, divergences = Inf, ebfmi = 0)
    input <- prepare_week_input(season_data[[as.character(s)]], w, priors[[as.character(s)]], cfg)
    nxt <- input$future |> filter(week_seq == w + 1L)
    if (nrow(nxt) == 0) return(NULL)
    f <- suppressWarnings(fit_power(input, cfg))
    draws <- extract_draws(f$fit); unlink(f$fit$output_files())
    predict_games(draws, nxt) |> mutate(cand = cand, season = s, week = w)
  }
  tasks <- do.call(rbind, lapply(seq_len(nrow(jobs)), function(i) {
    s <- jobs$season[i]; weeks <- setdiff(tar_read_raw(paste0("weeks_", s)), max(tar_read_raw(paste0("weeks_", s))))
    data.frame(cand = jobs$cand[i], season = s, week = weeks)
  }))
  cat(nrow(tasks), "fits\n")
  preds <- bind_rows(parallel::mclapply(seq_len(nrow(tasks)), function(i)
    with(tasks[i, ], one_week(cand, season, week)), mc.cores = workers))
  readr::write_csv(preds, "analysis/halflife_predictions.csv")

  games <- bind_rows(season_data |> lapply(`[[`, "games"))
  scored <- preds |>
    inner_join(games |> transmute(season, game_id, margin = home_points - away_points,
                                  home_win = as.integer(home_points > away_points),
                                  fbs = home_class == "fbs" & away_class == "fbs"), by = c("season", "game_id")) |>
    filter(!is.na(margin))
  summ <- function(df) df |> summarise(games = n(), mae = mean(abs(pred_margin - margin)),
                                       rmse = sqrt(mean((pred_margin - margin)^2)),
                                       brier = mean((pred_home_wp - home_win)^2),
                                       logloss = -mean(home_win * log(pred_home_wp) + (1 - home_win) * log(1 - pred_home_wp)),
                                       .groups = "drop")
  out <- bind_rows(
    scored |> group_by(cand) |> summ() |> mutate(scope = "all"),
    scored |> filter(fbs) |> group_by(cand) |> summ() |> mutate(scope = "fbs"),
    scored |> filter(fbs, week >= 4) |> group_by(cand) |> summ() |> mutate(scope = "fbs, week 5+"),
    scored |> filter(fbs, week >= 10) |> group_by(cand) |> summ() |> mutate(scope = "fbs, week 11+")
  )
  readr::write_csv(out, "analysis/halflife_backtest.csv")
  print(as.data.frame(out), digits = 4)
}
