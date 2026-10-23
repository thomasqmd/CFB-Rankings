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
#   margin_comparison  the margin families compared by LOO on each complete season (picks the
#                      published family: choose_margin_family)
# and cheap summaries of the published snapshots (resume validation, schedule graph).
# ==============================================================================

#' Short chains for point predictions: no gates, no retry
fast_config <- function(cfg = model_config(), chains = 2L, warmup = 400L, sampling = 400L) {
  cfg$chains <- chains; cfg$iter_warmup <- warmup; cfg$iter_sampling <- sampling
  cfg$gates <- list(rhat = Inf, ess = 0, divergences = Inf, ebfmi = 0)
  cfg
}


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
  # tau: wide, but centred away from 0 (a prior with its mode at 0 puts the sampler in the funnel
  # of the random walk); nu: Gamma(2, 0.05), mean 40
  cfg$tau_r_prior <- c(0.8, 0.8); cfg$tau_t_prior <- c(1, 0.8)
  cfg$nu_prior <- c(2, 0.05)
  cfg$sigma_gap_prior <- c(0, 25); cfg$gamma_prior <- c(0, 1)
  cfg$base_prior <- c(25, 20); cfg$t_tier_mean <- c(0, 0); cfg$t_tier_sd <- c(50, 50)
  cfg$sigma_t_team_prior <- c(0, 25); cfg$sigma_conf_t_prior <- c(0, 25); cfg$b_prev_t_prior <- c(0.5, 1)
  cfg$lambda_t_prior <- c(0, 2); cfg$alpha_es_prior <- c(0, 50); cfg$kappa_t_prior <- c(0, 2)
  cfg$sigma_es_prior <- c(0, 50); cfg$sigma_tot_prior <- c(0, 50)
  cfg$b_prev_low_prior <- c(0.5, 1); cfg$b_ret_prior <- c(0, 2); cfg$c_ret_prior <- c(0, 50)
  cfg$b_tal_prior <- c(0, 10); cfg$b_prev2_prior <- c(0, 1); cfg$delta_coach_prior <- c(0, 1)
  cfg
}

#' Longer chains than the published ones, for full-season fits under the vague priors: under the
#' wide random-walk priors the tails of the step sizes mix slowly
long_chains <- function(cfg) {
  cfg$iter_warmup <- 1000L; cfg$iter_sampling <- 2000L
  cfg$retry_warmup <- 1500L; cfg$retry_sampling <- 4000L
  cfg
}

#' Refit each season's final snapshot under the vague priors
#' @param final_inputs list of final-week inputs, one per complete season
run_prior_refits <- function(final_inputs, cfg = model_config(), workers = 2L, seed = 2026) {
  vcfg <- long_chains(vague_config(cfg))
  res <- parallel::mclapply(final_inputs, function(input) {
    input <- apply_priors(input, vcfg)
    f <- fit_power(input, vcfg, seed = seed)
    d <- posterior::summarise_draws(f$fit$draws(variables = model_vars(input$stan_data)), "mean", "sd")
    unlink(f$fit$output_files())
    d |> dplyr::mutate(season = input$season, n_games = nrow(input$played), passed = f$health$passed, .before = 1)
  }, mc.cores = workers, mc.preschedule = FALSE)
  dplyr::bind_rows(res)
}

#' Pool the vague-prior refits into priors. 2014 is left out (no carry-over that season, so
#' conference effects absorb team strength), as is 2020 for home field (no crowds). `drop` removes
#' more seasons (leave-one-season-out). Seasons are weighted by recency, 0.5^(|season - target| /
#' half_life), where target is the season the priors are for: the dropped season when one is
#' dropped, else the season after the last refit. The prior is centred on the weighted mean; its sd
#' is sd_mult x the between-season sd (sd_rule = "mult") or the random-effects predictive sd for a
#' new season (sd_rule = "predictive"): the between-season sd in excess of the one-season posterior
#' sd, plus the uncertainty of the mean, floored at sd_floor x the one-season posterior sd (for the
#' variables in floor_vars only, when it is set). The defaults come from model_config()$pool.
pool_priors <- function(refits, drop = integer(0), exclude = 2014L, hfa_exclude = 2020L,
                        pool = model_config()$pool, target = NULL) {
  half_life <- pool$half_life %||% Inf
  x <- refits |>
    dplyr::filter(!season %in% c(exclude, drop), !(variable == "hfa" & season %in% hfa_exclude))
  target <- target %||% if (length(drop) == 1) drop else max(x$season) + 1L
  x |>
    dplyr::mutate(w = 0.5^(abs(season - target) / half_life)) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      pooled_mean = stats::weighted.mean(mean, w),
      between_sd = sqrt(sum(w * (mean - pooled_mean)^2) / (sum(w) - sum(w^2) / sum(w))),
      typical_post_sd = sqrt(stats::weighted.mean(sd^2, w)),
      n_eff = sum(w)^2 / sum(w^2),
      min = min(mean), max = max(mean), n_seasons = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      excess_sd = sqrt(pmax(between_sd^2 - typical_post_sd^2, 0)),
      floor = (pool$sd_floor %||% 0) * (is.null(pool$floor_vars) | variable %in% pool$floor_vars),
      predictive_sd = pmax(sqrt(excess_sd^2 + between_sd^2 / n_eff), floor * typical_post_sd),
      prior_sd = if (identical(pool$sd_rule, "predictive")) predictive_sd else (pool$sd_mult %||% 2) * between_sd)
}

#' cfg with its structural priors replaced by pooled ones (the mixture's sigma_w / p_wide keep
#' their own priors; parameters absent from the refits keep cfg's). nu's Gamma prior matches the
#' pooled mean and sd.
priors_from_pool <- function(cfg, pooled) {
  m <- stats::setNames(pooled$pooled_mean, pooled$variable)
  s <- stats::setNames(pooled$prior_sd, pooled$variable)
  pair <- function(v) if (v %in% names(m)) unname(c(m[[v]], s[[v]]))
  if ("nu" %in% names(m)) cfg$nu_prior <- unname(c(m[["nu"]]^2 / s[["nu"]]^2, m[["nu"]] / s[["nu"]]^2))
  for (x in list(c("tau_r_prior", "tau_r"), c("tau_t_prior", "tau_t_raw[1]"), c("gamma_prior", "gamma_m"),
                 c("b_prev_low_prior", "b_prev_low_raw[1]"), c("b_ret_prior", "b_ret_raw[1]"),
                 c("c_ret_prior", "c_ret_raw[1]"), c("b_tal_prior", "b_tal_raw[1]"),
                 c("b_prev2_prior", "b_prev2_raw[1]"), c("delta_coach_prior", "delta_coach_raw[1]"),
                 c("base_prior", "base_raw[1]"), c("sigma_t_team_prior", "sigma_t_team_raw[1]"),
                 c("sigma_conf_t_prior", "sigma_conf_t_raw[1]"), c("b_prev_t_prior", "b_prev_t_raw[1]"),
                 c("lambda_t_prior", "lambda_t_raw[1]"), c("alpha_es_prior", "alpha_es_raw[1]"),
                 c("kappa_t_prior", "kappa_t_raw[1]"), c("sigma_es_prior", "sigma_es_raw[1]"),
                 c("sigma_tot_prior", "sigma_tot_raw[1]"))) {
    v <- pair(x[2])
    if (!is.null(v)) cfg[[x[1]]] <- v
  }
  if (all(c("t_tier_raw[1]", "t_tier_raw[2]") %in% names(m))) {
    cfg$t_tier_mean <- unname(m[c("t_tier_raw[1]", "t_tier_raw[2]")]); cfg$t_tier_sd <- unname(s[c("t_tier_raw[1]", "t_tier_raw[2]")])
  }
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

#' Each season's vague-prior estimate against the prior pooled from the other seasons: z is its
#' distance from that prior's mean in units of sqrt(prior sd^2 + its own posterior sd^2), the sd of
#' a new season's estimate under the prior
loso_prior_check <- function(refits, pool = model_config()$pool, exclude = 2014L, hfa_exclude = 2020L) {
  seasons <- setdiff(sort(unique(refits$season)), exclude)
  dplyr::bind_rows(lapply(seasons, function(s0) {
    p <- pool_priors(refits, drop = s0, exclude = exclude, hfa_exclude = hfa_exclude, pool = pool)
    refits |>
      dplyr::filter(season == s0, !(variable == "hfa" & season %in% hfa_exclude)) |>
      dplyr::inner_join(dplyr::select(p, variable, pooled_mean, prior_sd), by = "variable") |>
      dplyr::transmute(season, variable, z = (mean - pooled_mean) / sqrt(prior_sd^2 + sd^2))
  }))
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
    lcfg <- priors_from_pool(cfg, pool_priors(refits, drop = as.integer(s), pool = cfg$pool))
    f <- fit_power(apply_priors(input, lcfg), lcfg, seed = seed)
    draws <- extract_draws(f$fit, family = lcfg$margin_family); unlink(f$fit$output_files())
    predict_games(draws, nxt, input$week) |>
      dplyr::mutate(season = as.integer(s), week = input$week, sigma = mean(draws$par$sigma_marg),
                    nu = mean(draws$par$nu), .before = 1)
  }, mc.cores = workers, mc.preschedule = FALSE)
  dplyr::bind_rows(res)
}

#' Early-week comparison of prior settings (plans/todo.md, Phase 0). Each variant is a list of
#' cfg changes: model switches, `pool` settings or prior overrides. Its priors are pooled, leaving
#' out the season predicted, from vague-prior refits of every season's final week under the
#' variant's model (variants with the same model switches share refits); weeks min_week..max_week
#' of each season are then fitted with short chains and predict the following week. A variant
#' with `fixed_priors = TRUE` keeps its cfg priors instead (no pooling); `after_pool` (a list of
#' cfg changes) overrides pooled priors.
#' @param season_data,priors,priors2,weeks lists named by season (priors = final_prior of s - 1,
#'   priors2 = final_prior of s - 2 or NULL)
#' @return list(refits = vague-prior refits by model key, preds = predictions by variant)
run_prior_variants <- function(season_data, priors, priors2, weeks, variants, cfg = model_config(),
                               min_week = 0L, max_week = 3L, workers = 4L, seed = 2026, refits = NULL,
                               refit_cfg = fast_config(cfg, chains = 4L, warmup = 500L, sampling = 500L)) {
  seasons <- names(season_data)
  model_switches <- c("b_prev_tier", "use_ret", "use_tal", "use_prev2", "use_coach", "margin_family",
                      "dynamic", "use_total", "use_eff", "conf_level", "carry_over")
  vcfgs <- lapply(variants, \(v) utils::modifyList(cfg, v[setdiff(names(v), c("fixed_priors", "after_pool"))]))
  key_of <- function(x) paste(vapply(model_switches, \(k) as.character(x[[k]] %||% NA), character(1)), collapse = "|")
  keys <- vapply(vcfgs, key_of, character(1))
  pooled_variants <- !vapply(variants, \(v) isTRUE(v$fixed_priors), logical(1))
  need <- setdiff(unique(keys[pooled_variants]), unique(refits$key))
  if (length(need)) {
    jobs <- expand.grid(key = need, season = seasons, stringsAsFactors = FALSE)
    new <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
      s <- jobs$season[j]
      vc <- vcfgs[[match(jobs$key[j], keys)]]
      rc <- vague_config(utils::modifyList(vc, refit_cfg[c("chains", "iter_warmup", "iter_sampling", "gates")]))
      input <- prepare_week_input(season_data[[s]], max(weeks[[s]]), priors[[s]], rc, priors2[[s]])
      f <- suppressWarnings(fit_power(input, rc, seed = seed))
      d <- posterior::summarise_draws(f$fit$draws(variables = model_vars(input$stan_data)), "mean", "sd")
      unlink(f$fit$output_files())
      d |> dplyr::mutate(key = jobs$key[j], season = input$season, n_games = nrow(input$played),
                         passed = f$health$passed, .before = 1)
    }, mc.cores = workers, mc.preschedule = FALSE)
    bad <- vapply(new, inherits, logical(1), "try-error")
    if (any(bad)) stop("refit failed: ", conditionMessage(attr(new[[which(bad)[1]]], "condition")))
    refits <- dplyr::bind_rows(refits, new)
  }
  tasks <- do.call(rbind, lapply(names(variants), function(v) do.call(rbind, lapply(seasons, function(s) {
    w <- weeks[[s]]
    w <- w[w >= min_week & w <= max_week & w < max(w)]
    if (length(w)) data.frame(variant = v, season = s, week = w, stringsAsFactors = FALSE)
  }))))
  preds <- parallel::mclapply(seq_len(nrow(tasks)), function(j) {
    tk <- tasks[j, ]
    i <- match(tk$variant, names(variants))
    vc <- utils::modifyList(fast_config(vcfgs[[i]]), list(margin_family = vcfgs[[i]]$margin_family))
    if (pooled_variants[i]) {
      rf <- dplyr::filter(refits, key == keys[i]) |> dplyr::select(-key)
      vc <- priors_from_pool(vc, pool_priors(rf, drop = as.integer(tk$season), pool = vc$pool))
    }
    vc <- utils::modifyList(vc, variants[[i]]$after_pool %||% list())
    input <- prepare_week_input(season_data[[tk$season]], tk$week, priors[[tk$season]], vc, priors2[[tk$season]])
    nxt <- dplyr::filter(input$future, week_seq == tk$week + 1L)
    if (nrow(nxt) == 0) return(NULL)
    f <- suppressWarnings(fit_power(input, vc, seed = seed))
    draws <- extract_draws(f$fit, family = vc$margin_family); unlink(f$fit$output_files())
    predict_games(draws, nxt, tk$week) |>
      dplyr::mutate(variant = tk$variant, season = as.integer(tk$season), week = tk$week,
                    sigma = mean(draws$par$sigma_marg), nu = mean(draws$par$nu),
                    passed = f$health$passed, .before = 1)
  }, mc.cores = workers, mc.preschedule = FALSE)
  bad <- vapply(preds, inherits, logical(1), "try-error")
  if (any(bad)) stop("variant fit failed: ", conditionMessage(attr(preds[[which(bad)[1]]], "condition")))
  list(refits = refits, preds = dplyr::bind_rows(preds))
}

#' The structural priors in use before the 2026-10 rework (pooled under the static Student-t
#' model with equal weights and sd = 2x the between-season sd; tau, nu and the total model set by
#' hand) and no preseason terms: the reference for prior_variants
priors_2026_10_10 <- function() {
  list(hfa_mean = 2.4, hfa_sd = 0.801, tier_mean = c(-29.2, -61.2), tier_sd = c(3.45, 7.31),
       b_prev_mean = 0.779, b_prev_sd = 0.0867, sigma_team_mean = c(6.73, 6.32, 13.6),
       sigma_team_sd = c(1.45, 1.56, 9.35), sigma_conf_prior = c(2.95, 2.53),
       lambda_prior = c(.7623, .0486), kappa_prior = c(.7729, .0594), sigma_eff_prior = c(14.255, .681),
       sigma_m_prior = c(8.011, .806), nu_prior = c(2, 0.1), tau_r_prior = c(0.6, 0.25), tau_t_prior = c(1.0, 0.3),
       base_prior = c(26.5, 2), t_tier_mean = c(0, 0), t_tier_sd = c(6, 10), sigma_t_team_prior = c(4, 2),
       sigma_conf_t_prior = c(2, 1.5), b_prev_t_prior = c(0.5, 0.25), lambda_t_prior = c(0.8, 0.3),
       alpha_es_prior = c(13.5, 8), kappa_t_prior = c(0.55, 0.2), sigma_es_prior = c(17, 4),
       sigma_tot_prior = c(12.5, 2), gamma_prior = c(0, 0.3),
       b_prev_tier = FALSE, use_ret = FALSE, use_tal = FALSE, use_prev2 = FALSE, use_coach = FALSE)
}

preseason_labels <- c(b_prev_tier = "FCS carry-over", use_ret = "returning production", use_tal = "talent",
                      use_prev2 = "two-season carry-over", use_coach = "coaching change")

#' Variants for the prior_variants target: the old priors, the published priors (pooled leaving
#' the season out), the published model with equal-weight / 2x-sd pooling, and the published model
#' without each preseason term it uses
prior_variant_set <- function(cfg = model_config()) {
  on <- names(preseason_labels)[vapply(names(preseason_labels), \(k) isTRUE(cfg[[k]]), logical(1))]
  c(
    list("Before (2026-10-10 priors)" = c(priors_2026_10_10(), fixed_priors = TRUE),
         "Published" = list(),
         "Published, equal weights and sd x 2" = list(pool = list(half_life = Inf, sd_rule = "mult", sd_mult = 2, sd_floor = 0))),
    if (length(on)) stats::setNames(lapply(on, \(k) stats::setNames(list(FALSE), k)), paste("Without", preseason_labels[on]))
  )
}

#' Per-game scores of run_prior_variants() predictions (absolute error, log loss, CRPS and
#' interval coverage from the predictive quantiles), joined to the results and closing lines
score_variant_predictions <- function(preds, games_all) {
  res <- games_all |>
    dplyr::filter(completed, !is.na(home_points), !is.na(away_points)) |>
    dplyr::transmute(season, game_id, week_seq, margin = home_points - away_points,
                     home_win = as.integer(home_points > away_points), vegas_margin = -spread,
                     fbs_game = home_class %in% "fbs" & away_class %in% "fbs")
  preds |>
    dplyr::inner_join(res, by = c("season", "game_id")) |>
    dplyr::filter(week_seq == week + 1L) |>
    dplyr::mutate(
      abs_err = abs(pred_margin - margin),
      logloss = -(home_win * log(pmax(pred_home_wp, 1e-6)) + (1 - home_win) * log(pmax(1 - pred_home_wp, 1e-6))),
      crps = crps_quantiles(margin, pred_q),
      cover80 = margin >= quantile_at(pred_q, 0.10) & margin <= quantile_at(pred_q, 0.90),
      cover95 = margin >= quantile_at(pred_q, 0.025) & margin <= quantile_at(pred_q, 0.975),
      vegas_abs_err = abs(vegas_margin - margin)
    ) |>
    dplyr::select(-pred_q)
}

#' Each variant against a reference on the same games: mean differences in absolute error, log
#' loss and CRPS (negative = better than the reference) with paired bootstrap sds, and coverage
compare_variants <- function(scores, reference, by_week = FALSE, B = 1000L, seed = 1) {
  set.seed(seed)
  ref <- scores |> dplyr::filter(variant == reference) |>
    dplyr::select(season, week, game_id, r_ae = abs_err, r_ll = logloss, r_crps = crps)
  d <- scores |> dplyr::inner_join(ref, by = c("season", "week", "game_id"))
  if (by_week) d <- dplyr::group_by(d, variant, week) else d <- dplyr::group_by(d, variant)
  boot_sd <- function(x) {
    n <- length(x)
    stats::sd(replicate(B, mean(x[sample.int(n, n, replace = TRUE)])))
  }
  # differences are summarised before the means overwrite the per-game columns
  d |> dplyr::summarise(
    games = dplyr::n(),
    d_mae = mean(abs_err - r_ae), d_mae_sd = boot_sd(abs_err - r_ae),
    d_logloss = mean(logloss - r_ll), d_logloss_sd = boot_sd(logloss - r_ll),
    d_crps = mean(crps - r_crps), d_crps_sd = boot_sd(crps - r_crps),
    mae = mean(abs_err), logloss = mean(logloss), crps = mean(crps),
    cover80 = mean(cover80), cover95 = mean(cover95), mae_vegas = mean(vegas_abs_err, na.rm = TRUE),
    fits_passed = mean(passed), .groups = "drop")
}

# ---- Ablations and baselines --------------------------------------------------------

family_labels <- c(normal = "Normal margin", t = "Student-t margin", mixture = "Mixture margin",
                   scale_t = "Scale-t margin")

#' The published model and one variant per switch; every margin family other than the
#' published one is a variant too
ablation_variants <- function(cfg = model_config()) {
  others <- setdiff(names(family_labels), cfg$margin_family)
  c(
    list(
      "Published model" = list(),
      "Margin only" = list(use_eff = FALSE),
      "No carry-over" = list(carry_over = FALSE),
      "No conference level" = list(conf_level = FALSE),
      "Static ratings" = list(dynamic = FALSE)
    ),
    stats::setNames(lapply(others, \(f) list(margin_family = f)), family_labels[others])
  )
}

#' Rolling backtest of each variant: fit through week w with short chains, predict week w + 1.
#' Every variant predicts exactly the same games.
#' @param season_data,priors,weeks lists named by season (priors = final_prior of season - 1)
#' @param priors2 list named by season of final_prior of season - 2 (NULL entries allowed)
run_ablation <- function(season_data, priors, weeks, cfg = model_config(), workers = 4L, seed = 2026,
                         priors2 = list()) {
  variants <- ablation_variants(cfg)
  tasks <- do.call(rbind, lapply(names(season_data), function(s) {
    w <- weeks[[s]]
    expand.grid(variant = names(variants), season = s, week = setdiff(w, max(w)), stringsAsFactors = FALSE)
  }))
  res <- parallel::mclapply(seq_len(nrow(tasks)), function(j) {
    tk <- tasks[j, ]
    vcfg <- utils::modifyList(fast_config(cfg), variants[[tk$variant]])
    input <- prepare_week_input(season_data[[tk$season]], tk$week, priors[[tk$season]], vcfg, priors2[[tk$season]])
    nxt <- dplyr::filter(input$future, week_seq == tk$week + 1L)
    if (nrow(nxt) == 0) return(NULL)
    f <- suppressWarnings(fit_power(input, vcfg, seed = seed))
    draws <- extract_draws(f$fit, family = vcfg$margin_family); unlink(f$fit$output_files())
    predict_games(draws, nxt, tk$week) |>
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
    dplyr::mutate(team_id = school_team_id(team, teams_meta))
}

# ---- Final-week refits ----------------------------------------------------------------

#' WAB per draw (S x teams) against the b-th best FBS team; r must be FBS-centred
wab_draws <- function(r, par, teams, played, b = 12L, family = "t") {
  tg <- team_game_rows(played)
  S <- nrow(r)
  fbs_cols <- which(teams$tier == 1L)
  bench <- apply(r[, fbs_cols, drop = FALSE], 1, function(x) sort(x, decreasing = TRUE)[b])
  venue <- matrix(tg$venue, S, nrow(tg), byrow = TRUE)
  p_bench <- win_prob(bench + par$hfa * venue - r[, tg$opp, drop = FALSE], par, family)
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
    if (jobs$model[j] == "full") out <- c(out, final_refit_summaries(f$fit, input, cfg$margin_family))
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
final_refit_summaries <- function(fit, input, family = "t") {
  teams <- input$teams
  fbs <- teams$tier == 1L
  arr_r <- fit$draws("r", format = "draws_array")
  n_iter <- posterior::niterations(arr_r); n_ch <- posterior::nchains(arr_r)
  r <- unclass(posterior::as_draws_matrix(arr_r))
  r <- r - rowMeans(r[, fbs, drop = FALSE])
  par <- extract_draws(fit)$par

  # Benchmark rank b = 4, 12, 25
  bench <- dplyr::bind_rows(lapply(c(4L, 12L, 25L), function(b) {
    w <- wab_draws(r, par, teams, input$played, b, family)$wab[, fbs, drop = FALSE]
    rk <- rank_draws(w)
    tibble::tibble(team_id = teams$team_id[fbs], team = teams$team[fbs], b = b, wab = colMeans(w),
                   rank = rank(-colMeans(w), ties.method = "first"), p_top12 = colMeans(rk <= 12))
  }))

  # SOR from the posterior-mean benchmark probabilities (published) and averaged per draw
  wd <- wab_draws(r, par, teams, input$played, 12L, family)
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
    w <- wab_draws(r, draws$par, tm, pl, 12L, cfg$margin_family)$wab
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
#' what generated the data. The margin model only (the total model is switched off), with the
#' published margin family and random walk.
run_sbc <- function(input, cfg = model_config(), n_sims = 100L, workers = 2L, n_rank_draws = 99L) {
  tm <- input$teams
  pl <- input$played
  fbs <- tm$tier == 1L
  family <- cfg$margin_family %||% "t"
  scfg <- cfg
  scfg$chains <- 4L; scfg$iter_warmup <- 400L; scfg$iter_sampling <- 250L
  scfg$gates <- list(rhat = Inf, ess = 0, divergences = Inf, ebfmi = 0)
  scfg$use_total <- FALSE
  res <- parallel::mclapply(seq_len(n_sims), function(i) {
    set.seed(1000 + i)
    p <- draw_prior_params(input, cfg)
    pp <- prior_par(p)
    sd_ <- input$stan_data
    mu <- p$r_state[sd_$home_state] - p$r_state[sd_$away_state] + p$hfa * pl$home_adv
    he <- sd_$has_eff == 1L
    e <- ifelse(he, p$lambda * mu + stats::rnorm(nrow(pl), 0, p$sigma_eff), 0)
    # Margin given eff: the family with the conditional scales, located at mu + kappa * residual
    # (the scale-t scale still grows with mu); without eff, the marginal law
    pc <- pp; pc$sigma_marg <- p$sigma_m; pc$sigma_marg_w <- p$sigma_w
    y_cond <- mu + p$kappa * (e - p$lambda * mu) + (margin_rng(mu, pc, family) - mu)
    y <- ifelse(he, y_cond, margin_rng(mu, pp, family))
    sim <- input
    sim$stan_data$margin <- y
    sim$stan_data$eff <- e
    sim$stan_data$use_total <- 0L
    sim$played$margin <- y
    sim$played$home_points <- pmax(y, 0); sim$played$away_points <- pmax(-y, 0)
    f <- suppressWarnings(fit_power(sim, scfg, seed = i))
    draws <- extract_draws(f$fit)
    unlink(f$fit$output_files())

    # True quantities (FBS-centred), in the same form as the published ones
    r0 <- p$r_now - mean(p$r_now[fbs])
    w0 <- wab_draws(matrix(r0, 1), pp, tm, sim$played, 12L, family)$wab[1, fbs]
    r <- draws$r - rowMeans(draws$r[, fbs, drop = FALSE])
    w <- wab_draws(r, draws$par, tm, sim$played, 12L, family)$wab[, fbs, drop = FALSE]
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
    scal <- c("hfa", "b_prev", "sigma_m", "lambda", "kappa", "sigma_conf",
              if (family %in% c("t", "scale_t")) "nu", if (!isFALSE(cfg$dynamic)) "tau_r",
              if (family == "mixture") c("p_wide", "sigma_w"), if (family == "scale_t") "gamma_m",
              if (isTRUE(cfg$b_prev_tier)) "b_prev_low", if (isTRUE(cfg$use_ret)) c("b_ret", "c_ret"),
              if (isTRUE(cfg$use_tal)) "b_tal", if (isTRUE(cfg$use_prev2)) "b_prev2", if (isTRUE(cfg$use_coach)) "delta_coach")
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

# ---- Margin family ---------------------------------------------------------------------

#' Each margin family fit to every complete season's final week, under the vague priors and
#' without carry-over (so the comparison does not depend on the published fits, which depend on
#' its result) and without the total model (the family only enters the margin likelihood).
#' Records PSIS-LOO pointwise elpd of the fitted likelihood (efficiency and margin jointly)
#' and the posterior predictive checks of the margin (share within 7 points, ...).
#' @param season_data list of read_season() results, named by season
run_margin_comparison <- function(season_data, cfg = model_config(), families = c("t", "mixture", "scale_t"),
                                  workers = 2L, seed = 2026) {
  vcfg <- long_chains(vague_config(cfg))
  vcfg$carry_over <- FALSE
  vcfg$use_total <- FALSE
  jobs <- expand.grid(season = names(season_data), family = families, stringsAsFactors = FALSE)
  res <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
    d <- season_data[[jobs$season[j]]]
    fcfg <- vcfg; fcfg$margin_family <- jobs$family[j]
    wk <- max(concluded_weeks(d$games, min_share = 0))
    input <- prepare_week_input(d, wk, prior = NULL, cfg = fcfg)
    f <- fit_power(input, fcfg, compute_ll = TRUE, seed = seed)
    ll <- f$fit$draws("log_lik", format = "draws_array")
    l <- suppressWarnings(loo::loo(ll, r_eff = loo::relative_eff(exp(ll))))
    draws <- extract_draws(f$fit, states = TRUE, family = jobs$family[j])
    ppc <- posterior_predictive_check(draws, input)
    unlink(f$fit$output_files())
    list(season = as.integer(jobs$season[j]), family = jobs$family[j], health = f$health,
         elpd = l$pointwise[, "elpd_loo"], n_bad_k = sum(loo::pareto_k_values(l) > 0.7),
         par = posterior::summarise_draws(posterior::as_draws_df(draws$par), "mean", "sd"), ppc = ppc)
  }, mc.cores = workers, mc.preschedule = FALSE)
  failed <- vapply(res, inherits, logical(1), "try-error")
  if (any(failed)) stop("margin_comparison: ", sum(failed), " fits failed: ", as.character(res[failed][[1]]))
  res
}

#' Pooled LOO comparison of the margin families against the incumbent (cfg$margin_family):
#' per season, the summed pointwise elpd difference and its standard error; pooled over seasons
#' (independent), the sum and sqrt(sum se^2)
compare_margin_families <- function(comparison, incumbent = "t") {
  seasons <- unique(vapply(comparison, `[[`, integer(1), "season"))
  get <- function(s, f) Filter(\(x) x$season == s && x$family == f, comparison)[[1]]
  families <- unique(vapply(comparison, `[[`, "", "family"))
  per_season <- dplyr::bind_rows(lapply(seasons, function(s) {
    base <- get(s, incumbent)$elpd
    dplyr::bind_rows(lapply(families, function(f) {
      x <- get(s, f)
      d <- x$elpd - base
      tibble::tibble(season = s, family = f, elpd = sum(x$elpd), diff = sum(d), se = sqrt(length(d) * stats::var(d)),
                     n_bad_k = x$n_bad_k, passed = x$health$passed)
    }))
  }))
  pooled <- per_season |>
    dplyr::group_by(family) |>
    dplyr::summarise(elpd = sum(elpd), diff = sum(diff), se = sqrt(sum(se^2)), seasons = dplyr::n(),
                     seasons_better = sum(diff > 0), all_passed = all(passed), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(elpd))
  ppc <- dplyr::bind_rows(lapply(comparison, \(x) dplyr::mutate(x$ppc, season = x$season, family = x$family)))
  list(per_season = per_season, pooled = pooled, ppc = ppc)
}

#' The published margin family: the family with the best pooled elpd, provided it beats the
#' incumbent (cfg$margin_family) by more than `min_z` pooled standard errors; otherwise the
#' incumbent stays. Records the decision in cfg$margin_choice.
choose_margin_family <- function(cfg, comparison, min_z = 2) {
  cmp <- compare_margin_families(comparison, cfg$margin_family)
  best <- cmp$pooled[1, ]
  switch_ok <- best$family != cfg$margin_family && best$diff > min_z * best$se
  chosen <- if (switch_ok) best$family else cfg$margin_family
  message(sprintf("[margin family] %s (best pooled: %s, elpd diff vs %s %.1f, se %.1f)",
                  chosen, best$family, cfg$margin_family, best$diff, best$se))
  cfg$margin_choice <- list(incumbent = cfg$margin_family, chosen = chosen, min_z = min_z, pooled = cmp$pooled)
  cfg$margin_family <- chosen
  cfg
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

#' Counts and coverage shares of one season's games through week_seq `cut`, and of its
#' preseason inputs. kind: "count" (flagged if far from the reference range either way),
#' "share" (flagged if well below it) or "defect" (any above 0 fails).
season_data_metrics <- function(d, cut, teams_meta = NULL) {
  g <- d$games |> dplyr::filter(week_seq <= cut)
  done <- g$completed & !is.na(g$home_points) & !is.na(g$away_points)
  fbs_side <- g$home_class %in% "fbs" | g$away_class %in% "fbs"
  both_fbs <- g$home_class %in% "fbs" & g$away_class %in% "fbs"
  no_class <- is.na(g$home_class) | is.na(g$away_class)
  fbs_ids <- unique(c(g$home_id[g$home_class %in% "fbs"], g$away_id[g$away_class %in% "fbs"]))
  share <- function(x) if (length(x)) mean(x) else NA_real_
  pre <- d$preseason
  pre_cov <- function(col, key) {
    if (d$season < PRESEASON_FIRST_SEASON[[key]] || !length(fbs_ids)) return(NA_real_)
    have <- if (is.null(pre) || !nrow(pre)) integer(0) else pre$team_id[!is.na(pre[[col]])]
    mean(fbs_ids %in% have)
  }
  pair <- paste(substr(g$start_date, 1, 10), pmin(g$home_id, g$away_id), pmax(g$home_id, g$away_id))
  # Presence only: the table holds today's classification, so a team that has since left FBS
  # (Idaho, 2018) is not stale
  stale <- if (is.null(teams_meta) || !nrow(teams_meta)) NA_real_ else sum(!fbs_ids %in% teams_meta$team_id)
  tibble::tribble(
    ~metric,                                   ~kind,    ~value,
    "FBS games",                               "count",  sum(done & fbs_side),
    "Games without an FBS team",               "count",  sum(done & !fbs_side),
    "Games with a team of unknown division",   "count",  sum(done & no_class),
    "FBS teams",                               "count",  length(fbs_ids),
    "Efficiency coverage (FBS games)",         "share",  share(!is.na(g$home_ppa[done & fbs_side]) & !is.na(g$away_ppa[done & fbs_side])),
    "Line coverage (FBS vs FBS)",              "share",  share(!is.na(g$spread[done & both_fbs])),
    "Returning production coverage (FBS)",     "share",  pre_cov("ret_ppa", "ret"),
    "Talent coverage (FBS)",                   "share",  pre_cov("talent", "tal"),
    "Duplicate games",                         "defect", sum(duplicated(g$game_id)) + sum(duplicated(pair)),
    "Finished games without a score",          "defect", sum(g$completed & !done),
    "FBS teams missing from the teams table",  "defect", stale
  ) |> dplyr::mutate(season = d$season, .before = 1)
}

#' Data integrity: each season's data against the seasons before it, every season cut at the
#' live season's latest concluded week so a partial season compares like for like. A defect
#' (duplicate games, a finished game without a score, an FBS team the teams table lacks) FAILs;
#' a count more than `tol` outside the range of the previous `n_ref` seasons (2020 left out), or
#' a coverage share more than 5 points below it, WARNs. The live season's problems are raised
#' as warnings so tar_make() shows them; check_build.R prints the live season's table.
data_integrity <- function(season_data, teams_meta = NULL, n_ref = 5L, tol = 0.10) {
  seasons <- vapply(season_data, `[[`, integer(1), "season")
  live <- max(seasons)
  cut <- max(c(0L, concluded_weeks(season_data[[which(seasons == live)]]$games)))
  m <- dplyr::bind_rows(lapply(season_data, season_data_metrics, cut = cut, teams_meta = teams_meta))
  ref <- dplyr::bind_rows(lapply(seasons, function(s) {
    prev <- utils::tail(setdiff(seasons[seasons < s], 2020L), n_ref)
    if (!length(prev)) return(NULL)
    m |> dplyr::filter(season %in% prev, !is.na(value)) |>
      dplyr::group_by(metric) |>
      dplyr::summarise(ref_lo = min(value), ref_hi = max(value), n_ref = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(season = s)
  }))
  out <- m |>
    dplyr::left_join(ref, by = c("season", "metric")) |>
    dplyr::mutate(
      week = cut,
      level = dplyr::case_when(
        is.na(value) ~ "INFO",
        kind == "defect" ~ ifelse(value > 0, "FAIL", "PASS"),
        # the counts of a preseason cut (no games yet) say nothing
        kind == "count" & cut == 0L ~ "INFO",
        is.na(n_ref) | n_ref < 3 ~ "INFO",
        kind == "count" & (value > ref_hi * (1 + tol) | value < ref_lo * (1 - tol)) ~ "WARN",
        kind == "share" & value < ref_lo - 0.05 ~ "WARN",
        TRUE ~ "PASS")
    ) |>
    dplyr::select(season, week, metric, kind, value, ref_lo, ref_hi, level)
  bad <- out |> dplyr::filter(season == live, level %in% c("WARN", "FAIL"))
  for (i in seq_len(nrow(bad))) with(bad[i, ], warning(sprintf(
    "[data] %d through week %d: %s = %s (%s)", season, week, metric, signif(value, 3),
    if (kind == "defect") "should be 0" else sprintf("previous seasons %s-%s", signif(ref_lo, 3), signif(ref_hi, 3))),
    call. = FALSE))
  out
}

#' Score game predictions (pred_margin, pred_home_wp, and optionally pred_q -- predictive
#' quantiles -- for the CRPS) against results, one row per game
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
  if ("pred_q" %in% names(out)) {
    out <- out |> dplyr::mutate(crps = crps_quantiles(margin, pred_q)) |> dplyr::select(-pred_q)
  } else if (all(c("sigma", "nu") %in% names(out))) {
    out <- out |> dplyr::mutate(crps = crps_t(margin, pred_margin, sqrt(sigma^2 + dplyr::coalesce(pred_margin_sd, 0)^2), nu))
  }
  out
}
