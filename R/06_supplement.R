# ==============================================================================
# R/06_supplement.R
# Full-draw refits of two representative posteriors for the paper's supplementary
# model checks (paper/supplement.qmd). The weekly snapshots keep only summaries,
# so trace, rank, energy, pairs and LOO-PIT checks need draws kept on purpose.
#   first : week 1 of the season (about one game per team; the prior dominates)
#   final : the last concluded week (a full season of data)
# ==============================================================================

supplement_vars <- c("hfa", "b_prev", "lambda", "kappa", "sigma_eff", "sigma_m", "nu",
                     "sigma_conf", "sigma_team[1]", "sigma_team[2]", "sigma_team[3]",
                     "mu_tier[2]", "mu_tier[3]")

#' Refit week 1 and the final week of one season, keeping everything the checks need
run_supplement <- function(inputs, cfg = model_config(), seed = 2026) {
  weeks <- vapply(inputs, `[[`, integer(1), "week")
  picks <- list(first = inputs[[which(weeks == 1L)[1]]], final = inputs[[length(inputs)]])
  lapply(picks, supplement_fit, cfg = cfg, seed = seed)
}

#' One full-draw fit: draws, sampler diagnostics, PSIS-LOO with LOO-PIT, replicates,
#' and a power-scaling sequence. CmdStan CSVs are deleted afterwards.
supplement_fit <- function(input, cfg = model_config(), seed = 2026, n_rep = 100L) {
  f <- fit_power(input, cfg, compute_ll = TRUE, seed = seed)
  fit <- f$fit
  played <- input$played
  N <- nrow(played)

  par <- fit$draws(variables = supplement_vars)
  r <- fit$draws(variables = "r")
  conf_raw <- fit$draws(variables = "conf_raw")
  z <- fit$draws(variables = "z")
  diag <- fit$sampler_diagnostics()

  # PSIS-LOO on the joint (efficiency, margin) log-likelihood of each game
  l <- fit$loo(variables = "log_lik", r_eff = TRUE, save_psis = TRUE)
  w <- stats::weights(l$psis_object, log = FALSE, normalize = TRUE)   # S x N

  rm_ <- posterior::as_draws_matrix(r)
  pm <- posterior::as_draws_df(par)
  S <- nrow(rm_)
  mu <- rm_[, played$home, drop = FALSE] - rm_[, played$away, drop = FALSE] +
    pm$hfa * matrix(played$home_adv, S, N, byrow = TRUE)
  sig_marg <- sqrt(pm$sigma_m^2 + pm$kappa^2 * pm$sigma_eff^2)
  # LOO-PIT: leave-one-out predictive CDF at the observed value, from PSIS weights
  y_mat <- matrix(played$margin, S, N, byrow = TRUE)
  e_mat <- matrix(played$eff, S, N, byrow = TRUE)
  pit_margin <- colSums(w * stats::pt((y_mat - mu) / sig_marg, df = pm$nu))
  pit_eff <- colSums(w * stats::pnorm((e_mat - pm$lambda * mu) / pm$sigma_eff))
  pit_eff[!played$has_eff] <- NA_real_

  # Replicated regulation margins (marginal law, rounded) and efficiency margins
  set.seed(seed)
  pick <- sort(sample.int(S, n_rep))
  mu_p <- mu[pick, , drop = FALSE]
  yrep <- round(mu_p + matrix(stats::rt(n_rep * N, df = rep(pm$nu[pick], N)), n_rep) * sig_marg[pick])
  erep <- pm$lambda[pick] * mu_p + matrix(stats::rnorm(n_rep * N), n_rep) * pm$sigma_eff[pick]

  ps <- tryCatch(
    priorsense::powerscale_sequence(
      fit$draws(variables = c(supplement_vars, "lprior", "log_lik")),
      variable = supplement_vars
    ),
    error = function(e) NULL
  )

  out <- list(
    season = input$season, week = input$week, week_label = input$week_label,
    health = f$health, time = fit$time(),
    metadata = fit$metadata()[c("max_treedepth", "adapt_delta", "iter_warmup", "iter_sampling")],
    par = par, r = r, conf_raw = conf_raw, z = z, diag = diag,
    loo = l[c("estimates", "pointwise", "diagnostics")],
    pit_margin = pit_margin, pit_eff = pit_eff,
    mu_mean = colMeans(mu), yrep = yrep, erep = erep,
    played = played, teams = input$teams, conf_tier = input$stan_data$conf_tier,
    powerscale = ps
  )
  unlink(fit$output_files())
  out
}

# ---- Prior perturbation ------------------------------------------------------

#' A copy of cfg with every structural prior widened (sd * sd_mult) and/or shifted
#' (mean + shift * sd, before widening). The nu prior is fixed in the Stan program.
perturb_priors <- function(cfg, sd_mult = 1, shift = 0) {
  pairs <- list(c("hfa_mean", "hfa_sd"), c("tier_mean", "tier_sd"), c("b_prev_mean", "b_prev_sd"),
                c("sigma_team_mean", "sigma_team_sd"))
  for (p in pairs) {
    cfg[[p[1]]] <- cfg[[p[1]]] + shift * cfg[[p[2]]]
    cfg[[p[2]]] <- cfg[[p[2]]] * sd_mult
  }
  for (p in c("sigma_conf_prior", "lambda_prior", "kappa_prior", "sigma_eff_prior", "sigma_m_prior")) {
    cfg[[p]][1] <- cfg[[p]][1] + shift * cfg[[p]][2]
    cfg[[p]][2] <- cfg[[p]][2] * sd_mult
  }
  # b_prev lives in (0, 1)
  cfg$b_prev_mean <- min(max(cfg$b_prev_mean, 0.05), 0.95)
  cfg
}

prior_scenarios <- function() {
  tibble::tribble(
    ~scenario,          ~sd_mult, ~shift,
    "Baseline",          1,        0,
    "SD x 2",            2,        0,
    "SD x 5",            5,        0,
    "Means + 1 SD",      1,        1,
    "Means - 1 SD",      1,       -1
  )
}

#' Refit week 1 and the final week under each prior scenario and summarise the published
#' products (power, WAB, ranks) as well as the structural parameters.
run_prior_sensitivity <- function(inputs, cfg = model_config(), seed = 2026) {
  weeks <- vapply(inputs, `[[`, integer(1), "week")
  picks <- list(first = inputs[[which(weeks == 1L)[1]]], final = inputs[[length(inputs)]])
  sc <- prior_scenarios()
  out <- list()
  for (nm in names(picks)) {
    input <- picks[[nm]]
    for (i in seq_len(nrow(sc))) {
      cfg_i <- perturb_priors(cfg, sc$sd_mult[i], sc$shift[i])
      input$stan_data <- utils::modifyList(input$stan_data, list(
        hfa_mean = cfg_i$hfa_mean, hfa_sd = cfg_i$hfa_sd, tier_mean = cfg_i$tier_mean, tier_sd = cfg_i$tier_sd,
        b_prev_mean = cfg_i$b_prev_mean, b_prev_sd = cfg_i$b_prev_sd,
        sigma_team_mean = cfg_i$sigma_team_mean, sigma_team_sd = cfg_i$sigma_team_sd,
        sigma_conf_prior = cfg_i$sigma_conf_prior, lambda_prior = cfg_i$lambda_prior,
        kappa_prior = cfg_i$kappa_prior, sigma_eff_prior = cfg_i$sigma_eff_prior,
        sigma_m_prior = cfg_i$sigma_m_prior))
      f <- fit_power(input, cfg_i, seed = seed)
      par <- posterior::summarise_draws(f$fit$draws(variables = supplement_vars), "mean", "sd")
      draws <- extract_draws(f$fit)
      unlink(f$fit$output_files())
      snap <- summarise_snapshot(input, draws, f$health, cfg_i)
      out[[length(out) + 1]] <- list(
        fit = nm, scenario = sc$scenario[i], health = f$health, par = par,
        ratings = snap$ratings |>
          dplyr::select(team_id, team, tier, power, power_sd, power_rank, wab, resume_rank)
      )
    }
  }
  out
}

# ---- Induced priors ----------------------------------------------------------

#' Prior predictive simulation: draw every structural parameter from its prior, build team
#' strengths through the hierarchy (with the season's carry-over d_i), and record the
#' quantities the priors induce on things we have intuition about.
induced_priors <- function(input, cfg = model_config(), n = 4000L, seed = 1) {
  set.seed(seed)
  rtn <- function(n, m, s, lo = -Inf, hi = Inf) {
    u <- stats::runif(n, stats::pnorm(lo, m, s), stats::pnorm(hi, m, s))
    stats::qnorm(u, m, s)
  }
  rgam_trunc <- function(n) stats::qgamma(stats::runif(n, stats::pgamma(1, 2, 0.1), 1), 2, 0.1)
  sd_ <- input$stan_data
  tm <- input$teams
  confs <- sd_$conf_tier
  games <- input$played
  fbs <- tm$tier == 1L
  res <- vector("list", n)
  for (s in seq_len(n)) {
    hfa <- rtn(1, cfg$hfa_mean, cfg$hfa_sd)
    mt <- c(0, rtn(1, cfg$tier_mean[1], cfg$tier_sd[1]), rtn(1, cfg$tier_mean[2], cfg$tier_sd[2]))
    b <- rtn(1, cfg$b_prev_mean, cfg$b_prev_sd, 0, 1)
    st <- vapply(1:3, \(k) rtn(1, cfg$sigma_team_mean[k], cfg$sigma_team_sd[k], 0), numeric(1))
    om <- rtn(1, cfg$sigma_conf_prior[1], cfg$sigma_conf_prior[2], 0)
    lam <- rtn(1, cfg$lambda_prior[1], cfg$lambda_prior[2], 0)
    kap <- stats::rnorm(1, cfg$kappa_prior[1], cfg$kappa_prior[2])
    se <- rtn(1, cfg$sigma_eff_prior[1], cfg$sigma_eff_prior[2], 0)
    sm <- rtn(1, cfg$sigma_m_prior[1], cfg$sigma_m_prior[2], 0)
    nu <- rgam_trunc(1)
    alpha <- mt[confs] + om * stats::rnorm(length(confs))
    r <- alpha[tm$conf_idx] + b * sd_$prev_dev + st[tm$tier] * stats::rnorm(nrow(tm))
    r <- r - mean(r[fbs])
    sig <- sqrt(sm^2 + kap^2 * se^2)
    rf <- unname(sort(r[fbs], decreasing = TRUE))
    mu <- r[games$home] - r[games$away] + hfa * games$home_adv
    y <- mu + stats::rt(length(mu), nu) * sig
    fbs_g <- tm$tier[games$home] == 1L & tm$tier[games$away] == 1L
    res[[s]] <- c(
      hfa = hfa, sigma_marg = sig, r2 = kap^2 * se^2 / sig^2, nu = nu,
      sd_fbs = stats::sd(r[fbs]), top_minus_12 = rf[1] - rf[12], bench = rf[12],
      p_home_even = stats::pt(hfa / sig, nu),
      p_top_vs_avg = stats::pt(rf[1] / sig, nu),
      sd_margin = stats::sd(y), within7 = mean(abs(round(y)) <= 7),
      home_win = mean(y[games$home_adv == 1] > 0),
      sd_margin_fbs = stats::sd(y[fbs_g]), fcs_gap = mean(r[tm$tier == 2L]) - 0
    )
  }
  list(draws = tibble::as_tibble(do.call(rbind, res)),
       observed = c(sd_margin = stats::sd(games$margin),
                    within7 = mean(abs(games$margin) <= 7),
                    home_win = mean(games$margin[games$home_adv == 1] > 0)))
}
