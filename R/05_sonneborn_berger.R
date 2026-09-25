# ==============================================================================
# Module: R/05_sonneborn_berger.R
# Purpose: Derive Sonneborn-Berger, Buchholz, opponent-adjusted EPA, and the two
#          published orderings from posterior draws:
#            - Ranking (SOR):   past performance / strength of resume
#                               (WLD: theta, SB, Buchholz, PD: delta)
#            - Power Ranking:   predicted neutral-field strength going forward
#                               (XSOR = XPD: alpha + XWLD: omega, adjusted EPA/play, PD: delta)
# ==============================================================================

library(posterior)
library(dplyr)
library(tidyr)
library(stats)

#' Extract team ability parameter draws from fitted Stan models
#' @param fit Fitted CmdStanMCMC or stanfit object
#' @param param_prefix Parameter name: "theta" (WLD), "delta" (PD), "omega" (XWLD), or "alpha" (XPD)
#' @param N_teams Number of teams
#' @return Matrix of draws (rows = iterations, cols = teams)
extract_team_draws <- function(fit, param_prefix = "theta", N_teams) {
  # Pull only the requested vector (skips log_lik / *_rep) and subset with posterior's own
  # verb; base `[` on a draws_df strips its metadata and emits "Dropping 'draws_df' class"
  draws <- if (inherits(fit, "CmdStanMCMC")) {
    fit$draws(variables = param_prefix, format = "draws_matrix")
  } else {
    posterior::as_draws_matrix(fit)
  }
  cols <- sprintf("%s[%d]", param_prefix, 1:N_teams)
  existing_cols <- intersect(cols, posterior::variables(draws))

  if (length(existing_cols) < N_teams) {
    stop(sprintf("Could not find all %d parameters for %s in posterior draws.", N_teams, param_prefix))
  }

  sub <- posterior::subset_draws(draws, variable = cols)
  mat <- matrix(unclass(sub), nrow = posterior::ndraws(sub))
  colnames(mat) <- 1:N_teams
  return(mat)
}

#' Calculate game-level outcome points for each participant
#' @param wld_df WLD game outcomes tibble
#' @return Long-format tibble with team_id, opp_id, and points_earned (0 to 3)
expand_game_points <- function(wld_df) {
  # Home team perspective
  home_persp <- wld_df |>
    dplyr::transmute(
      game_id = game_id,
      week = week,
      team_id = home_id,
      team_name = home_team,
      opp_id = away_id,
      opp_name = away_team,
      is_home = TRUE,
      points_earned = dplyr::case_when(
        outcome == 4L ~ 3, # Regulation Win
        outcome == 3L ~ 2, # Overtime Win
        outcome == 2L ~ 1, # Overtime Loss
        outcome == 1L ~ 0  # Regulation Loss
      )
    )

  # Away team perspective
  away_persp <- wld_df |>
    dplyr::transmute(
      game_id = game_id,
      week = week,
      team_id = away_id,
      team_name = away_team,
      opp_id = home_id,
      opp_name = home_team,
      is_home = FALSE,
      points_earned = dplyr::case_when(
        outcome == 1L ~ 3, # Away Regulation Win
        outcome == 2L ~ 2, # Away Overtime Win
        outcome == 3L ~ 1, # Away Overtime Loss
        outcome == 4L ~ 0  # Away Regulation Loss
      )
    )

  dplyr::bind_rows(home_persp, away_persp) |>
    dplyr::arrange(team_id, week)
}

#' Compute Sonneborn-Berger and Buchholz ratings over posterior draws
#' @param fit_wld Fitted WLD Stan model
#' @param wld_df WLD game outcomes data frame
#' @param team_lookup Master team lookup table
#' @return Tibble containing team ratings with credible intervals
compute_sonneborn_berger <- function(fit_wld, wld_df, team_lookup) {
  message("[Sonneborn-Berger] Computing tie-break schedule equity over posterior draws...")

  N_teams <- nrow(team_lookup)
  theta_draws <- extract_team_draws(fit_wld, param_prefix = "theta", N_teams = N_teams)
  n_draws <- nrow(theta_draws)

  # Map latent log-odds into probability space M(theta) = plogis(theta)
  m_theta_draws <- stats::plogis(theta_draws)

  # Expand games into team-opponent pairs
  game_pairs <- expand_game_points(wld_df)

  # Pre-allocate SB and Buchholz matrices
  sb_draws <- matrix(0, nrow = n_draws, ncol = N_teams)
  bh_draws <- matrix(0, nrow = n_draws, ncol = N_teams)

  # Group games by team
  team_games_list <- split(game_pairs, game_pairs$team_id)

  for (t_str in names(team_games_list)) {
    t_id <- as.integer(t_str)
    tg <- team_games_list[[t_str]]
    opp_ids <- tg$opp_id
    weights <- tg$points_earned

    # Opponent quality draws for this team's schedule: [n_draws x n_opponents]
    opp_m_draws <- m_theta_draws[, opp_ids, drop = FALSE]

    # SB_i = sum_k w_k * M(theta_opp)
    sb_draws[, t_id] <- as.vector(opp_m_draws %*% weights)
    # BH_i = sum_k M(theta_opp)
    bh_draws[, t_id] <- as.vector(opp_m_draws %*% rep(1, length(opp_ids)))
  }

  # Summarize posterior distributions
  calc_quantiles <- function(mat) {
    tibble::tibble(
      median = apply(mat, 2, median),
      q05 = apply(mat, 2, quantile, probs = 0.05),
      q95 = apply(mat, 2, quantile, probs = 0.95),
      mean = apply(mat, 2, mean),
      sd = apply(mat, 2, sd)
    )
  }

  sb_summary <- calc_quantiles(sb_draws)
  bh_summary <- calc_quantiles(bh_draws)
  theta_summary <- calc_quantiles(theta_draws)

  ratings <- team_lookup |>
    dplyr::mutate(
      theta_median = theta_summary$median,
      theta_q05 = theta_summary$q05,
      theta_q95 = theta_summary$q95,
      sb_score = sb_summary$median,
      sb_q05 = sb_summary$q05,
      sb_q95 = sb_summary$q95,
      buchholz_sos = bh_summary$median,
      buchholz_q05 = bh_summary$q05,
      buchholz_q95 = bh_summary$q95
    )

  return(ratings)
}

#' Opponent- and venue-adjusted net EPA per play (ridge-regularized paired comparison)
#'
#' Fits  net_epa_pp[k] = r[home] - r[away] + b_fcs * (fcs[home] - fcs[away]) + hfa * (1 - neutral[k])
#' by penalized least squares. Team effects are ridge-shrunk toward their subdivision mean
#' (FBS anchored at 0, FCS at b_fcs), which keeps sparse FBS-FCS links from inflating ratings.
#' The resulting rating is a neutral-field net EPA/play: rating_A - rating_B is the expected
#' per-play EPA edge of A over B on a neutral field.
#' @param epa_df Game-level tibble from build_epa_dataset
#' @param team_lookup Master team lookup table
#' @param ridge_lambda Ridge penalty on team effects (in games-equivalent units)
#' @return Tibble keyed by team_id with raw offense/defense EPA/play and adjusted net EPA/play
compute_adjusted_epa <- function(epa_df, team_lookup, ridge_lambda = 2) {
  message("[EPA] Fitting opponent-adjusted net EPA/play ratings...")
  N_teams <- nrow(team_lookup)

  out <- tibble::tibble(
    team_id = team_lookup$team_id,
    epa_off_pp = NA_real_,
    epa_def_pp = NA_real_,
    epa_net_pp = NA_real_,
    epa_adj = NA_real_,
    epa_games = 0L,
    plays_per_game = NA_real_
  )
  if (is.null(epa_df) || nrow(epa_df) == 0) return(out)

  # Raw (unadjusted) play-weighted offense/defense EPA per play
  long <- dplyr::bind_rows(
    epa_df |> dplyr::transmute(team_id = home_id, off_epa = home_epa_pp * home_plays, off_plays = home_plays,
                               def_epa = away_epa_pp * away_plays, def_plays = away_plays),
    epa_df |> dplyr::transmute(team_id = away_id, off_epa = away_epa_pp * away_plays, off_plays = away_plays,
                               def_epa = home_epa_pp * home_plays, def_plays = home_plays)
  ) |>
    dplyr::group_by(team_id) |>
    dplyr::summarize(
      epa_off_pp = sum(off_epa) / sum(off_plays),
      epa_def_pp = sum(def_epa) / sum(def_plays),   # EPA/play allowed (lower is better)
      epa_games = dplyr::n(),
      plays_per_game = sum(off_plays) / dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(epa_net_pp = epa_off_pp - epa_def_pp)

  # Ridge paired-comparison design
  G <- nrow(epa_df)
  X <- matrix(0, nrow = G, ncol = N_teams)
  X[cbind(seq_len(G), epa_df$home_id)] <- 1
  X[cbind(seq_len(G), epa_df$away_id)] <- -1
  is_fcs <- as.numeric(team_lookup$subdivision_id == 2L)
  Z <- cbind(X, is_fcs[epa_df$home_id] - is_fcs[epa_df$away_id], 1 - epa_df$neutral_site)
  # Tiny penalty on the FCS/HFA columns only guards against singularity (e.g. no FCS games yet)
  penalty <- diag(c(rep(ridge_lambda, N_teams), 1e-6, 1e-6))
  beta <- as.vector(solve(crossprod(Z) + penalty, crossprod(Z, epa_df$net_epa_pp)))

  adj <- beta[seq_len(N_teams)] + beta[N_teams + 1] * is_fcs

  out |>
    dplyr::select(team_id) |>
    dplyr::left_join(long, by = "team_id") |>
    dplyr::mutate(
      epa_adj = adj[team_id],
      epa_games = dplyr::coalesce(epa_games, 0L)
    )
}

#' Compute the SOR Ranking (past performance) and Power Ranking (future, neutral field)
#'
#' Ranking (SOR): what a team has earned. Phi-blend of WLD resume (theta), Sonneborn-Berger
#'   quality wins, realized point margin (delta), and Buchholz schedule strength, plus a
#'   small head-to-head credit for beating good teams.
#' Power Ranking: how good a team is right now on a neutral field. Blend of standardized
#'   XPD drive efficiency (alpha), opponent-adjusted net EPA/play, XWLD expected outcomes
#'   (omega), and realized margin (delta). Blended in z-space (not Phi) so the tails keep
#'   their spacing, then expressed in points as `power_rating`: the neutral-field spread
#'   between A and B is power_rating[A] - power_rating[B].
#'
#' @param sb_ratings Tibble from compute_sonneborn_berger (contains theta, SB, Buchholz)
#' @param fit_pd Fitted PD Stan model (or NULL)
#' @param fit_xwld Fitted XWLD Stan model (or NULL)
#' @param fit_xpd Fitted XPD Stan model (or NULL)
#' @param team_lookup Master team lookup table
#' @param wld_df Optional game-level WLD tibble to apply direct head-to-head victory equity
#' @param epa_ratings Optional tibble from compute_adjusted_epa (falls back to alpha if NULL)
#' @param sor_weights Named weights c(wld, sb, pd, sos) for the SOR Ranking
#' @param power_weights Named weights c(xpd, epa, xwld, pd) for the Power Ranking
#' @return Ranked tibble with SOR and Power orderings, all pipeline ratings, and divergence metrics
compute_composite_rankings <- function(sb_ratings,
                                       fit_pd = NULL,
                                       fit_xwld = NULL,
                                       fit_xpd = NULL,
                                       team_lookup,
                                       wld_df = NULL,
                                       epa_ratings = NULL,
                                       sor_weights = c(wld = 0.40, sb = 0.30, pd = 0.20, sos = 0.10),
                                       power_weights = c(xpd = 0.40, epa = 0.25, xwld = 0.20, pd = 0.15)) {
  message("[Rankings] Building SOR Ranking (WLD, SB, PD, SOS) and Power Ranking (XPD, EPA, XWLD, PD)...")

  N_teams <- nrow(team_lookup)
  combined <- sb_ratings

  # 1. Pipeline 2: PD (Point Differential: delta)
  if (!is.null(fit_pd)) {
    delta_draws <- extract_team_draws(fit_pd, param_prefix = "delta", N_teams = N_teams)
    combined <- combined |>
      dplyr::mutate(
        delta_median = apply(delta_draws, 2, median),
        delta_q05 = apply(delta_draws, 2, quantile, probs = 0.05),
        delta_q95 = apply(delta_draws, 2, quantile, probs = 0.95)
      )
  } else {
    combined <- combined |>
      dplyr::mutate(
        delta_median = theta_median * 7.0, # Scaled fallback
        delta_q05 = theta_q05 * 7.0,
        delta_q95 = theta_q95 * 7.0
      )
  }

  # 2. Pipeline 3: XWLD (Expected Win/Loss/OT: omega)
  if (!is.null(fit_xwld)) {
    omega_draws <- extract_team_draws(fit_xwld, param_prefix = "omega", N_teams = N_teams)
    combined <- combined |>
      dplyr::mutate(
        omega_median = apply(omega_draws, 2, median),
        omega_q05 = apply(omega_draws, 2, quantile, probs = 0.05),
        omega_q95 = apply(omega_draws, 2, quantile, probs = 0.95)
      )
  } else {
    combined <- combined |>
      dplyr::mutate(
        omega_median = theta_median,
        omega_q05 = theta_q05,
        omega_q95 = theta_q95
      )
  }

  # 3. Pipeline 4: XPD (Expected Point Differential EPA: alpha)
  if (!is.null(fit_xpd)) {
    alpha_draws <- extract_team_draws(fit_xpd, param_prefix = "alpha", N_teams = N_teams)
    combined <- combined |>
      dplyr::mutate(
        alpha_median = apply(alpha_draws, 2, median),
        alpha_q05 = apply(alpha_draws, 2, quantile, probs = 0.05),
        alpha_q95 = apply(alpha_draws, 2, quantile, probs = 0.95)
      )
  } else {
    combined <- combined |>
      dplyr::mutate(
        alpha_median = delta_median,
        alpha_q05 = delta_q05,
        alpha_q95 = delta_q95
      )
  }

  # 4. Opponent-adjusted EPA/play (not a Stan pipeline; ridge point estimate)
  if (!is.null(epa_ratings) && nrow(epa_ratings) > 0) {
    combined <- combined |>
      dplyr::left_join(epa_ratings, by = "team_id")
  } else {
    combined <- combined |>
      dplyr::mutate(
        epa_off_pp = NA_real_, epa_def_pp = NA_real_, epa_net_pp = NA_real_,
        epa_adj = NA_real_, epa_games = 0L, plays_per_game = NA_real_
      )
  }

  # Normalize weights to sum to 1.0 within each ordering
  sor_weights <- sor_weights / sum(sor_weights)
  power_weights <- power_weights / sum(power_weights)
  w_wld <- sor_weights[["wld"]]
  w_sb  <- sor_weights[["sb"]]
  w_pd  <- sor_weights[["pd"]]
  w_sos <- sor_weights[["sos"]]
  pw_xpd  <- power_weights[["xpd"]]
  pw_epa  <- power_weights[["epa"]]
  pw_xwld <- power_weights[["xwld"]]
  pw_pd   <- power_weights[["pd"]]

  # Standardize all latent metrics across FBS programs
  fbs_sub <- combined |> dplyr::filter(subdivision_id == 1L)
  z_ref <- function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    c(m = m, s = if (is.na(s) || s == 0) 1.0 else s)
  }
  theta_ref <- z_ref(fbs_sub$theta_median)
  delta_ref <- z_ref(fbs_sub$delta_median)
  omega_ref <- z_ref(fbs_sub$omega_median)
  alpha_ref <- z_ref(fbs_sub$alpha_median)
  sb_ref    <- z_ref(fbs_sub$sb_score)
  bh_ref    <- z_ref(fbs_sub$buchholz_sos)
  epa_ref   <- z_ref(fbs_sub$epa_adj)

  final_rankings <- combined |>
    dplyr::mutate(
      z_theta = (theta_median - theta_ref[["m"]]) / theta_ref[["s"]],
      z_delta = (delta_median - delta_ref[["m"]]) / delta_ref[["s"]],
      z_omega = (omega_median - omega_ref[["m"]]) / omega_ref[["s"]],
      z_alpha = (alpha_median - alpha_ref[["m"]]) / alpha_ref[["s"]],
      z_sb    = (sb_score - sb_ref[["m"]]) / sb_ref[["s"]],
      z_sos   = (buchholz_sos - bh_ref[["m"]]) / bh_ref[["s"]],
      # Teams without usable play-by-play fall back to their XPD efficiency
      z_epa   = dplyr::coalesce((epa_adj - epa_ref[["m"]]) / epa_ref[["s"]], z_alpha),

      phi_theta = stats::pnorm(z_theta),
      phi_delta = stats::pnorm(z_delta),
      phi_omega = stats::pnorm(z_omega),
      phi_alpha = stats::pnorm(z_alpha),
      phi_sb    = stats::pnorm(z_sb),
      phi_sos   = stats::pnorm(z_sos),
      phi_epa   = stats::pnorm(z_epa),

      # RANKING — Strength of Resume (SOR): actual on-field accomplishment against schedule
      sor_index = w_wld * phi_theta + w_sb * phi_sb + w_pd * phi_delta + w_sos * phi_sos,

      # Expected Strength of Resume (xSOR): the XPD + XWLD sub-blend, kept for resume-luck diagnostics
      xsor_index = (pw_xpd * phi_alpha + pw_xwld * phi_omega) / (pw_xpd + pw_xwld),

      # POWER RANKING — neutral-field strength (linear in z so elite margins are not compressed)
      power_z = pw_xpd * z_alpha + pw_epa * z_epa + pw_xwld * z_omega + pw_pd * z_delta
    )

  # Direct Head-to-Head Victory Equity (SOR only): beating quality opponents adds tiebreak equity
  if (!is.null(wld_df) && nrow(wld_df) > 0) {
    theta_lookup <- setNames(final_rankings$phi_theta, final_rankings$team_id)
    h2h_credit <- numeric(nrow(final_rankings))

    # 1=Away Win, 2=Away OT Win, 3=Home Win, 4=Home OT Win
    home_w <- wld_df[wld_df$outcome %in% c(3L, 4L), ]
    away_w <- wld_df[wld_df$outcome %in% c(1L, 2L), ]

    if (nrow(home_w) > 0) {
      for (k in seq_len(nrow(home_w))) {
        w_id <- home_w$home_id[k]
        l_id <- home_w$away_id[k]
        opp_qual <- if (as.character(l_id) %in% names(theta_lookup)) theta_lookup[[as.character(l_id)]] else 0.5
        h2h_credit[w_id] <- h2h_credit[w_id] + 0.015 * opp_qual
      }
    }
    if (nrow(away_w) > 0) {
      for (k in seq_len(nrow(away_w))) {
        w_id <- away_w$away_id[k]
        l_id <- away_w$home_id[k]
        opp_qual <- if (as.character(l_id) %in% names(theta_lookup)) theta_lookup[[as.character(l_id)]] else 0.5
        h2h_credit[w_id] <- h2h_credit[w_id] + 0.020 * opp_qual # Higher reward for road victory
      }
    }
    final_rankings$sor_index <- final_rankings$sor_index + h2h_credit
  }

  # Express the power blend in points: rescale to unit FBS sd, then onto the spread of the
  # realized-margin (delta) ratings, so rating differences read as neutral-field point spreads
  pz_fbs <- final_rankings$power_z[final_rankings$subdivision_id == 1L]
  pz_ref <- z_ref(pz_fbs)
  final_rankings <- final_rankings |>
    dplyr::mutate(
      power_z = (power_z - pz_ref[["m"]]) / pz_ref[["s"]],
      power_index = stats::pnorm(power_z),
      power_rating = power_z * delta_ref[["s"]]
    )

  final_rankings <- final_rankings |>
    dplyr::mutate(
      rank_sor_overall = dplyr::row_number(dplyr::desc(sor_index)),
      rank_power_overall = dplyr::row_number(dplyr::desc(power_z))
    ) |>
    dplyr::group_by(subdivision_id) |>
    dplyr::mutate(
      rank_sor = dplyr::row_number(dplyr::desc(sor_index)),
      rank_power = dplyr::row_number(dplyr::desc(power_z)),
      rank_xsor = dplyr::min_rank(desc(xsor_index)),
      rank_wld = dplyr::min_rank(desc(theta_median)),
      rank_sb = dplyr::min_rank(desc(sb_score)),
      rank_pd = dplyr::min_rank(desc(delta_median)),
      rank_xwld = dplyr::min_rank(desc(omega_median)),
      rank_xpd = dplyr::min_rank(desc(alpha_median)),
      rank_epa = dplyr::min_rank(desc(dplyr::coalesce(epa_adj, -Inf))),
      rank_buchholz = dplyr::min_rank(desc(buchholz_sos)),

      # Cross-Pipeline Divergence Metrics
      power_gap = rank_power - rank_sor,          # Positive = Resume (Ranking) outranks current strength (Power)
      resume_luck = rank_xsor - rank_sor,         # Positive = Actual resume outranks underlying play efficiency
      outcome_luck = rank_xwld - rank_wld,        # Positive = Team won more games than expected from play efficiency
      margin_luck = rank_xpd - rank_pd,          # Positive = Scoreboard margin outperformed expected EPA margin
      finish_factor = rank_pd - rank_wld,        # Positive = Clutch record outperforms point differential
      conversion_factor = rank_xpd - rank_xwld   # Positive = EPA translates strongly into expected wins
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(subdivision_id, rank_sor)

  return(final_rankings)
}

#' Posterior median home-field advantage (points) from the PD model, for projected scores
#' @param fit_pd Fitted PD Stan model
#' @return Scalar home-field edge in points (on the same scale as delta / power_rating)
extract_home_field_pts <- function(fit_pd) {
  draws <- if (inherits(fit_pd, "CmdStanMCMC")) {
    fit_pd$draws(variables = "beta_home", format = "draws_matrix")
  } else {
    posterior::subset_draws(posterior::as_draws_matrix(fit_pd), variable = "beta_home")
  }
  stats::median(as.numeric(unclass(draws)))
}
