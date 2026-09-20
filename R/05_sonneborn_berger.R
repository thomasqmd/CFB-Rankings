# ==============================================================================
# Module: R/05_sonneborn_berger.R
# Purpose: Derive Sonneborn-Berger, Buchholz, and 4-Pipeline Composite Resumes from Posterior Draws
#          (WLD: theta, PD: delta, XWLD: omega, XPD: alpha)
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
  draws_df <- posterior::as_draws_df(fit)
  cols <- sprintf("%s[%d]", param_prefix, 1:N_teams)
  existing_cols <- intersect(cols, names(draws_df))
  
  if (length(existing_cols) < N_teams) {
    stop(sprintf("Could not find all %d parameters for %s in posterior draws.", N_teams, param_prefix))
  }
  
  mat <- as.matrix(draws_df[, cols])
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

#' Compute 4-Pipeline Composite Rankings fusing WLD, PD, XWLD, and XPD
#' @param sb_ratings Tibble from compute_sonneborn_berger (contains theta, SB, Buchholz)
#' @param fit_pd Fitted PD Stan model (or NULL)
#' @param fit_xwld Fitted XWLD Stan model (or NULL)
#' @param fit_xpd Fitted XPD Stan model (or NULL)
#' @param team_lookup Master team lookup table
#' @param weights Named numeric vector of weights for c(wld, pd, xwld, xpd)
#' @param lambda Optional legacy weighting parameter (if passed, sets wld/xpd balance)
#' @return Comprehensive ranked tibble with all 4 pipelines and cross-pipeline divergence metrics
compute_composite_rankings <- function(sb_ratings, 
                                       fit_pd = NULL, 
                                       fit_xwld = NULL, 
                                       fit_xpd = NULL, 
                                       team_lookup, 
                                       weights = c(wld = 0.35, pd = 0.25, xwld = 0.15, xpd = 0.25),
                                       lambda = NULL) {
  message("[Composite Index] Fusing 4 pipelines: WLD, PD, XWLD, and XPD...")
  
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
  
  # Handle legacy lambda parameter if explicitly passed
  if (!is.null(lambda)) {
    # lambda controls Actual (WLD + PD) vs Expected (XWLD + XPD)
    actual_wt <- lambda
    exp_wt <- 1 - lambda
    weights <- c(
      wld = actual_wt * 0.60,
      pd = actual_wt * 0.40,
      xwld = exp_wt * 0.40,
      xpd = exp_wt * 0.60
    )
  }
  
  # Normalize weights to sum to 1.0
  weights <- weights / sum(weights)
  w_wld <- weights[["wld"]]
  w_pd <- weights[["pd"]]
  w_xwld <- weights[["xwld"]]
  w_xpd <- weights[["xpd"]]
  
  # Standardize all 4 latent metrics across FBS programs
  fbs_sub <- combined |> dplyr::filter(subdivision_id == 1L)
  
  theta_m <- mean(fbs_sub$theta_median, na.rm = TRUE)
  theta_s <- sd(fbs_sub$theta_median, na.rm = TRUE)
  
  delta_m <- mean(fbs_sub$delta_median, na.rm = TRUE)
  delta_s <- sd(fbs_sub$delta_median, na.rm = TRUE)
  
  omega_m <- mean(fbs_sub$omega_median, na.rm = TRUE)
  omega_s <- sd(fbs_sub$omega_median, na.rm = TRUE)
  
  alpha_m <- mean(fbs_sub$alpha_median, na.rm = TRUE)
  alpha_s <- sd(fbs_sub$alpha_median, na.rm = TRUE)
  
  final_rankings <- combined |>
    dplyr::mutate(
      # Standard normal CDF transformation Phi((x - mu) / sd)
      z_theta = (theta_median - theta_m) / theta_s,
      z_delta = (delta_median - delta_m) / delta_s,
      z_omega = (omega_median - omega_m) / omega_s,
      z_alpha = (alpha_median - alpha_m) / alpha_s,
      
      phi_theta = stats::pnorm(z_theta),
      phi_delta = stats::pnorm(z_delta),
      phi_omega = stats::pnorm(z_omega),
      phi_alpha = stats::pnorm(z_alpha),
      
      # 4-Pipeline Composite Index
      composite_index = (w_wld * phi_theta) + 
                        (w_pd * phi_delta) + 
                        (w_xwld * phi_omega) + 
                        (w_xpd * phi_alpha)
    ) |>
    dplyr::arrange(desc(composite_index)) |>
    dplyr::mutate(
      rank_composite_overall = dplyr::row_number()
    ) |>
    dplyr::group_by(subdivision_id) |>
    dplyr::mutate(
      rank_composite = dplyr::row_number(),
      rank_wld = dplyr::min_rank(desc(theta_median)),
      rank_pd = dplyr::min_rank(desc(delta_median)),
      rank_xwld = dplyr::min_rank(desc(omega_median)),
      rank_xpd = dplyr::min_rank(desc(alpha_median)),
      rank_sb = dplyr::min_rank(desc(sb_score)),
      rank_buchholz = dplyr::min_rank(desc(buchholz_sos)),
      
      # Cross-Pipeline Divergence Metrics (Positive = Actual outperforming Expected)
      outcome_luck = rank_xwld - rank_wld,        # Positive = Team won more games than expected from play efficiency
      margin_luck = rank_xpd - rank_pd,          # Positive = Scoreboard margin outperformed expected EPA margin
      finish_factor = rank_pd - rank_wld,        # Positive = Clutch record outperforms point differential
      conversion_factor = rank_xpd - rank_xwld   # Positive = EPA translates strongly into expected wins
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(subdivision_id, rank_composite)
  
  return(final_rankings)
}
