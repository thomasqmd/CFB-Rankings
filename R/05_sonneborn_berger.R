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
#' @param wld_df Optional game-level WLD tibble to apply direct head-to-head victory equity
#' @param weights Named numeric vector of weights for c(wld, sb, pd, sos, xpd, xwld)
#' @param lambda Optional legacy weighting parameter (if passed, sets wld/xpd balance)
#' @return Comprehensive ranked tibble with all 4 pipelines and cross-pipeline divergence metrics
compute_composite_rankings <- function(sb_ratings, 
                                       fit_pd = NULL, 
                                       fit_xwld = NULL, 
                                       fit_xpd = NULL, 
                                       team_lookup, 
                                       wld_df = NULL,
                                       weights = c(wld = 0.20, sb = 0.15, pd = 0.15, sos = 0.05, xpd = 0.30, xwld = 0.15),
                                       lambda = NULL) {
  message("[Composite Index] Fusing WLD, SB (Opponent Strength), XPD, PD, XWLD, and SOS...")
  
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
    actual_wt <- lambda
    exp_wt <- 1 - lambda
    weights <- c(
      wld = actual_wt * 0.40,
      sb = actual_wt * 0.35,
      sos = actual_wt * 0.10,
      pd = actual_wt * 0.15,
      xwld = exp_wt * 0.35,
      xpd = exp_wt * 0.65
    )
  }
  
  # If sb or sos not explicitly provided in weights, dynamically allocate from wld
  if (!all(c("sb", "sos") %in% names(weights))) {
    w_wld_total <- if ("wld" %in% names(weights)) weights[["wld"]] else 0.35
    w_sb <- w_wld_total * 0.35
    w_sos <- w_wld_total * 0.15
    w_wld <- w_wld_total * 0.50
    w_xpd <- if ("xpd" %in% names(weights)) weights[["xpd"]] else 0.25
    w_pd <- if ("pd" %in% names(weights)) weights[["pd"]] else 0.18
    w_xwld <- if ("xwld" %in% names(weights)) weights[["xwld"]] else 0.12
    weights <- c(wld = w_wld, sb = w_sb, xpd = w_xpd, pd = w_pd, xwld = w_xwld, sos = w_sos)
  }
  
  # Normalize weights to sum to 1.0
  weights <- weights / sum(weights)
  w_wld  <- weights[["wld"]]
  w_sb   <- weights[["sb"]]
  w_sos  <- weights[["sos"]]
  w_xpd  <- weights[["xpd"]]
  w_pd   <- weights[["pd"]]
  w_xwld <- weights[["xwld"]]
  
  # Standardize all latent metrics across FBS programs
  fbs_sub <- combined |> dplyr::filter(subdivision_id == 1L)
  
  theta_m <- mean(fbs_sub$theta_median, na.rm = TRUE)
  theta_s <- sd(fbs_sub$theta_median, na.rm = TRUE)
  
  delta_m <- mean(fbs_sub$delta_median, na.rm = TRUE)
  delta_s <- sd(fbs_sub$delta_median, na.rm = TRUE)
  
  omega_m <- mean(fbs_sub$omega_median, na.rm = TRUE)
  omega_s <- sd(fbs_sub$omega_median, na.rm = TRUE)
  
  alpha_m <- mean(fbs_sub$alpha_median, na.rm = TRUE)
  alpha_s <- sd(fbs_sub$alpha_median, na.rm = TRUE)
  
  sb_m <- mean(fbs_sub$sb_score, na.rm = TRUE)
  sb_s <- sd(fbs_sub$sb_score, na.rm = TRUE)
  sb_s <- if (is.na(sb_s) || sb_s == 0) 1.0 else sb_s
  
  bh_m <- mean(fbs_sub$buchholz_sos, na.rm = TRUE)
  bh_s <- sd(fbs_sub$buchholz_sos, na.rm = TRUE)
  bh_s <- if (is.na(bh_s) || bh_s == 0) 1.0 else bh_s
  
  final_rankings <- combined |>
    dplyr::mutate(
      # Standard normal CDF transformation Phi((x - mu) / sd)
      z_theta = (theta_median - theta_m) / theta_s,
      z_delta = (delta_median - delta_m) / delta_s,
      z_omega = (omega_median - omega_m) / omega_s,
      z_alpha = (alpha_median - alpha_m) / alpha_s,
      z_sb    = (sb_score - sb_m) / sb_s,
      z_sos   = (buchholz_sos - bh_m) / bh_s,
      
      phi_theta = stats::pnorm(z_theta),
      phi_delta = stats::pnorm(z_delta),
      phi_omega = stats::pnorm(z_omega),
      phi_alpha = stats::pnorm(z_alpha),
      phi_sb    = stats::pnorm(z_sb),
      phi_sos   = stats::pnorm(z_sos),
      
      # 1. Strength of Resume (SOR): Actual on-field accomplishment against schedule
      sor_index = (w_wld * phi_theta + w_sb * phi_sb + w_pd * phi_delta + w_sos * phi_sos) / 
                  pmax(1e-6, w_wld + w_sb + w_pd + w_sos),
      
      # 2. Expected Strength of Resume (xSOR): Underlying down-by-down play efficiency
      xsor_index = (w_xpd * phi_alpha + w_xwld * phi_omega) / 
                   pmax(1e-6, w_xpd + w_xwld),

      # 3. Composite Index: Synthesis of Strength of Resume (SOR) + Expected Strength of Resume (xSOR)
      composite_index = (w_wld * phi_theta) + 
                        (w_sb * phi_sb) + 
                        (w_sos * phi_sos) + 
                        (w_xpd * phi_alpha) + 
                        (w_pd * phi_delta) + 
                        (w_xwld * phi_omega)
    )

  # Direct Head-to-Head Victory Equity: Beating quality opponents head-to-head adds definitive tiebreak equity
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
    final_rankings$composite_index <- final_rankings$composite_index + h2h_credit
  }

  final_rankings <- final_rankings |>
    dplyr::arrange(desc(composite_index)) |>
    dplyr::mutate(
      rank_composite_overall = dplyr::row_number()
    ) |>
    dplyr::group_by(subdivision_id) |>
    dplyr::mutate(
      rank_composite = dplyr::row_number(),
      rank_sor = dplyr::min_rank(desc(sor_index)),
      rank_xsor = dplyr::min_rank(desc(xsor_index)),
      rank_wld = dplyr::min_rank(desc(theta_median)),
      rank_sb = dplyr::min_rank(desc(sb_score)),
      rank_pd = dplyr::min_rank(desc(delta_median)),
      rank_xwld = dplyr::min_rank(desc(omega_median)),
      rank_xpd = dplyr::min_rank(desc(alpha_median)),
      rank_buchholz = dplyr::min_rank(desc(buchholz_sos)),
      
      # Cross-Pipeline Divergence Metrics
      resume_luck = rank_xsor - rank_sor,         # Positive = Actual resume outranks underlying play efficiency
      outcome_luck = rank_xwld - rank_wld,        # Positive = Team won more games than expected from play efficiency
      margin_luck = rank_xpd - rank_pd,          # Positive = Scoreboard margin outperformed expected EPA margin
      finish_factor = rank_pd - rank_wld,        # Positive = Clutch record outperforms point differential
      conversion_factor = rank_xpd - rank_xwld   # Positive = EPA translates strongly into expected wins
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(subdivision_id, rank_composite)
  
  return(final_rankings)
}
