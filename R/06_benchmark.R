# ==============================================================================
# Module: R/06_benchmark.R
# Purpose: External polling alignment, Colley Matrix baseline, and benchmark divergence metrics
# ==============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(cfbfastR)

#' Compute Colley's Bias-Free Matrix Ranking
#' @param wld_df WLD game outcomes tibble
#' @param team_lookup Master team lookup table
#' @return Tibble containing Colley ratings and ranks
compute_colley_matrix <- function(wld_df, team_lookup) {
  message("[Benchmark] Solving Colley Matrix linear system...")
  
  N_teams <- nrow(team_lookup)
  C <- matrix(0, nrow = N_teams, ncol = N_teams)
  b <- numeric(N_teams)
  
  # Track wins, losses, games played
  wins <- numeric(N_teams)
  losses <- numeric(N_teams)
  games_played <- numeric(N_teams)
  
  for (k in seq_len(nrow(wld_df))) {
    h <- wld_df$home_id[k]
    a <- wld_df$away_id[k]
    out <- wld_df$outcome[k]
    
    games_played[h] <- games_played[h] + 1
    games_played[a] <- games_played[a] + 1
    
    # Bilateral game tally
    C[h, a] <- C[h, a] - 1
    C[a, h] <- C[a, h] - 1
    
    if (out %in% c(3L, 4L)) {
      wins[h] <- wins[h] + 1
      losses[a] <- losses[a] + 1
    } else {
      losses[h] <- losses[h] + 1
      wins[a] <- wins[a] + 1
    }
  }
  
  # Set diagonal: C_ii = 2 + n_i
  diag(C) <- 2 + games_played
  
  # Set RHS vector: b_i = 1 + (W_i - L_i) / 2
  b <- 1 + (wins - losses) / 2
  
  # Solve linear system C * r = b
  colley_ratings <- solve(C, b)
  
  colley_df <- team_lookup |>
    dplyr::mutate(
      colley_rating = colley_ratings,
      wins = wins,
      losses = losses,
      games_played = games_played
    ) |>
    dplyr::group_by(subdivision_id) |>
    dplyr::mutate(
      rank_colley = dplyr::min_rank(desc(colley_rating))
    ) |>
    dplyr::ungroup()
  
  return(colley_df)
}

#' Fetch AP Poll, Coaches Poll, and CFP Committee rankings
#' @param season Target season integer
#' @param week Optional target week integer
#' @return Cleaned tibble of media/committee rankings
fetch_external_polls <- function(season, week = NULL) {
  message(sprintf("[Benchmark] Fetching external poll rankings for season %d...", season))
  
  polls_raw <- tryCatch({
    cfbfastR::cfbd_rankings(season = season, week = week)
  }, error = function(e) {
    message("Notice: Could not fetch live cfbd rankings (API key required or network issue). Using empty poll fallback.")
    return(tibble::tibble())
  })
  
  if (nrow(polls_raw) == 0) {
    return(tibble::tibble(
      poll = character(0),
      week = integer(0),
      team = character(0),
      rank = integer(0),
      points = numeric(0)
    ))
  }
  
  cleaned_polls <- polls_raw |>
    dplyr::filter(poll %in% c("AP Top 25", "Coaches Poll", "Playoff Committee Rankings")) |>
    dplyr::transmute(
      poll = as.character(poll),
      week = as.integer(week),
      team = as.character(school),
      rank = as.integer(rank),
      points = as.numeric(points)
    )
  
  return(cleaned_polls)
}

#' Align Bayesian Composite rankings against Colley and AP Poll
#' @param composite_rankings Tibble from compute_composite_rankings
#' @param colley_rankings Tibble from compute_colley_matrix
#' @param external_polls Tibble from fetch_external_polls
#' @return Comprehensive benchmark comparison list with correlation metrics and divergence outliers
align_and_benchmark_rankings <- function(composite_rankings, colley_rankings, external_polls) {
  message("[Benchmark] Aligning rankings and computing divergence diagnostics...")
  
  # Extract latest AP Top 25
  ap_poll <- external_polls |>
    dplyr::filter(poll == "AP Top 25")
  
  if (nrow(ap_poll) > 0) {
    latest_week <- max(ap_poll$week, na.rm = TRUE)
    ap_latest <- ap_poll |>
      dplyr::filter(week == latest_week) |>
      dplyr::select(team, rank_ap = rank, ap_points = points)
  } else {
    ap_latest <- tibble::tibble(team = character(0), rank_ap = integer(0), ap_points = numeric(0))
  }
  
  # Join all rating systems for FBS
  comparison_df <- composite_rankings |>
    dplyr::filter(subdivision_id == 1L) |>
    dplyr::inner_join(
      colley_rankings |> dplyr::select(team, rank_colley, colley_rating, wins, losses),
      by = "team"
    ) |>
    dplyr::left_join(ap_latest, by = "team") |>
    dplyr::mutate(
      rank_ap_imputed = ifelse(is.na(rank_ap), 30L, rank_ap), # Unranked mapped to 30 for delta calc
      ap_delta = rank_ap_imputed - rank_composite # Positive = Ranked higher by Bayes than AP (AP underrated)
    ) |>
    dplyr::arrange(rank_composite)
  
  # Correlation diagnostics with Colley Matrix
  spearman_colley <- cor(comparison_df$rank_composite, comparison_df$rank_colley, method = "spearman")
  kendall_colley <- cor(comparison_df$rank_composite, comparison_df$rank_colley, method = "kendall")
  
  # Top 25 overlap
  top25_bayes <- comparison_df$team[1:25]
  top25_colley <- comparison_df |> dplyr::arrange(rank_colley) |> dplyr::slice(1:25) |> dplyr::pull(team)
  jaccard_colley <- length(intersect(top25_bayes, top25_colley)) / length(union(top25_bayes, top25_colley))
  
  # AP poll comparison (if available)
  spearman_ap <- NA_real_
  jaccard_ap <- NA_real_
  if (nrow(ap_latest) >= 20) {
    ranked_both <- comparison_df |> dplyr::filter(!is.na(rank_ap))
    spearman_ap <- cor(ranked_both$rank_composite, ranked_both$rank_ap, method = "spearman")
    top25_ap <- ap_latest |> dplyr::arrange(rank_ap) |> dplyr::slice(1:25) |> dplyr::pull(team)
    jaccard_ap <- length(intersect(top25_bayes, top25_ap)) / length(union(top25_bayes, top25_ap))
  }
  
  # Outliers: Most underrated and overrated by AP relative to Bayes resume
  most_underrated <- comparison_df |>
    dplyr::filter(rank_composite <= 25) |>
    dplyr::arrange(desc(ap_delta)) |>
    dplyr::head(5)
  
  most_overrated <- comparison_df |>
    dplyr::filter(!is.na(rank_ap), rank_ap <= 25) |>
    dplyr::arrange(ap_delta) |>
    dplyr::head(5)
  
  benchmark_metrics <- tibble::tibble(
    Benchmark = c("Colley Matrix", "AP Top 25"),
    Spearman_Rho = c(spearman_colley, spearman_ap),
    Kendall_Tau = c(kendall_colley, NA_real_),
    Top25_Jaccard = c(jaccard_colley, jaccard_ap)
  )
  
  return(list(
    comparison_table = comparison_df,
    benchmark_metrics = benchmark_metrics,
    most_underrated = most_underrated,
    most_overrated = most_overrated
  ))
}
