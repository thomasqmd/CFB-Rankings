# ==============================================================================
# Module: R/02_transform.R
# Purpose: Transform raw schedule & PBP data into structured WLD & XPD modeling frames
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(stringr)

#' Build master team lookup dictionary
#' @param sched_df Cleaned schedules data frame
#' @return Tibble of unique teams with integer IDs and division indicators (1=FBS, 2=FCS)
build_team_lookup <- function(sched_df) {
  home_teams <- sched_df |>
    dplyr::select(team = home_team, conference = home_conference, division = home_division)
  away_teams <- sched_df |>
    dplyr::select(team = away_team, conference = away_conference, division = away_division)
  
  teams <- dplyr::bind_rows(home_teams, away_teams) |>
    dplyr::filter(!is.na(team), team != "") |>
    dplyr::group_by(team) |>
    dplyr::summarize(
      conference = dplyr::first(na.omit(conference)),
      division = dplyr::first(na.omit(division)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      division_clean = tolower(coalesce(division, "fbs")),
      is_fbs = division_clean %in% c("fbs", "i-a", "division i-a"),
      subdivision_id = ifelse(is_fbs, 1L, 2L) # 1 = FBS, 2 = FCS
    ) |>
    dplyr::arrange(desc(is_fbs), team) |>
    dplyr::mutate(team_id = dplyr::row_number())
  
  return(teams)
}

#' Extract and engineer discrete Win/Loss/OT (WLD) game outcomes
#' @param con Active DuckDB connection
#' @param season Target season integer
#' @param team_lookup Master team lookup table
#' @return Cleaned tibble of discrete game outcomes
build_wld_dataset <- function(con, season, team_lookup) {
  query <- sprintf("
    SELECT game_id, season, week, neutral_site, home_team, away_team, 
           home_points, away_points, notes
    FROM schedules
    WHERE season = %d AND home_points IS NOT NULL AND away_points IS NOT NULL
  ", season)
  
  raw_games <- DBI::dbGetQuery(con, query)
  
  if (nrow(raw_games) == 0) {
    stop(sprintf("No completed games found for season %d in DuckDB.", season))
  }
  
  # Determine overtime from notes or scores
  wld_games <- raw_games |>
    dplyr::mutate(
      is_ot = stringr::str_detect(tolower(coalesce(notes, "")), "ot|overtime|2ot|3ot|4ot|5ot|6ot|7ot|8ot|9ot"),
      point_diff = home_points - away_points,
      # Outcome coding: 1=RegLoss, 2=OTLoss, 3=OTWin, 4=RegWin
      outcome = dplyr::case_when(
        point_diff > 0 & is_ot  ~ 3L, # Overtime Win
        point_diff > 0 & !is_ot ~ 4L, # Regulation Win
        point_diff < 0 & is_ot  ~ 2L, # Overtime Loss
        point_diff < 0 & !is_ot ~ 1L, # Regulation Loss
        TRUE                    ~ 1L  # Fallback for ties
      ),
      neutral_site = as.integer(coalesce(neutral_site, FALSE))
    ) |>
    dplyr::inner_join(
      team_lookup |> dplyr::select(home_team = team, home_id = team_id), 
      by = "home_team"
    ) |>
    dplyr::inner_join(
      team_lookup |> dplyr::select(away_team = team, away_id = team_id), 
      by = "away_team"
    ) |>
    dplyr::arrange(week, game_id)
  
  return(wld_games)
}

#' Extract and aggregate play-by-play Expected Points Added (EPA) into game-level XPD
#' @param con Active DuckDB connection
#' @param season Target season integer
#' @param team_lookup Master team lookup table
#' @param filter_garbage_time Logical indicating whether to filter garbage time plays
#' @param garbage_threshold Absolute point threshold in 2nd half (default: 28 points)
#' @return Cleaned tibble of game-level net EPA differential (XPD)
build_xpd_dataset <- function(con, season, team_lookup, filter_garbage_time = TRUE, garbage_threshold = 28) {
  # Query play-by-play joined with schedule metadata
  query <- sprintf("
    SELECT p.game_id, p.period, p.posteam, p.defteam, p.epa,
           s.neutral_site, s.home_team, s.away_team,
           abs(p.home_score - p.away_score) AS score_diff
    FROM pbp p
    JOIN schedules s ON p.game_id = s.game_id
    WHERE s.season = %d AND p.epa IS NOT NULL
  ", season)
  
  raw_pbp <- DBI::dbGetQuery(con, query)
  
  if (nrow(raw_pbp) == 0) {
    message("Warning: No valid PBP EPA rows found. Returning empty XPD table.")
    return(tibble::tibble())
  }
  
  # Apply garbage-time filter if requested (plays in 2nd half with margin > threshold)
  filtered_pbp <- raw_pbp
  if (filter_garbage_time) {
    filtered_pbp <- filtered_pbp |>
      dplyr::filter(!(period >= 3 & score_diff > garbage_threshold))
  }
  
  # Aggregate net EPA per game
  game_xpd <- filtered_pbp |>
    dplyr::group_by(game_id, home_team, away_team, neutral_site) |>
    dplyr::summarize(
      home_epa_total = sum(epa[posteam == home_team], na.rm = TRUE),
      away_epa_total = sum(epa[posteam == away_team], na.rm = TRUE),
      home_net_epa = home_epa_total - away_epa_total,
      total_plays = n(),
      .groups = "drop"
    ) |>
    dplyr::filter(total_plays >= 40) |> # Minimum plays for reliable sample
    dplyr::inner_join(
      team_lookup |> dplyr::select(home_team = team, home_id = team_id), 
      by = "home_team"
    ) |>
    dplyr::inner_join(
      team_lookup |> dplyr::select(away_team = team, away_id = team_id), 
      by = "away_team"
    ) |>
    dplyr::mutate(neutral_site = as.integer(coalesce(neutral_site, FALSE)))
  
  return(game_xpd)
}

#' Coordinate transformation and package inputs for Stan models
#' @param season Target season integer
#' @param db_path File path to DuckDB
#' @param filter_garbage_time Logical to filter garbage time in XPD
#' @return Comprehensive list containing data frames, team lookup, and Stan data lists
transform_wld_xpd <- function(season, db_path = "data/cfb.duckdb", filter_garbage_time = TRUE) {
  con <- get_duckdb_connection(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  sched_df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM schedules WHERE season = %d", season))
  team_lookup <- build_team_lookup(sched_df)
  
  wld_df <- build_wld_dataset(con, season, team_lookup)
  xpd_df <- build_xpd_dataset(con, season, team_lookup, filter_garbage_time = filter_garbage_time)
  
  # Construct Stan data objects with prior parameters
  stan_data_wld <- list(
    N_games = nrow(wld_df),
    N_teams = nrow(team_lookup),
    home_team = wld_df$home_id,
    away_team = wld_df$away_id,
    neutral_site = wld_df$neutral_site,
    outcome = wld_df$outcome,
    team_subdivision = team_lookup$subdivision_id,
    prior_mu_fcs_mean = -1.5,
    prior_mu_fcs_sd = 0.5,
    prior_sigma_rate = 1.0,
    prior_gamma_mean = 0.2,
    prior_gamma_sd = 0.1
  )
  
  stan_data_xpd <- list(
    N_games = nrow(xpd_df),
    N_teams = nrow(team_lookup),
    home_team = xpd_df$home_id,
    away_team = xpd_df$away_id,
    neutral_site = xpd_df$neutral_site,
    xpd = as.numeric(xpd_df$home_net_epa),
    team_subdivision = team_lookup$subdivision_id,
    prior_xi_fcs_mean = -21.0,
    prior_xi_fcs_sd = 3.0,
    prior_omega_rate = 0.1,
    prior_beta_home_mean = 2.5,
    prior_beta_home_sd = 1.0
  )
  
  return(list(
    season = season,
    team_lookup = team_lookup,
    wld_df = wld_df,
    xpd_df = xpd_df,
    stan_data_wld = stan_data_wld,
    stan_data_xpd = stan_data_xpd
  ))
}
