# ==============================================================================
# Module: R/02_transform.R
# Purpose: Transform raw schedule & PBP data into structured 4-pipeline modeling frames
#          (WLD: Actual Discrete, PD: Actual Continuous, 
#           XWLD: Expected Discrete, XPD: Expected Continuous)
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
  
  fbs_confs <- c(
    "acc", "big ten", "big 12", "sec", "pac-12", "pac-2",
    "american athletic", "conference usa", "mid-american",
    "mountain west", "sun belt", "fbs independents"
  )
  
  teams <- dplyr::bind_rows(home_teams, away_teams) |>
    dplyr::filter(!is.na(team), team != "") |>
    dplyr::group_by(team) |>
    dplyr::summarize(
      conference = dplyr::first(na.omit(conference)),
      division = dplyr::first(na.omit(division)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      div_lower = tolower(coalesce(division, "")),
      conf_lower = tolower(coalesce(conference, "")),
      # Robust subdivision classification accounting for year-to-year realignment and FCS promotions:
      is_fbs = (div_lower %in% c("fbs", "i-a", "division i-a") | conf_lower %in% fbs_confs) &
               !div_lower %in% c("fcs", "i-aa", "division i-aa", "ii", "iii"),
      subdivision_id = ifelse(is_fbs, 1L, 2L) # 1 = FBS, 2 = FCS
    ) |>
    dplyr::select(team, conference, division, is_fbs, subdivision_id) |>
    dplyr::arrange(desc(is_fbs), team) |>
    dplyr::mutate(team_id = dplyr::row_number())
  
  return(teams)
}

#' Extract and engineer Pipeline 1: discrete Win/Loss/OT (WLD) game outcomes
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

#' Extract and engineer Pipeline 2: actual Point Differential (PD) continuous outcomes
#' @param con Active DuckDB connection
#' @param season Target season integer
#' @param team_lookup Master team lookup table
#' @return Cleaned tibble of actual point differentials
build_pd_dataset <- function(con, season, team_lookup) {
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
  
  pd_games <- raw_games |>
    dplyr::mutate(
      point_diff = as.numeric(home_points - away_points),
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
  
  return(pd_games)
}

#' Extract and aggregate play-by-play Expected Points Added (EPA) into Pipeline 4 (XPD)
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
    message(sprintf("Warning: No valid PBP EPA rows found for season %d. Returning empty XPD table.", season))
    return(tibble::tibble(
      game_id = character(0),
      home_team = character(0),
      away_team = character(0),
      neutral_site = integer(0),
      home_epa_total = numeric(0),
      away_epa_total = numeric(0),
      home_net_epa = numeric(0),
      total_plays = integer(0),
      home_id = integer(0),
      away_id = integer(0)
    ))
  }
  
  # Apply garbage-time filter if requested (plays in 2nd half with margin > threshold)
  filtered_pbp <- raw_pbp
  if (filter_garbage_time) {
    filtered_pbp <- filtered_pbp |>
      dplyr::filter(!(period >= 3 & score_diff > garbage_threshold))
  }
  
  # Aggregate net EPA per game
  game_xpd <- filtered_pbp |>
    dplyr::mutate(
      pos_clean = tolower(trimws(posteam)),
      home_clean = tolower(trimws(home_team)),
      away_clean = tolower(trimws(away_team))
    ) |>
    dplyr::group_by(game_id, home_team, away_team, neutral_site) |>
    dplyr::summarize(
      home_epa_total = sum(epa[pos_clean == home_clean], na.rm = TRUE),
      away_epa_total = sum(epa[pos_clean == away_clean], na.rm = TRUE),
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

#' Derive Pipeline 3: Expected Win/Loss/OT (XWLD) from play-level Net EPA
#' Maps net EPA into Post-Game Win Expectancy (PGWE) and discrete expected outcome categories
#' @param xpd_df Game-level XPD tibble
#' @param team_lookup Master team lookup table
#' @return Cleaned tibble of expected discrete game outcomes
build_xwld_dataset <- function(xpd_df, team_lookup) {
  if (nrow(xpd_df) == 0) {
    return(tibble::tibble(
      game_id = character(0),
      home_team = character(0),
      away_team = character(0),
      neutral_site = integer(0),
      home_epa_total = numeric(0),
      away_epa_total = numeric(0),
      home_net_epa = numeric(0),
      total_plays = integer(0),
      home_id = integer(0),
      away_id = integer(0),
      pgwe = numeric(0),
      outcome = integer(0)
    ))
  }
  
  # Calculate post-game win expectancy (PGWE) from net EPA:
  # Logistic scaling: 0 EPA = 0.50 PGWE, ~14 EPA net difference ~= 0.89 PGWE
  xwld_df <- xpd_df |>
    dplyr::mutate(
      pgwe = 1 / (1 + exp(-0.15 * as.numeric(home_net_epa))),
      # Outcome coding: 1=ExpRegLoss (<0.25), 2=ExpOTLoss (0.25-0.50), 3=ExpOTWin (0.50-0.75), 4=ExpRegWin (>=0.75)
      outcome = dplyr::case_when(
        pgwe >= 0.75 ~ 4L,
        pgwe >= 0.50 ~ 3L,
        pgwe >= 0.25 ~ 2L,
        TRUE         ~ 1L
      )
    ) |>
    dplyr::arrange(game_id)
  
  return(xwld_df)
}

#' Coordinate transformation and package inputs for all 4 Stan models
#' @param season Target season integer
#' @param db_path File path to DuckDB
#' @param filter_garbage_time Logical to filter garbage time in XPD
#' @return Comprehensive list containing data frames, team lookup, and 4 Stan data lists
transform_all_pipelines <- function(season, db_path = "data/cfb.duckdb", filter_garbage_time = TRUE) {
  con <- get_duckdb_connection(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  sched_df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM schedules WHERE season = %d", season))
  team_lookup <- build_team_lookup(sched_df)
  
  # Build 4 modeling datasets
  wld_df <- build_wld_dataset(con, season, team_lookup)
  pd_df <- build_pd_dataset(con, season, team_lookup)
  xpd_df <- build_xpd_dataset(con, season, team_lookup, filter_garbage_time = filter_garbage_time)
  xwld_df <- build_xwld_dataset(xpd_df, team_lookup)
  
  # Validate that modeling datasets contain games and plays before constructing Stan inputs
  if (nrow(wld_df) == 0) {
    stop(sprintf("Cannot construct Stan datasets: 0 completed games found for season %d. Ensure the season has completed games.", season))
  }
  if (nrow(xpd_df) == 0) {
    stop(sprintf("Cannot construct Stan datasets: 0 play-by-play EPA records found for season %d in DuckDB. Ensure DuckDB has synced PBP data for this season.", season))
  }
  
  # 1. Stan data for Pipeline 1: WLD (Ordered Logit Actual Outcomes)
  stan_data_wld <- list(
    season = as.integer(season),
    N_games = as.integer(nrow(wld_df)),
    N_teams = as.integer(nrow(team_lookup)),
    home_team = as.integer(wld_df$home_id),
    away_team = as.integer(wld_df$away_id),
    neutral_site = as.integer(wld_df$neutral_site),
    outcome = as.integer(wld_df$outcome),
    team_subdivision = as.integer(team_lookup$subdivision_id),
    prior_mu_fcs_mean = -1.5,
    prior_mu_fcs_sd = 0.5,
    prior_sigma_rate = 1.0,
    prior_gamma_mean = 0.2,
    prior_gamma_sd = 0.1
  )
  
  # 2. Stan data for Pipeline 2: PD (Robust Student-t Actual Point Differential)
  stan_data_pd <- list(
    season = as.integer(season),
    N_games = as.integer(nrow(pd_df)),
    N_teams = as.integer(nrow(team_lookup)),
    home_team = as.integer(pd_df$home_id),
    away_team = as.integer(pd_df$away_id),
    neutral_site = as.integer(pd_df$neutral_site),
    pd = as.numeric(pd_df$point_diff),
    team_subdivision = as.integer(team_lookup$subdivision_id),
    prior_xi_fcs_mean = -24.0,
    prior_xi_fcs_sd = 3.0,
    prior_omega_rate = 0.1,
    prior_beta_home_mean = 2.8,
    prior_beta_home_sd = 1.0
  )
  
  # 3. Stan data for Pipeline 3: XWLD (Ordered Logit Expected Outcomes)
  stan_data_xwld <- list(
    season = as.integer(season),
    N_games = as.integer(nrow(xwld_df)),
    N_teams = as.integer(nrow(team_lookup)),
    home_team = as.integer(xwld_df$home_id),
    away_team = as.integer(xwld_df$away_id),
    neutral_site = as.integer(xwld_df$neutral_site),
    outcome = as.integer(xwld_df$outcome),
    team_subdivision = as.integer(team_lookup$subdivision_id),
    prior_mu_fcs_mean = -1.5,
    prior_mu_fcs_sd = 0.5,
    prior_sigma_rate = 1.0,
    prior_gamma_mean = 0.2,
    prior_gamma_sd = 0.1
  )
  
  # 4. Stan data for Pipeline 4: XPD (Robust Student-t Net EPA Differential)
  stan_data_xpd <- list(
    season = as.integer(season),
    N_games = as.integer(nrow(xpd_df)),
    N_teams = as.integer(nrow(team_lookup)),
    home_team = as.integer(xpd_df$home_id),
    away_team = as.integer(xpd_df$away_id),
    neutral_site = as.integer(xpd_df$neutral_site),
    xpd = as.numeric(xpd_df$home_net_epa),
    team_subdivision = as.integer(team_lookup$subdivision_id),
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
    pd_df = pd_df,
    xwld_df = xwld_df,
    xpd_df = xpd_df,
    stan_data_wld = stan_data_wld,
    stan_data_pd = stan_data_pd,
    stan_data_xwld = stan_data_xwld,
    stan_data_xpd = stan_data_xpd
  ))
}

#' Backward compatibility alias for targets pipeline
transform_wld_xpd <- transform_all_pipelines
