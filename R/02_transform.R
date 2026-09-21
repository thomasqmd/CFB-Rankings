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

if (!exists("canonical_team_name", mode = "function")) {
  canonical_team_name <- function(team) {
    if (is.null(team) || length(team) == 0) return(character(0))
    t <- tolower(trimws(as.character(team)))
    t <- gsub("['`]", "", t)
    t <- gsub("[.]", "", t)
    t <- gsub("&", "and", t)
    t <- gsub("\\s+", " ", t)
    dplyr::case_when(
      (grepl("ole miss", t) | 
       (grepl("mississippi", t) & !grepl("state|valley|southern", t)) | 
       (grepl("rebels", t) & !grepl("unlv|nevada", t)) | 
       t %in% c("miss", "um")) ~ "ole miss",
      (grepl("charlotte", t) | t %in% c("clt", "char", "49ers")) ~ "charlotte",
      (grepl("louisville", t) | t %in% c("lou", "lville", "cardinals")) ~ "louisville",
      (grepl("lsu", t) | grepl("louisiana state", t)) ~ "lsu",
      t %in% c("ul monroe", "louisiana monroe", "louisiana-monroe", "ul-monroe") ~ "ul monroe",
      t %in% c("louisiana", "louisiana lafayette", "louisiana-lafayette", "ul lafayette", "ul-lafayette") ~ "louisiana",
      t %in% c("miami", "miami (fl)", "miami fl", "miami (fla)", "miami fla", "miami hurricanes") ~ "miami",
      t %in% c("miami (oh)", "miami oh", "miami (ohio)", "miami ohio", "miami redhawks", "miami university") ~ "miami (oh)",
      t %in% c("hawaii", "hawai'i", "hawai i", "rainbow warriors") ~ "hawaii",
      t %in% c("san jose state", "san jose st", "san josé state", "san josé st") ~ "san jose state",
      t %in% c("southern miss", "southern mississippi", "so mississippi", "so miss") ~ "southern miss",
      t %in% c("nc state", "north carolina state", "north carolina st", "nc st") ~ "nc state",
      t %in% c("app state", "appalachian state", "appalachian st", "app st") ~ "app state",
      t %in% c("sam houston", "sam houston state", "sam houston st") ~ "sam houston",
      t %in% c("umass", "massachusetts") ~ "umass",
      t %in% c("uconn", "connecticut") ~ "uconn",
      t %in% c("fiu", "florida international") ~ "fiu",
      t %in% c("fau", "florida atlantic") ~ "fau",
      t %in% c("utep", "texas-el paso", "texas el paso") ~ "utep",
      t %in% c("utsa", "texas-san antonio", "texas san antonio") ~ "utsa",
      t %in% c("pitt", "pittsburgh") ~ "pitt",
      t %in% c("ucf", "central florida") ~ "ucf",
      t %in% c("usf", "south florida") ~ "usf",
      t %in% c("smu", "southern methodist") ~ "smu",
      t %in% c("tcu", "texas christian") ~ "tcu",
      t %in% c("byu", "brigham young") ~ "byu",
      t %in% c("army", "army west point", "army black knights") ~ "army",
      t %in% c("navy", "navy midshipmen") ~ "navy",
      t %in% c("middle tennessee", "middle tennessee state", "middle tennessee st", "mtsu") ~ "middle tennessee",
      t %in% c("wku", "western kentucky") ~ "wku",
      t %in% c("bowling green", "bowling green state", "bowling green st") ~ "bowling green",
      t %in% c("san diego state", "san diego st", "sdsu") ~ "san diego state",
      TRUE ~ t
    )
  }
}

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
#' @param max_week Optional maximum week integer threshold
#' @return Cleaned tibble of discrete game outcomes
build_wld_dataset <- function(con, season, team_lookup, max_week = NULL) {
  query <- if (!is.null(max_week)) {
    sprintf("
      SELECT game_id, season, week, neutral_site, home_team, away_team, 
             home_points, away_points, notes
      FROM schedules
      WHERE season = %d AND week <= %d AND home_points IS NOT NULL AND away_points IS NOT NULL
    ", season, as.integer(max_week))
  } else {
    sprintf("
      SELECT game_id, season, week, neutral_site, home_team, away_team, 
             home_points, away_points, notes
      FROM schedules
      WHERE season = %d AND home_points IS NOT NULL AND away_points IS NOT NULL
    ", season)
  }
  
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
#' @param max_week Optional maximum week integer threshold
#' @return Cleaned tibble of actual point differentials
build_pd_dataset <- function(con, season, team_lookup, max_week = NULL) {
  query <- if (!is.null(max_week)) {
    sprintf("
      SELECT game_id, season, week, neutral_site, home_team, away_team, 
             home_points, away_points, notes
      FROM schedules
      WHERE season = %d AND week <= %d AND home_points IS NOT NULL AND away_points IS NOT NULL
    ", season, as.integer(max_week))
  } else {
    sprintf("
      SELECT game_id, season, week, neutral_site, home_team, away_team, 
             home_points, away_points, notes
      FROM schedules
      WHERE season = %d AND home_points IS NOT NULL AND away_points IS NOT NULL
    ", season)
  }
  
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
#' @param max_week Optional maximum week integer threshold
#' @param filter_garbage_time Logical indicating whether to filter garbage time plays
#' @param garbage_threshold Absolute point threshold in 2nd half (default: 28 points)
#' @return Cleaned tibble of game-level net EPA differential (XPD)
build_xpd_dataset <- function(con, season, team_lookup, max_week = NULL, filter_garbage_time = TRUE, garbage_threshold = 28) {
  week_clause <- if (!is.null(max_week)) sprintf("AND s2.week <= %d", as.integer(max_week)) else ""
  
  query <- sprintf("
    WITH plays AS (
      SELECT 
        p.game_id, p.period, p.posteam, p.yards_to_goal, p.home_score, p.away_score,
        s2.home_team, s2.away_team, s2.neutral_site,
        CASE 
          WHEN LOWER(TRIM(p.posteam)) = LOWER(TRIM(s2.home_team))
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%miss%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%miss%%%%')
            OR (LOWER(TRIM(s2.home_team)) = 'lsu' AND LOWER(TRIM(p.posteam)) LIKE '%%%%louisiana state%%%%')
            OR (LOWER(TRIM(s2.home_team)) = 'louisiana state' AND LOWER(TRIM(p.posteam)) = 'lsu')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%appalachian%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%app%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%nc state%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%north carolina state%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%monroe%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%monroe%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%lafayette%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%louisiana%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%hawai%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%hawai%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%san jos%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%san jos%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%southern miss%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%southern miss%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%sam houston%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%sam houston%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%massachusetts%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%umass%%%%')
            OR (LOWER(TRIM(s2.home_team)) LIKE '%%%%connecticut%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%uconn%%%%')
            OR (LOWER(TRIM(COALESCE(p.defteam, ''))) = LOWER(TRIM(s2.away_team)) AND LOWER(TRIM(COALESCE(p.posteam, ''))) != LOWER(TRIM(s2.away_team)))
          THEN 1
          WHEN LOWER(TRIM(p.posteam)) = LOWER(TRIM(s2.away_team))
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%miss%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%miss%%%%')
            OR (LOWER(TRIM(s2.away_team)) = 'lsu' AND LOWER(TRIM(p.posteam)) LIKE '%%%%louisiana state%%%%')
            OR (LOWER(TRIM(s2.away_team)) = 'louisiana state' AND LOWER(TRIM(p.posteam)) = 'lsu')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%appalachian%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%app%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%nc state%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%north carolina state%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%monroe%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%monroe%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%lafayette%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%louisiana%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%hawai%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%hawai%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%san jos%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%san jos%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%southern miss%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%southern miss%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%sam houston%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%sam houston%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%massachusetts%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%umass%%%%')
            OR (LOWER(TRIM(s2.away_team)) LIKE '%%%%connecticut%%%%' AND LOWER(TRIM(p.posteam)) LIKE '%%%%uconn%%%%')
            OR (LOWER(TRIM(COALESCE(p.defteam, ''))) = LOWER(TRIM(s2.home_team)) AND LOWER(TRIM(COALESCE(p.posteam, ''))) != LOWER(TRIM(s2.home_team)))
          THEN 2
          ELSE 0
        END as side,
        p.play_id,
        p.clock_minutes,
        p.clock_seconds
      FROM pbp p
      JOIN schedules s2 ON p.game_id = s2.game_id
      WHERE s2.season = %d %s AND p.posteam IS NOT NULL AND p.yards_to_goal IS NOT NULL
    ),
    flagged AS (
      SELECT *,
        CASE WHEN LAG(side) OVER (PARTITION BY game_id ORDER BY period ASC, (COALESCE(clock_minutes, 0) * 60 + COALESCE(clock_seconds, 0)) DESC, CAST(play_id AS BIGINT) ASC) != side 
             OR LAG(side) OVER (PARTITION BY game_id ORDER BY period ASC, (COALESCE(clock_minutes, 0) * 60 + COALESCE(clock_seconds, 0)) DESC, CAST(play_id AS BIGINT) ASC) IS NULL 
             THEN 1 ELSE 0 END as new_drive
      FROM plays
      WHERE side > 0
    ),
    drives AS (
      SELECT game_id, side, home_team, away_team, neutral_site,
        SUM(new_drive) OVER (PARTITION BY game_id ORDER BY period ASC, (COALESCE(clock_minutes, 0) * 60 + COALESCE(clock_seconds, 0)) DESC, CAST(play_id AS BIGINT) ASC) as drive_id,
        yards_to_goal, home_score, away_score
      FROM flagged
    ),
    drive_summary AS (
      SELECT game_id, side, home_team, away_team, neutral_site, drive_id,
        MIN(yards_to_goal) as min_ytg,
        MAX(CASE WHEN side = 1 THEN home_score ELSE away_score END) - 
        MIN(CASE WHEN side = 1 THEN home_score ELSE away_score END) as pts_gained,
        COUNT(*) as drive_plays
      FROM drives
      GROUP BY game_id, side, home_team, away_team, neutral_site, drive_id
    ),
    drive_xp AS (
      SELECT game_id, side, home_team, away_team, neutral_site, drive_plays,
        CASE 
          WHEN pts_gained >= 6 OR min_ytg = 0 THEN 7.0
          WHEN pts_gained = 3 THEN GREATEST(3.0, 4.5 - 0.05 * min_ytg)
          WHEN min_ytg <= 10 THEN 5.5 - 0.05 * min_ytg
          WHEN min_ytg <= 25 THEN 4.5 - 0.067 * (min_ytg - 10)
          WHEN min_ytg <= 40 THEN 3.5 - 0.10 * (min_ytg - 25)
          WHEN min_ytg <= 60 THEN 2.0 - 0.075 * (min_ytg - 40)
          ELSE 0.2
        END as xp
      FROM drive_summary
    )
    SELECT 
      game_id,
      home_team,
      away_team,
      neutral_site,
      ROUND(SUM(CASE WHEN side = 1 THEN xp ELSE 0 END), 2) as home_epa_total,
      ROUND(SUM(CASE WHEN side = 2 THEN xp ELSE 0 END), 2) as away_epa_total,
      ROUND(SUM(CASE WHEN side = 1 THEN xp ELSE 0 END) - SUM(CASE WHEN side = 2 THEN xp ELSE 0 END), 2) as home_net_epa,
      SUM(drive_plays) as total_plays
    FROM drive_xp
    GROUP BY game_id, home_team, away_team, neutral_site
    HAVING SUM(drive_plays) >= 30
  ", as.integer(season), week_clause)
  
  xpd_raw <- DBI::dbGetQuery(con, query)
  
  if (nrow(xpd_raw) == 0) {
    message(sprintf("Warning: No valid Drive XP rows found for season %d. Returning empty XPD table.", season))
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
  
  game_xpd <- xpd_raw |>
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
#' @param max_week Optional maximum week integer threshold to align with concluded weeks
#' @param db_path File path to DuckDB
#' @param filter_garbage_time Logical to filter garbage time in XPD
#' @param version Semantic version tag to track schema and attribution changes
#' @return Comprehensive list containing data frames, team lookup, and 4 Stan data lists
transform_all_pipelines <- function(season, max_week = NULL, db_path = "data/cfb.duckdb", filter_garbage_time = TRUE, version = "v5_drive_xg") {
  con <- get_duckdb_connection(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  sched_query <- if (!is.null(max_week)) {
    sprintf("SELECT * FROM schedules WHERE season = %d AND week <= %d", season, as.integer(max_week))
  } else {
    sprintf("SELECT * FROM schedules WHERE season = %d", season)
  }
  sched_df <- DBI::dbGetQuery(con, sched_query)
  team_lookup <- build_team_lookup(sched_df)
  
  # Build 4 modeling datasets
  wld_df <- build_wld_dataset(con, season, team_lookup, max_week = max_week)
  pd_df <- build_pd_dataset(con, season, team_lookup, max_week = max_week)
  xpd_df <- build_xpd_dataset(con, season, team_lookup, max_week = max_week, filter_garbage_time = filter_garbage_time)
  xwld_df <- build_xwld_dataset(xpd_df, team_lookup)
  
  # Validate that modeling datasets contain games and plays before constructing Stan inputs
  if (nrow(wld_df) == 0) {
    stop(sprintf("Cannot construct Stan datasets: 0 completed games found for season %d (week <= %s). Ensure the season has completed games.", 
                 season, ifelse(is.null(max_week), "all", as.character(max_week))))
  }
  if (nrow(xpd_df) == 0) {
    stop(sprintf("Cannot construct Stan datasets: 0 play-by-play EPA records found for season %d (week <= %s) in DuckDB. Ensure DuckDB has synced PBP data for this season.", 
                 season, ifelse(is.null(max_week), "all", as.character(max_week))))
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
    max_week = max_week,
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
