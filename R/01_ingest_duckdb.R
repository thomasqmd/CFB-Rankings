# ==============================================================================
# Module: R/01_ingest_duckdb.R
# Purpose: High-throughput ingestion, incremental sync, and DuckDB analytical storage
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(cfbfastR)

#' Get or create DuckDB connection
#' @param db_path Path to the DuckDB file
#' @param read_only Logical indicating read-only access
#' @return DBI connection object
get_duckdb_connection <- function(db_path = "data/cfb.duckdb", read_only = FALSE) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
  return(con)
}

#' Initialize DuckDB schema tables
#' @param con Active DuckDB connection
init_duckdb_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sync_metadata (
      season INTEGER,
      last_sync TIMESTAMP,
      games_count INTEGER,
      pbp_count INTEGER,
      PRIMARY KEY (season)
    );
  ")
}

#' Determine the latest available season and week
#' @param season Optional season integer; defaults to current calendar year
#' @return List with season and latest completed week
get_latest_cfb_week <- function(season = NULL) {
  if (is.null(season)) {
    curr_date <- Sys.Date()
    curr_year <- as.integer(format(curr_date, "%Y"))
    curr_month <- as.integer(format(curr_date, "%m"))
    # In CFB, season starts around late August. Before August, previous year is active
    season <- if (curr_month >= 8) curr_year else curr_year - 1
  }
  
  sched <- tryCatch({
    cfbfastR::load_cfb_schedules(season)
  }, error = function(e) {
    message("Failed to load schedules for ", season, ", falling back to previous year.")
    season <<- season - 1
    cfbfastR::load_cfb_schedules(season)
  })
  
  completed_games <- sched |>
    dplyr::filter(!is.na(home_points), !is.na(away_points))
  
  max_week <- if (nrow(completed_games) > 0) max(completed_games$week, na.rm = TRUE) else 1
  
  return(list(
    season = season,
    week = max_week,
    n_completed_games = nrow(completed_games)
  ))
}

#' Ingest and sync schedule metadata into DuckDB
#' @param seasons Vector of integer seasons to ingest
#' @param con Active DuckDB connection
ingest_schedules_data <- function(seasons, con) {
  for (s in seasons) {
    message(sprintf("[DuckDB] Ingesting schedule data for season %d...", s))
    sched_raw <- tryCatch({
      cfbfastR::load_cfb_schedules(s)
    }, error = function(e) {
      warning("Could not fetch schedules for season ", s, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(sched_raw) || nrow(sched_raw) == 0) next
    
    # Standardize and clean schedule columns
    sched_clean <- sched_raw |>
      dplyr::transmute(
        game_id = as.character(game_id),
        season = as.integer(season),
        week = as.integer(week),
        season_type = as.character(season_type),
        start_date = as.character(start_date),
        neutral_site = as.logical(coalesce(neutral_site, FALSE)),
        conference_game = as.logical(coalesce(conference_game, FALSE)),
        home_team = as.character(home_team),
        home_conference = as.character(home_conference),
        home_division = as.character(coalesce(home_division, "fbs")),
        home_points = as.integer(home_points),
        away_team = as.character(away_team),
        away_conference = as.character(away_conference),
        away_division = as.character(coalesce(away_division, "fbs")),
        away_points = as.integer(away_points),
        notes = as.character(notes)
      )
    
    duckdb::duckdb_register(con, "temp_sched", sched_clean)
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS schedules AS SELECT * FROM temp_sched WHERE 1=0;
    ")
    # Delete existing records for season to avoid duplicates, then insert
    DBI::dbExecute(con, sprintf("DELETE FROM schedules WHERE season = %d", s))
    DBI::dbExecute(con, "INSERT INTO schedules SELECT * FROM temp_sched")
    duckdb::duckdb_unregister(con, "temp_sched")
  }
}

#' Ingest play-by-play data and extract net play EPA into DuckDB
#' @param seasons Vector of integer seasons to ingest
#' @param con Active DuckDB connection
ingest_pbp_data <- function(seasons, con) {
  for (s in seasons) {
    message(sprintf("[DuckDB] Ingesting PBP EPA data for season %d...", s))
    pbp_raw <- tryCatch({
      cfbfastR::load_cfb_pbp(s)
    }, error = function(e) {
      warning("Could not fetch PBP for season ", s, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(pbp_raw) || nrow(pbp_raw) == 0) next
    
    # Keep only plays with valid EPA to optimize storage
    pbp_clean <- pbp_raw |>
      dplyr::filter(!is.na(epa)) |>
      dplyr::transmute(
        game_id = as.character(game_id),
        play_id = as.character(coalesce(as.character(id_play), as.character(row_number()))),
        period = as.integer(coalesce(period, 1L)),
        clock_minutes = as.integer(coalesce(clock.minutes, 0L)),
        clock_seconds = as.integer(coalesce(clock.seconds, 0L)),
        posteam = as.character(pos_team),
        defteam = as.character(def_pos_team),
        down = as.integer(down),
        distance = as.integer(distance),
        yards_to_goal = as.integer(yards_to_goal),
        epa = as.numeric(epa),
        wpa = as.numeric(coalesce(wpa, 0.0)),
        home_score = as.integer(coalesce(pos_team_score, 0L)),
        away_score = as.integer(coalesce(def_pos_team_score, 0L))
      )
    
    duckdb::duckdb_register(con, "temp_pbp", pbp_clean)
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS pbp AS SELECT * FROM temp_pbp WHERE 1=0;
    ")
    DBI::dbExecute(con, sprintf("
      DELETE FROM pbp WHERE game_id IN (SELECT game_id FROM schedules WHERE season = %d)
    ", s))
    DBI::dbExecute(con, "INSERT INTO pbp SELECT * FROM temp_pbp")
    duckdb::duckdb_unregister(con, "temp_pbp")
  }
}

#' Pull and synchronize DuckDB with latest CFB data
#' @param season Season integer (or NULL for current)
#' @param db_path File path to DuckDB
#' @param full_refresh Logical to force re-fetch
#' @return Summary list of synced records
pull_and_sync_duckdb <- function(season = NULL, db_path = "data/cfb.duckdb", full_refresh = FALSE) {
  latest_info <- get_latest_cfb_week(season)
  target_season <- latest_info$season
  
  con <- get_duckdb_connection(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  init_duckdb_schema(con)
  
  # Check if season needs syncing
  metadata <- tryCatch({
    DBI::dbGetQuery(con, sprintf("SELECT * FROM sync_metadata WHERE season = %d", target_season))
  }, error = function(e) data.frame())
  
  needs_sync <- full_refresh || nrow(metadata) == 0 ||
    as.numeric(difftime(Sys.time(), metadata$last_sync[1], units = "hours")) > 12
  
  if (needs_sync) {
    message(sprintf("[DuckDB Sync] Syncing season %d into %s...", target_season, db_path))
    ingest_schedules_data(target_season, con)
    ingest_pbp_data(target_season, con)
    
    n_games <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM schedules WHERE season = %d", target_season))$n
    n_pbp <- DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) AS n FROM pbp WHERE game_id IN (SELECT game_id FROM schedules WHERE season = %d)
    ", target_season))$n
    
    DBI::dbExecute(con, sprintf("
      INSERT OR REPLACE INTO sync_metadata (season, last_sync, games_count, pbp_count)
      VALUES (%d, CURRENT_TIMESTAMP, %d, %d);
    ", target_season, n_games, n_pbp))
  } else {
    message(sprintf("[DuckDB Sync] Season %d is up to date.", target_season))
  }
  
  summary_stats <- DBI::dbGetQuery(con, "SELECT * FROM sync_metadata")
  return(list(
    db_path = db_path,
    season = target_season,
    week = latest_info$week,
    sync_summary = summary_stats
  ))
}
