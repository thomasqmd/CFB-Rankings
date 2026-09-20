# ==============================================================================
# Module: R/01_ingest_duckdb.R
# Purpose: High-throughput ingestion, incremental sync, and DuckDB analytical storage
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)
library(cfbfastR)

# Ensure ~/.Renviron is loaded and sanitize quotes
if (file.exists("~/.Renviron")) readRenviron("~/.Renviron")
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (nzchar(Sys.getenv("CFBD_API_KEY", ""))) {
  Sys.setenv(CFBD_API_KEY = gsub('^["\']|["\']$', '', Sys.getenv("CFBD_API_KEY")))
}
has_cfbd <- nzchar(Sys.getenv("CFBD_API_KEY", ""))

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
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS schedules (
      game_id VARCHAR PRIMARY KEY,
      season INTEGER,
      week INTEGER,
      season_type VARCHAR,
      start_date VARCHAR,
      neutral_site BOOLEAN,
      conference_game BOOLEAN,
      home_team VARCHAR,
      home_conference VARCHAR,
      home_division VARCHAR,
      home_points INTEGER,
      away_team VARCHAR,
      away_conference VARCHAR,
      away_division VARCHAR,
      away_points INTEGER,
      notes VARCHAR
    );
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS pbp (
      game_id VARCHAR,
      play_id VARCHAR,
      period INTEGER,
      clock_minutes INTEGER,
      clock_seconds INTEGER,
      posteam VARCHAR,
      defteam VARCHAR,
      down INTEGER,
      distance INTEGER,
      yards_to_goal INTEGER,
      epa DOUBLE,
      wpa DOUBLE,
      home_score INTEGER,
      away_score INTEGER
    );
  ")
}

#' Determine the latest available season and week
#' @param season Optional season integer; defaults to current calendar year (or latest open release)
#' @return List with season and latest completed week
get_latest_cfb_week <- function(season = NULL) {
  if (!is.null(season)) {
    target_season <- as.integer(season)
  } else {
    curr_date <- Sys.Date()
    curr_year <- as.integer(format(curr_date, "%Y"))
    curr_month <- as.integer(format(curr_date, "%m"))
    candidate_season <- if (curr_month >= 8) curr_year else curr_year - 1
    
    # Static cfbfastR data releases currently extend through 2024 unless a CFBD API key is configured
    has_api_key <- nzchar(Sys.getenv("CFBD_API_KEY"))
    if (!has_api_key && candidate_season > 2024) {
      message(sprintf("No CFBD API key found; capping candidate season from %d to latest open data release (2024).", candidate_season))
      candidate_season <- 2024L
    }
    target_season <- candidate_season
  }
  
  filter_completed <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    df |>
      dplyr::filter(
        !is.na(home_points), !is.na(away_points),
        as.Date(substr(coalesce(start_date, "1900-01-01"), 1, 10)) <= Sys.Date(),
        (home_points > 0 | away_points > 0)
      )
  }
  
  sched <- tryCatch({
    cfbfastR::load_cfb_schedules(target_season)
  }, error = function(e) NULL)
  
  completed_games <- filter_completed(sched)
  
  # If the queried season has no completed games, step back to the most recent completed season
  while (nrow(completed_games) == 0 && target_season >= 2020) {
    message(sprintf("Season %d has no completed games; falling back to previous season %d...", target_season, target_season - 1))
    target_season <- target_season - 1
    sched <- tryCatch(cfbfastR::load_cfb_schedules(target_season), error = function(e) NULL)
    completed_games <- filter_completed(sched)
  }
  
  max_week <- if (nrow(completed_games) > 0) max(completed_games$week, na.rm = TRUE) else 1
  
  return(list(
    season = as.integer(target_season),
    week = as.integer(max_week),
    n_completed_games = as.integer(nrow(completed_games))
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
        home_division = as.character(home_division),
        home_points = as.integer(home_points),
        away_team = as.character(away_team),
        away_conference = as.character(away_conference),
        away_division = as.character(away_division),
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
    
    if (is.null(pbp_raw) || nrow(pbp_raw) == 0) {
      message(sprintf("[DuckDB] No play-by-play records returned for season %d, skipping.", s))
      next
    }
    
    # Normalize all column names to lowercase to avoid case-sensitivity bugs (e.g. EPA vs epa)
    names(pbp_raw) <- tolower(names(pbp_raw))
    
    # Identify EPA column (EPA, epa, or epa_model)
    epa_col <- intersect(c("epa", "epa_model", "expected_points_added"), names(pbp_raw))[1]
    if (is.na(epa_col)) {
      message(sprintf("[DuckDB] No EPA column found in play-by-play for season %d, skipping.", s))
      next
    }
    
    # Resolve team, clock, and score columns across schema variations
    pos_col <- intersect(c("pos_team", "posteam", "pos_team_name", "offense_play", "offense_team"), names(pbp_raw))[1]
    def_col <- intersect(c("def_pos_team", "defteam", "def_team_name", "defense_play", "defense_team"), names(pbp_raw))[1]
    play_id_col <- intersect(c("id_play", "play_id", "id"), names(pbp_raw))[1]
    clock_min_col <- intersect(c("clock.minutes", "clock_minutes", "clock_minute"), names(pbp_raw))[1]
    clock_sec_col <- intersect(c("clock.seconds", "clock_seconds", "clock_second"), names(pbp_raw))[1]
    home_score_col <- intersect(c("pos_team_score", "home_score"), names(pbp_raw))[1]
    away_score_col <- intersect(c("def_pos_team_score", "away_score"), names(pbp_raw))[1]
    wpa_col <- intersect(c("wpa", "wpa_model", "win_probability_added"), names(pbp_raw))[1]
    
    # Keep only plays with valid EPA to optimize storage
    pbp_clean <- tibble::tibble(
      game_id = as.character(pbp_raw$game_id),
      play_id = if (!is.na(play_id_col)) as.character(pbp_raw[[play_id_col]]) else as.character(seq_len(nrow(pbp_raw))),
      period = as.integer(coalesce(if ("period" %in% names(pbp_raw)) pbp_raw$period else 1L, 1L)),
      clock_minutes = as.integer(coalesce(if (!is.na(clock_min_col)) pbp_raw[[clock_min_col]] else 0L, 0L)),
      clock_seconds = as.integer(coalesce(if (!is.na(clock_sec_col)) pbp_raw[[clock_sec_col]] else 0L, 0L)),
      posteam = if (!is.na(pos_col)) as.character(pbp_raw[[pos_col]]) else NA_character_,
      defteam = if (!is.na(def_col)) as.character(pbp_raw[[def_col]]) else NA_character_,
      down = as.integer(coalesce(if ("down" %in% names(pbp_raw)) pbp_raw$down else 1L, 1L)),
      distance = as.integer(coalesce(if ("distance" %in% names(pbp_raw)) pbp_raw$distance else 10L, 10L)),
      yards_to_goal = as.integer(coalesce(if ("yards_to_goal" %in% names(pbp_raw)) pbp_raw$yards_to_goal else 50L, 50L)),
      epa = as.numeric(pbp_raw[[epa_col]]),
      wpa = as.numeric(coalesce(if (!is.na(wpa_col)) pbp_raw[[wpa_col]] else 0.0, 0.0)),
      home_score = as.integer(coalesce(if (!is.na(home_score_col)) pbp_raw[[home_score_col]] else 0L, 0L)),
      away_score = as.integer(coalesce(if (!is.na(away_score_col)) pbp_raw[[away_score_col]] else 0L, 0L))
    ) |>
      dplyr::filter(!is.na(epa))
    
    if (nrow(pbp_clean) == 0) {
      message(sprintf("[DuckDB] No plays with non-NA EPA for season %d, skipping.", s))
      next
    }
    
    duckdb::duckdb_register(con, "temp_pbp", pbp_clean)
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS pbp AS SELECT * FROM temp_pbp WHERE 1=0;
    ")
    DBI::dbExecute(con, sprintf("
      DELETE FROM pbp WHERE game_id IN (SELECT game_id FROM schedules WHERE season = %d)
    ", s))
    DBI::dbExecute(con, "INSERT INTO pbp SELECT * FROM temp_pbp")
    duckdb::duckdb_unregister(con, "temp_pbp")
    message(sprintf("[DuckDB] Successfully ingested %d plays with EPA for season %d.", nrow(pbp_clean), s))
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
  
  # Check if season needs syncing (force re-sync if pbp_count is 0)
  metadata <- tryCatch({
    DBI::dbGetQuery(con, sprintf("SELECT * FROM sync_metadata WHERE season = %d", target_season))
  }, error = function(e) data.frame())
  
  has_valid_pbp <- nrow(metadata) > 0 && !is.na(metadata$pbp_count[1]) && metadata$pbp_count[1] > 0
  needs_sync <- full_refresh || nrow(metadata) == 0 || !has_valid_pbp ||
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
