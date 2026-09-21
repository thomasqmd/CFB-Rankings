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

#' Normalize team names to canonical forms to reconcile differences across schedules, PBP, and polls
#' @param team Character vector of team names
#' @return Character vector of canonical team names
canonical_team_name <- function(team) {
  if (is.null(team) || length(team) == 0) return(character(0))
  t <- tolower(trimws(as.character(team)))
  
  # Strip punctuation, accents, and extra whitespace
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

#' Resolve the latest completed college football week aligned with the AP Poll release
#'
#' College Football regular season slates typically conclude on Saturday night (with occasional
#' Sunday or Monday games). The Associated Press Top 25 Poll is officially published on
#' Sunday afternoons at approximately 2:00 PM US Eastern Time (14:00 America/New_York).
#' 
#' For a week to be counted as the active evaluation week for modeling and external benchmarks:
#' 1. All scheduled games for that week must be completed (allowing >= 95% completion for rare weather cancellations).
#' 2. Current time must be past the expected AP Poll release time:
#'    - Sunday at 14:00 ET for slates finishing on Saturday (or Thursday/Friday).
#'    - Monday at 14:00 ET for slates finishing on Sunday.
#'    - Tuesday at 14:00 ET for slates finishing on Monday (e.g., Week 1 Labor Day weekend).
#'
#' If a week is currently underway (e.g. games played on Thursday/Friday/Saturday, or Sunday morning
#' before the AP poll drops), the pipeline steps back to the most recent fully concluded week.
#'
#' @param weekly_summary Data frame containing: season, week, total_games, completed_games, latest_start_date
#' @param now POSIXct timestamp of current time (default Sys.time())
#' @return List with week (integer), n_completed_games (integer), and in_progress_week (integer or NULL)
resolve_completed_ap_week <- function(weekly_summary, now = Sys.time()) {
  if (is.null(weekly_summary) || nrow(weekly_summary) == 0) {
    return(list(week = 1L, n_completed_games = 0L, in_progress_week = NULL))
  }
  
  # Ensure now is evaluated in Eastern Time (America/New_York)
  now_et <- as.POSIXct(format(now, tz = "America/New_York", usetz = FALSE), tz = "America/New_York")
  
  # Helper to parse diverse start_date strings into POSIXct UTC
  parse_iso <- function(x) {
    if (is.na(x) || !nzchar(trimws(as.character(x)))) return(as.POSIXct(NA))
    x_str <- trimws(as.character(x))
    t <- tryCatch(as.POSIXct(x_str, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"), error = function(e) NA)
    if (is.na(t)) t <- tryCatch(as.POSIXct(x_str, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), error = function(e) NA)
    if (is.na(t)) t <- tryCatch(as.POSIXct(substr(x_str, 1, 10), format = "%Y-%m-%d", tz = "UTC"), error = function(e) NA)
    t
  }
  
  evaluated_weeks <- weekly_summary |>
    dplyr::arrange(week) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      is_eligible = {
        if (completed_games == 0L) {
          FALSE
        } else {
          # Check 1: Game completion threshold
          # A week is concluded if >= 60% of scheduled games are complete, OR if >= 20 games are complete
          # (accommodates lower-tier/FCS games lacking timely score entries in external feeds)
          games_finished <- (completed_games >= total_games) || 
                            (total_games > 0 && (completed_games / total_games) >= 0.60) ||
                            (completed_games >= 20L)
          
          # Check 2: Expected AP Poll Release Time in Eastern Time
          dt_utc <- parse_iso(latest_start_date)
          if (is.na(dt_utc)) {
            games_finished
          } else {
            game_date_et <- as.Date(format(dt_utc, tz = "America/New_York", format = "%Y-%m-%d"))
            wday_val <- as.POSIXlt(game_date_et)$wday # 0=Sun, 1=Mon, ..., 6=Sat
            
            # Derive expected AP release date & time (14:00 ET = 2:00 PM Eastern)
            # Monday game (e.g. Labor Day Monday) -> AP poll drops Tuesday 14:00 ET
            # Sunday/Saturday slates -> AP poll drops Sunday 14:00 ET
            if (wday_val == 1L) {
              ap_date <- game_date_et + 1L
            } else if (wday_val == 0L) {
              ap_date <- game_date_et
            } else {
              days_to_sun <- (7L - wday_val) %% 7L
              ap_date <- game_date_et + days_to_sun
            }
            
            ap_release_et <- as.POSIXct(paste0(as.character(ap_date), " 14:00:00"), tz = "America/New_York")
            
            # Eligible if games are finished and AP release time has arrived, OR if >= 25 games completed
            (games_finished && (now_et >= ap_release_et)) || (completed_games >= 25L && now_et >= as.POSIXct(paste0(as.character(game_date_et), " 22:00:00"), tz = "America/New_York"))
          }
        }
      }
    ) |>
    dplyr::ungroup()
  
  # Identify weeks currently in progress (games started/completed, but week not AP-eligible yet)
  in_progress <- evaluated_weeks |>
    dplyr::filter(!is_eligible, completed_games > 0)
  in_prog_week <- if (nrow(in_progress) > 0) max(in_progress$week) else NULL
  
  eligible <- evaluated_weeks |>
    dplyr::filter(is_eligible)
  
  if (nrow(eligible) > 0) {
    active_week <- max(eligible$week)
    n_games <- sum(evaluated_weeks$completed_games[evaluated_weeks$week <= active_week])
    return(list(
      week = as.integer(active_week),
      n_completed_games = as.integer(n_games),
      in_progress_week = in_prog_week
    ))
  } else {
    # If no weeks have met the threshold yet (e.g., Week 1 currently underway)
    fallback_week <- if (nrow(evaluated_weeks) > 0) min(evaluated_weeks$week) else 1L
    n_games <- sum(evaluated_weeks$completed_games[evaluated_weeks$week <= fallback_week])
    return(list(
      week = as.integer(fallback_week),
      n_completed_games = as.integer(n_games),
      in_progress_week = in_prog_week
    ))
  }
}

#' Determine the latest available season and week, checking date and DuckDB freshness
#' @param season Optional season integer; defaults to latest available in DuckDB or calendar year
#' @param db_path Optional path to DuckDB analytical store to check local cache first
#' @param force_network Logical; if TRUE, forces query to cfbfastR network endpoints
#' @return List with season, latest completed week, and completed game count
get_latest_cfb_week <- function(season = NULL, db_path = "data/cfb.duckdb", force_network = FALSE) {
  curr_date <- Sys.Date()
  curr_year <- as.integer(format(curr_date, "%Y"))
  curr_month <- as.integer(format(curr_date, "%m"))
  curr_active_season <- if (curr_month >= 8) curr_year else curr_year - 1L

  # 1. Check local DuckDB first to protect against unnecessary network / API requests
  if (!force_network && !is.null(db_path) && file.exists(db_path)) {
    local_info <- tryCatch({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
      
      if (DBI::dbExistsTable(con, "schedules")) {
        seasons_avail <- DBI::dbGetQuery(con, "SELECT DISTINCT season FROM schedules ORDER BY season DESC")$season
        if (length(seasons_avail) > 0) {
          target_seasons <- if (!is.null(season)) as.integer(season) else seasons_avail
          
          # Check if the active current season in DuckDB was synced today
          is_active_stale <- FALSE
          if (curr_active_season %in% target_seasons) {
            last_sync_date <- if (DBI::dbExistsTable(con, "sync_metadata")) {
              meta <- DBI::dbGetQuery(con, sprintf("SELECT last_sync FROM sync_metadata WHERE season = %d", curr_active_season))
              if (nrow(meta) > 0 && !is.na(meta$last_sync[1])) as.Date(meta$last_sync[1]) else as.Date("1970-01-01")
            } else {
              as.Date("1970-01-01")
            }
            if (last_sync_date < curr_date) {
              is_active_stale <- TRUE
            }
          }

          if (!is_active_stale) {
            for (s in target_seasons) {
              weekly_df <- DBI::dbGetQuery(con, sprintf("
                SELECT 
                  season,
                  week,
                  COUNT(*) as total_games,
                  COUNT(CASE WHEN home_points IS NOT NULL AND away_points IS NOT NULL THEN 1 END) as completed_games,
                  MAX(start_date) as latest_start_date
                FROM schedules 
                WHERE season = %d 
                GROUP BY season, week 
                ORDER BY week ASC
              ", as.integer(s)))
              
              if (nrow(weekly_df) > 0 && sum(weekly_df$completed_games) > 0) {
                res <- resolve_completed_ap_week(weekly_df)
                if (!is.null(res$in_progress_week)) {
                  message(sprintf("[Season Tracker] Season %d Week %d is currently underway (AP Poll expected Sunday 14:00 ET). Evaluating latest concluded Week %d (%d games).",
                                  s, res$in_progress_week, res$week, res$n_completed_games))
                } else {
                  message(sprintf("[Season Tracker] Using cached DuckDB state: season %d (week %d, %d completed games).",
                                  s, res$week, res$n_completed_games))
                }
                return(list(
                  season = as.integer(s),
                  week = as.integer(res$week),
                  n_completed_games = as.integer(res$n_completed_games)
                ))
              }
            }
          } else {
            message(sprintf("[Season Tracker] DuckDB for active season %d was last synced before today (%s < %s). Checking external network schedules...",
                            curr_active_season, as.character(last_sync_date), as.character(curr_date)))
          }
        }
      }
      NULL
    }, error = function(e) NULL)
    
    if (!is.null(local_info)) return(local_info)
  }

  # 2. Fall back to network discovery if not in local store or active season is stale
  if (!is.null(season)) {
    target_season <- as.integer(season)
  } else {
    target_season <- curr_active_season
  }
  
  message(sprintf("[Season Tracker] Querying external network schedules for season %d...", target_season))
  sched <- tryCatch({
    cfbfastR::load_cfb_schedules(target_season)
  }, error = function(e) NULL)
  
  build_weekly <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    df |>
      dplyr::group_by(season, week) |>
      dplyr::summarize(
        total_games = n(),
        completed_games = sum(!is.na(home_points) & !is.na(away_points) & (home_points > 0 | away_points > 0 | (home_points == 0 & away_points == 0))),
        latest_start_date = if (all(is.na(start_date))) NA_character_ else max(as.character(start_date), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(week)
  }
  
  weekly_df <- build_weekly(sched)
  res <- if (nrow(weekly_df) > 0) resolve_completed_ap_week(weekly_df) else list(week = 1L, n_completed_games = 0L, in_progress_week = NULL)
  
  # If the queried season has no completed games, step back to the most recent completed season
  while (res$n_completed_games == 0 && target_season >= 2020) {
    message(sprintf("Season %d has no completed games; falling back to previous season %d...", target_season, target_season - 1))
    target_season <- target_season - 1
    sched <- tryCatch(cfbfastR::load_cfb_schedules(target_season), error = function(e) NULL)
    weekly_df <- build_weekly(sched)
    res <- if (nrow(weekly_df) > 0) resolve_completed_ap_week(weekly_df) else list(week = 1L, n_completed_games = 0L, in_progress_week = NULL)
  }
  
  if (!is.null(res$in_progress_week)) {
    message(sprintf("[Season Tracker] Season %d Week %d is currently underway (AP Poll expected Sunday 14:00 ET). Evaluating latest concluded Week %d (%d games).",
                    target_season, res$in_progress_week, res$week, res$n_completed_games))
  }
  
  return(list(
    season = as.integer(target_season),
    week = as.integer(res$week),
    n_completed_games = as.integer(res$n_completed_games)
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
    if (s < 2014) {
      message(sprintf("[DuckDB] Play-by-play EPA is only available for 2014+, skipping PBP for season %d.", s))
      next
    }
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
      posteam = if (!is.na(pos_col)) canonical_team_name(pbp_raw[[pos_col]]) else NA_character_,
      defteam = if (!is.na(def_col)) canonical_team_name(pbp_raw[[def_col]]) else NA_character_,
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
#' List all seasons currently stored in DuckDB
#' @param db_path Path to the DuckDB file
#' @return Tibble of seasons, game counts, play counts, and last sync timestamp
list_duckdb_seasons <- function(db_path = "data/cfb.duckdb") {
  if (!file.exists(db_path)) return(tibble::tibble())
  con <- get_duckdb_connection(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  if (!DBI::dbExistsTable(con, "sync_metadata")) return(tibble::tibble())
  res <- DBI::dbGetQuery(con, "
    SELECT 
      m.season, 
      m.games_count, 
      m.pbp_count, 
      m.last_sync,
      COUNT(DISTINCT s.home_team) as n_teams
    FROM sync_metadata m
    LEFT JOIN schedules s ON m.season = s.season
    GROUP BY m.season, m.games_count, m.pbp_count, m.last_sync
    ORDER BY m.season DESC
  ")
  return(tibble::as_tibble(res))
}

#' Pull and synchronize DuckDB with CFB data across one or multiple seasons
#' @param seasons Integer vector of seasons to sync (e.g. 2021:2024). If NULL, uses latest available.
#' @param db_path File path to DuckDB
#' @param full_refresh Logical to force re-fetch
#' @param current_season_info Optional current season info list from get_latest_cfb_week
#' @return Summary list of synced records
pull_and_sync_duckdb <- function(seasons = NULL, db_path = "data/cfb.duckdb", full_refresh = FALSE, current_season_info = NULL) {
  target_seasons <- if (!is.null(seasons)) {
    as.integer(seasons)
  } else if (!is.null(current_season_info)) {
    as.integer(current_season_info$season)
  } else {
    get_latest_cfb_week(db_path = db_path)$season
  }
  
  con <- get_duckdb_connection(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  init_duckdb_schema(con)
  curr_year <- as.integer(format(Sys.Date(), "%Y"))
  
  for (s in target_seasons) {
    metadata <- tryCatch({
      DBI::dbGetQuery(con, sprintf("SELECT * FROM sync_metadata WHERE season = %d", s))
    }, error = function(e) data.frame())
    
    has_valid_data <- nrow(metadata) > 0 && 
                      !is.na(metadata$games_count[1]) && metadata$games_count[1] > 0 &&
                      (s < 2014 || (!is.na(metadata$pbp_count[1]) && metadata$pbp_count[1] > 0))
    
    # Completed past seasons are frozen and never need re-fetching
    is_completed_season <- s < curr_year || (s == curr_year && as.integer(format(Sys.Date(), "%m")) < 8)
    
    needs_sync <- if (full_refresh) {
      TRUE
    } else if (!has_valid_data) {
      TRUE
    } else if (is_completed_season) {
      # Data already present for a completed season -> strictly no API/network fetch
      FALSE
    } else {
      # Current active season: refresh if last sync was before today,
      # if >= 3 hours elapsed, or if current_season_info has more completed games than DuckDB
      last_sync_time <- as.POSIXct(metadata$last_sync[1])
      (as.Date(last_sync_time) < Sys.Date()) ||
        (as.numeric(difftime(Sys.time(), last_sync_time, units = "hours")) >= 3) ||
        (!is.null(current_season_info) && !is.null(current_season_info$n_completed_games) && 
         current_season_info$n_completed_games > metadata$games_count[1])
    }
    
    if (needs_sync) {
      message(sprintf("[DuckDB Sync] Syncing season %d into %s...", s, db_path))
      ingest_schedules_data(s, con)
      ingest_pbp_data(s, con)
      
      n_games <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM schedules WHERE season = %d", s))$n
      n_pbp <- DBI::dbGetQuery(con, sprintf("
        SELECT COUNT(*) AS n FROM pbp WHERE game_id IN (SELECT game_id FROM schedules WHERE season = %d)
      ", s))$n
      
      DBI::dbExecute(con, sprintf("
        INSERT OR REPLACE INTO sync_metadata (season, last_sync, games_count, pbp_count)
        VALUES (%d, CURRENT_TIMESTAMP, %d, %d);
      ", s, n_games, n_pbp))
    } else {
      message(sprintf("[DuckDB Sync] Season %d is up to date (%d games, %d plays). Skipping API/network fetch.", 
                      s, metadata$games_count[1], metadata$pbp_count[1]))
    }
  }
  
  latest_info <- get_latest_cfb_week(if (length(target_seasons) > 0) max(target_seasons) else NULL, db_path = db_path)
  summary_stats <- DBI::dbGetQuery(con, "SELECT * FROM sync_metadata ORDER BY season DESC")
  return(list(
    db_path = db_path,
    primary_season = max(target_seasons),
    seasons = target_seasons,
    week = latest_info$week,
    sync_summary = summary_stats
  ))
}

#' Extract multi-season standings and ratings across all seasons in DuckDB
#' @param db_path Path to DuckDB
#' @param min_season Earliest season to retrieve (defaults to 2013, spanning BCS finale and full modern CFP era)
#' @return Tibble containing multi-season team records, point differentials, and rankings
get_multi_season_records <- function(db_path = "data/cfb.duckdb", min_season = 2013) {
  if (!file.exists(db_path)) return(tibble::tibble())
  
  con <- tryCatch(get_duckdb_connection(db_path, read_only = TRUE), error = function(e) NULL)
  if (is.null(con)) return(tibble::tibble())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  if (!DBI::dbExistsTable(con, "schedules")) return(tibble::tibble())
  
  q <- sprintf("
    WITH games_home AS (
      SELECT 
        season,
        home_team as team,
        home_conference as conference,
        home_division as division,
        CASE WHEN home_points > away_points THEN 1 ELSE 0 END as win,
        CASE WHEN home_points < away_points THEN 1 ELSE 0 END as loss,
        home_points - away_points as margin,
        home_points as pts_for,
        away_points as pts_against
      FROM schedules
      WHERE season >= %d AND home_points IS NOT NULL AND away_points IS NOT NULL
    ),
    games_away AS (
      SELECT 
        season,
        away_team as team,
        away_conference as conference,
        away_division as division,
        CASE WHEN away_points > home_points THEN 1 ELSE 0 END as win,
        CASE WHEN away_points < home_points THEN 1 ELSE 0 END as loss,
        away_points - home_points as margin,
        away_points as pts_for,
        home_points as pts_against
      FROM schedules
      WHERE season >= %d AND home_points IS NOT NULL AND away_points IS NOT NULL
    ),
    all_team_games AS (
      SELECT * FROM games_home UNION ALL SELECT * FROM games_away
    )
    SELECT 
      season,
      team,
      FIRST(conference) as conference,
      FIRST(division) as division,
      COUNT(*) as games_played,
      SUM(win) as wins,
      SUM(loss) as losses,
      ROUND(SUM(win) * 1.0 / COUNT(*), 3) as win_pct,
      ROUND(AVG(margin), 1) as avg_margin,
      ROUND(AVG(pts_for), 1) as ppg,
      ROUND(AVG(pts_against), 1) as opp_ppg
    FROM all_team_games
    GROUP BY season, team
    ORDER BY season DESC, wins DESC, avg_margin DESC
  ", as.integer(min_season), as.integer(min_season))
  
  res <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) data.frame())
  
  if (nrow(res) == 0) return(tibble::tibble())
  
  out <- tibble::as_tibble(res) |>
    dplyr::group_by(season) |>
    dplyr::mutate(
      rank_season = dplyr::row_number(),
      record = paste0(wins, "-", losses)
    ) |>
    dplyr::ungroup()

  return(out)
}

#' Extract a per-team, per-game schedule log across all seasons in DuckDB, including
#' actual results and an EPA-derived "expected" result/margin for each game
#' @param db_path Path to DuckDB
#' @param min_season Earliest season to retrieve (defaults to 2013)
#' @param filter_garbage_time Logical; excludes 2nd-half plays once a game is out of hand
#' @param garbage_threshold Absolute point margin threshold used for the garbage-time filter
#' @return Tibble with one row per team per game: opponent, location, actual result/score,
#'   and expected result/margin derived from cumulative net EPA
get_team_game_log <- function(db_path = "data/cfb.duckdb", min_season = 2013,
                               filter_garbage_time = TRUE, garbage_threshold = 28,
                               version = "v5_drive_xg") {
  if (!file.exists(db_path)) return(tibble::tibble())

  con <- tryCatch(get_duckdb_connection(db_path, read_only = TRUE), error = function(e) NULL)
  if (is.null(con)) return(tibble::tibble())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  if (!DBI::dbExistsTable(con, "schedules")) return(tibble::tibble())
  has_pbp <- DBI::dbExistsTable(con, "pbp")

  if (has_pbp) {
    epa_join <- sprintf("
      LEFT JOIN (
        WITH plays AS (
          SELECT 
            p.game_id, p.period, p.posteam, p.yards_to_goal, p.home_score, p.away_score,
            s2.home_team, s2.away_team,
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
          WHERE s2.season >= %d AND p.posteam IS NOT NULL AND p.yards_to_goal IS NOT NULL
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
          SELECT game_id, side,
            SUM(new_drive) OVER (PARTITION BY game_id ORDER BY period ASC, (COALESCE(clock_minutes, 0) * 60 + COALESCE(clock_seconds, 0)) DESC, CAST(play_id AS BIGINT) ASC) as drive_id,
            yards_to_goal, home_score, away_score
          FROM flagged
        ),
        drive_summary AS (
          SELECT game_id, side, drive_id,
            MIN(yards_to_goal) as min_ytg,
            MAX(CASE WHEN side = 1 THEN home_score ELSE away_score END) - 
            MIN(CASE WHEN side = 1 THEN home_score ELSE away_score END) as pts_gained
          FROM drives
          GROUP BY game_id, side, drive_id
        ),
        drive_xp AS (
          SELECT game_id, side,
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
          ROUND(SUM(CASE WHEN side = 1 THEN xp ELSE 0 END), 2) as home_epa_total,
          ROUND(SUM(CASE WHEN side = 2 THEN xp ELSE 0 END), 2) as away_epa_total
        FROM drive_xp
        GROUP BY game_id
      ) ge ON s.game_id = ge.game_id
    ", as.integer(min_season))
    epa_select <- "ge.home_epa_total, ge.away_epa_total"
  } else {
    epa_join <- ""
    epa_select <- "CAST(NULL AS DOUBLE) AS home_epa_total, CAST(NULL AS DOUBLE) AS away_epa_total"
  }

  q <- sprintf("
    WITH base AS (
      SELECT
        s.season, s.week, s.season_type, s.game_id, s.start_date, s.notes,
        s.home_team, s.home_conference, s.away_team, s.away_conference,
        s.neutral_site, s.home_points, s.away_points,
        %s
      FROM schedules s
      %s
      WHERE s.season >= %d AND s.home_points IS NOT NULL AND s.away_points IS NOT NULL
    ),
    games_home AS (
      SELECT
        season, week, season_type, game_id, start_date, notes,
        home_team AS team, home_conference AS conference,
        away_team AS opponent, away_conference AS opponent_conference,
        CASE WHEN neutral_site THEN 'Neutral' ELSE 'Home' END AS location,
        home_points AS team_points, away_points AS opp_points,
        home_epa_total AS team_epa, away_epa_total AS opp_epa
      FROM base
    ),
    games_away AS (
      SELECT
        season, week, season_type, game_id, start_date, notes,
        away_team AS team, away_conference AS conference,
        home_team AS opponent, home_conference AS opponent_conference,
        CASE WHEN neutral_site THEN 'Neutral' ELSE 'Away' END AS location,
        away_points AS team_points, home_points AS opp_points,
        away_epa_total AS team_epa, home_epa_total AS opp_epa
      FROM base
    )
    SELECT * FROM games_home
    UNION ALL
    SELECT * FROM games_away
    ORDER BY season DESC, team, week
  ", epa_select, epa_join, as.integer(min_season))

  res <- tryCatch(DBI::dbGetQuery(con, q), error = function(e) data.frame())

  if (nrow(res) == 0) return(tibble::tibble())

  # CFBD numbers postseason (bowl/CFP) games starting again from week 1, which otherwise
  # collides with the season's real Week 1 game and, since every postseason game shares
  # week 1, ties all bowl/CFP games together too. Give postseason rows a distinct label
  # and sort them after all regular-season weeks, using start_date to break the tie among
  # them (e.g. CFP first round before quarterfinal before semifinal before championship).
  tibble::as_tibble(res) |>
    dplyr::mutate(
      is_postseason = !is.na(season_type) & season_type == "postseason",
      week_label = dplyr::case_when(
        is_postseason & !is.na(notes) & nzchar(notes) ~ notes,
        is_postseason ~ "Postseason",
        TRUE ~ paste("Week", week)
      ),
      week_sort = ifelse(is_postseason, 9000L, week),
      result = ifelse(team_points > opp_points, "W", "L"),
      margin = as.integer(team_points - opp_points),
      team_epa = round(team_epa, 2),
      opp_epa = round(opp_epa, 2),
      net_epa = round(team_epa - opp_epa, 2),
      expected_result = dplyr::case_when(
        is.na(net_epa) ~ NA_character_,
        net_epa > 0 ~ "W",
        net_epa < 0 ~ "L",
        TRUE ~ "-"
      )
    ) |>
    dplyr::arrange(season, team, week_sort, start_date)
}
