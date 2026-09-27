# ==============================================================================
# R/01_ingest.R
# CFBD REST API -> raw JSON cache (data/raw) -> DuckDB tables.
#
# Every endpoint is fetched once per season and cached on disk. A season is refetched
# only while it is still live (see season_is_final()), and never more often than
# `min_refresh_hours`, because the free CFBD tier allows ~1000 calls per month.
# ==============================================================================

CFBD_BASE <- "https://api.collegefootballdata.com"

cfbd_api_key <- function() {
  if (file.exists(".Renviron")) readRenviron(".Renviron")
  key <- gsub("^[\"']|[\"']$", "", Sys.getenv("CFBD_API_KEY", ""))
  if (!nzchar(key)) stop("CFBD_API_KEY is not set (add it to .Renviron).")
  key
}

#' Single GET against the CFBD API, returning parsed JSON (lists, not simplified)
cfbd_get <- function(path, query = list()) {
  req <- httr2::request(CFBD_BASE) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_auth_bearer_token(cfbd_api_key()) |>
    httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 502, 503, 504))
  resp <- httr2::req_perform(req)
  remaining <- httr2::resp_header(resp, "x-calllimit-remaining")
  if (!is.null(remaining)) message(sprintf("[CFBD] %s %s (calls remaining: %s)", path,
                                           paste(names(query), query, sep = "=", collapse = "&"), remaining))
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' The season currently being played (or most recently played): Aug-Dec -> this year, Jan-Jul -> last year
current_cfb_season <- function(today = Sys.Date()) {
  yr <- as.integer(format(today, "%Y"))
  if (as.integer(format(today, "%m")) >= 8) yr else yr - 1L
}

#' A season is final once it has been synced after Feb 1 of the following year (bowls/CFP done)
season_is_final <- function(season, last_sync) {
  !is.na(last_sync) && as.Date(last_sync) >= as.Date(sprintf("%d-02-01", season + 1L))
}

#' Fetch (or read cached) raw JSON for one endpoint + season
#' @return list(data = parsed JSON, fetched = logical)
cfbd_cached <- function(name, season, path, query, raw_dir = "data/raw",
                        refresh = FALSE, min_refresh_hours = 6) {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(raw_dir, sprintf("%s_%d.json", name, season))
  if (file.exists(file)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(file), units = "hours"))
    if (!refresh || age_h < min_refresh_hours) {
      return(list(data = jsonlite::read_json(file, simplifyVector = FALSE), fetched = FALSE))
    }
  }
  data <- cfbd_get(path, query)
  jsonlite::write_json(data, file, auto_unbox = TRUE, null = "null", digits = NA)
  list(data = data, fetched = TRUE)
}

# ------------------------------------------------------------------------------
# Parsers: raw JSON -> tidy tibbles
# ------------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
int <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)
chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)

#' Regulation margin and OT flag from line scores (periods beyond 4 are overtime)
line_score_summary <- function(home_ls, away_ls) {
  h <- unlist(home_ls); a <- unlist(away_ls)
  if (length(h) < 4 || length(a) < 4) {
    return(list(periods = NA_integer_, ot = NA, reg_margin = NA_real_))
  }
  list(periods = length(h), ot = length(h) > 4, reg_margin = sum(h[1:4]) - sum(a[1:4]))
}

parse_games <- function(raw) {
  rows <- lapply(raw, function(g) {
    ls <- line_score_summary(g$homeLineScores, g$awayLineScores)
    tibble::tibble(
      game_id = as.character(g$id), season = int(g$season), week = int(g$week),
      season_type = chr(g$seasonType), start_date = chr(g$startDate),
      completed = isTRUE(g$completed), neutral_site = isTRUE(g$neutralSite),
      conference_game = isTRUE(g$conferenceGame),
      home_id = int(g$homeId), home_team = chr(g$homeTeam),
      home_conference = chr(g$homeConference), home_class = chr(g$homeClassification),
      home_points = int(g$homePoints),
      away_id = int(g$awayId), away_team = chr(g$awayTeam),
      away_conference = chr(g$awayConference), away_class = chr(g$awayClassification),
      away_points = int(g$awayPoints),
      periods = ls$periods, ot = ls$ot, reg_margin = ls$reg_margin,
      notes = chr(g$notes)
    )
  })
  dplyr::bind_rows(rows)
}

#' Consecutive week index: postseason games (which CFBD numbers from week 1 again) all
#' share week_seq = last regular-season week + 1
add_week_seq <- function(games) {
  last_reg <- max(games$week[games$season_type == "regular"], na.rm = TRUE)
  games |>
    dplyr::mutate(
      week_seq = ifelse(season_type == "postseason", last_reg + 1L, week),
      week_label = ifelse(season_type == "postseason", "Postseason", paste("Week", week))
    )
}

parse_game_stats <- function(raw) {
  if (length(raw) == 0) {
    return(tibble::tibble(game_id = character(0), team = character(0), opponent = character(0),
                          off_plays = integer(0), off_ppa = numeric(0), off_success = numeric(0),
                          def_plays = integer(0), def_ppa = numeric(0)))
  }
  rows <- lapply(raw, function(s) {
    tibble::tibble(
      game_id = as.character(s$gameId), team = chr(s$team), opponent = chr(s$opponent),
      off_plays = int(s$offense$plays), off_ppa = num(s$offense$ppa),
      off_success = num(s$offense$successRate),
      def_plays = int(s$defense$plays), def_ppa = num(s$defense$ppa)
    )
  })
  dplyr::bind_rows(rows) |> dplyr::distinct(game_id, team, .keep_all = TRUE)
}

#' Consensus closing line per game: median spread across providers (home perspective,
#' negative = home favoured, as CFBD reports it)
parse_lines <- function(raw) {
  if (length(raw) == 0) {
    return(tibble::tibble(game_id = character(0), spread = numeric(0), over_under = numeric(0), n_providers = integer(0)))
  }
  rows <- lapply(raw, function(g) {
    spreads <- vapply(g$lines, function(l) num(l$spread), numeric(1))
    totals <- vapply(g$lines, function(l) num(l$overUnder), numeric(1))
    if (length(spreads) == 0) return(NULL)
    tibble::tibble(
      game_id = as.character(g$id),
      spread = if (all(is.na(spreads))) NA_real_ else stats::median(spreads, na.rm = TRUE),
      over_under = if (all(is.na(totals))) NA_real_ else stats::median(totals, na.rm = TRUE),
      n_providers = sum(!is.na(spreads))
    )
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(parse_lines(list()))
  out |> dplyr::filter(!is.na(spread))
}

parse_polls <- function(raw) {
  rows <- list()
  for (wk in raw) {
    for (p in wk$polls) {
      for (r in p$ranks) {
        rows[[length(rows) + 1]] <- tibble::tibble(
          season = int(wk$season), season_type = chr(wk$seasonType), week = int(wk$week),
          poll = chr(p$poll), rank = int(r$rank), team_id = int(r$teamId),
          school = chr(r$school), points = num(r$points)
        )
      }
    }
  }
  dplyr::bind_rows(rows)
}

parse_teams <- function(raw) {
  rows <- lapply(raw, function(t) {
    logos <- unlist(t$logos)
    tibble::tibble(
      team_id = int(t$id), school = chr(t$school), abbreviation = chr(t$abbreviation),
      mascot = chr(t$mascot), classification = chr(t$classification),
      color = chr(t$color), alt_color = chr(t$alternateColor),
      logo = if (length(logos)) logos[grepl("/logos/", logos)][1] %||% logos[1] else NA_character_,
      logo_dark = if (length(logos)) logos[grepl("/logos-dark/", logos)][1] %||% NA_character_ else NA_character_
    )
  })
  dplyr::bind_rows(rows)
}

# ------------------------------------------------------------------------------
# DuckDB store
# ------------------------------------------------------------------------------

db_connect <- function(db_path = "data/cfb.duckdb", read_only = FALSE) {
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = db_path, read_only = read_only)
}

#' Replace one season's rows in a table (creating the table on first write)
db_replace_season <- function(con, table, df, season) {
  if (is.null(df) || nrow(df) == 0) return(invisible(0L))
  if (DBI::dbExistsTable(con, table)) {
    DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE season = %d", table, as.integer(season)))
    DBI::dbAppendTable(con, table, df)
  } else {
    DBI::dbWriteTable(con, table, df)
  }
  invisible(nrow(df))
}

#' Sync one season from CFBD into DuckDB
sync_season <- function(con, season, refresh = FALSE, raw_dir = "data/raw", min_refresh_hours = 6) {
  fetch <- function(name, path, query) {
    cfbd_cached(name, season, path, query, raw_dir = raw_dir,
                refresh = refresh, min_refresh_hours = min_refresh_hours)$data
  }
  # /games without a classification returns FBS-involved games; the FCS call adds
  # FCS-vs-FCS games (and repeats FBS-vs-FCS ones, removed by distinct())
  games <- dplyr::bind_rows(
    parse_games(fetch("games_fbs", "games", list(year = season, seasonType = "both", classification = "fbs"))),
    parse_games(fetch("games_fcs", "games", list(year = season, seasonType = "both", classification = "fcs")))
  ) |>
    dplyr::distinct(game_id, .keep_all = TRUE) |>
    add_week_seq()

  stats <- dplyr::bind_rows(
    parse_game_stats(fetch("stats_regular", "stats/game/advanced",
                           list(year = season, seasonType = "regular", excludeGarbageTime = "true"))),
    parse_game_stats(fetch("stats_postseason", "stats/game/advanced",
                           list(year = season, seasonType = "postseason", excludeGarbageTime = "true")))
  ) |> dplyr::mutate(season = as.integer(season), .after = game_id)

  lines <- parse_lines(fetch("lines", "lines", list(year = season, seasonType = "both"))) |>
    dplyr::mutate(season = as.integer(season), .after = game_id)
  polls <- parse_polls(fetch("rankings", "rankings", list(year = season, seasonType = "both")))

  db_replace_season(con, "games", games, season)
  db_replace_season(con, "game_stats", stats, season)
  db_replace_season(con, "lines", lines, season)
  db_replace_season(con, "polls", polls, season)

  n_done <- sum(games$completed)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS sync_metadata (season INTEGER PRIMARY KEY, last_sync TIMESTAMP, games INTEGER, completed INTEGER)")
  DBI::dbExecute(con, sprintf("INSERT OR REPLACE INTO sync_metadata VALUES (%d, CURRENT_TIMESTAMP, %d, %d)",
                              as.integer(season), nrow(games), as.integer(n_done)))
  invisible(list(season = season, games = nrow(games), completed = n_done))
}

sync_teams <- function(con, refresh = FALSE, raw_dir = "data/raw") {
  season <- current_cfb_season()
  teams <- parse_teams(cfbd_cached("teams", season, "teams", list(), raw_dir = raw_dir,
                                   refresh = refresh, min_refresh_hours = 24 * 30)$data)
  DBI::dbWriteTable(con, "teams", teams, overwrite = TRUE)
  invisible(nrow(teams))
}

#' Drop tables from the pre-2026-09 schema (cfbfastR schedules + play-by-play store)
drop_legacy_tables <- function(con) {
  legacy_meta <- DBI::dbExistsTable(con, "sync_metadata") &&
    !"completed" %in% DBI::dbListFields(con, "sync_metadata")
  if (legacy_meta) DBI::dbExecute(con, "DROP TABLE sync_metadata")
  for (t in c("schedules", "pbp")) {
    if (DBI::dbExistsTable(con, t)) DBI::dbExecute(con, sprintf("DROP TABLE %s", t))
  }
  if (legacy_meta) DBI::dbExecute(con, "CHECKPOINT")
}

#' Sync all seasons. Final seasons already in the store are skipped entirely (no API calls);
#' live seasons are refreshed (subject to min_refresh_hours).
#' @return Fingerprint tibble (season, games, completed): changes only when data changes,
#'   so downstream targets stay cached between runs with nothing new.
sync_all <- function(seasons, db_path = "data/cfb.duckdb", full_refresh = FALSE,
                     raw_dir = "data/raw", min_refresh_hours = 6) {
  con <- db_connect(db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  drop_legacy_tables(con)
  meta <- if (DBI::dbExistsTable(con, "sync_metadata")) DBI::dbReadTable(con, "sync_metadata") else
    data.frame(season = integer(0), last_sync = as.POSIXct(character(0)))

  for (s in seasons) {
    m <- meta[meta$season == s, ]
    have <- nrow(m) == 1
    final <- have && season_is_final(s, m$last_sync)
    if (have && final && !full_refresh) next
    sync_season(con, s, refresh = full_refresh || have, raw_dir = raw_dir,
                min_refresh_hours = if (full_refresh) 0 else min_refresh_hours)
  }
  if (full_refresh || !DBI::dbExistsTable(con, "teams")) sync_teams(con, refresh = full_refresh, raw_dir = raw_dir)
  # CBS Sports 138: archive this week's live ranking while a season is being played
  live <- any(!vapply(seasons, \(s) { m <- meta[meta$season == s, ]; nrow(m) == 1 && season_is_final(s, m$last_sync) }, logical(1)))
  sync_cbs138(con, raw_dir = raw_dir, live = live)

  fp <- DBI::dbGetQuery(con, sprintf(
    "SELECT season, COUNT(*) AS games, SUM(CASE WHEN completed THEN 1 ELSE 0 END) AS completed,
            SUM(COALESCE(home_points, 0) + COALESCE(away_points, 0)) AS points
     FROM games WHERE season IN (%s) GROUP BY season ORDER BY season", paste(seasons, collapse = ",")))
  # New CBS rankings must invalidate that season's data target too
  if (DBI::dbExistsTable(con, "cbs_rankings")) {
    cbs <- DBI::dbGetQuery(con, "SELECT season, COUNT(DISTINCT updated) AS cbs_rankings FROM cbs_rankings GROUP BY season")
    fp <- merge(fp, cbs, all.x = TRUE)
  }
  fp
}

# ------------------------------------------------------------------------------
# Readers
# ------------------------------------------------------------------------------

#' All games for a season, with efficiency (garbage time excluded) attached per side
read_season <- function(season, db_path = "data/cfb.duckdb") {
  con <- db_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  q <- function(sql) tibble::as_tibble(DBI::dbGetQuery(con, sprintf(sql, as.integer(season))))
  games <- q("SELECT * FROM games WHERE season = %d")
  stats <- q("SELECT * FROM game_stats WHERE season = %d")
  lines <- if (DBI::dbExistsTable(con, "lines")) q("SELECT * FROM lines WHERE season = %d") else NULL
  polls <- if (DBI::dbExistsTable(con, "polls")) q("SELECT * FROM polls WHERE season = %d") else NULL
  cbs <- if (DBI::dbExistsTable(con, "cbs_rankings")) q("SELECT * FROM cbs_rankings WHERE season = %d") else NULL

  side <- function(prefix) {
    stats |> dplyr::transmute(game_id, "{prefix}_team" := team,
                              "{prefix}_plays" := off_plays, "{prefix}_ppa" := off_ppa)
  }
  games <- games |>
    dplyr::left_join(side("home"), by = c("game_id", "home_team")) |>
    dplyr::left_join(side("away"), by = c("game_id", "away_team")) |>
    dplyr::left_join(if (!is.null(lines)) dplyr::select(lines, game_id, spread, over_under) else
      tibble::tibble(game_id = character(0), spread = numeric(0), over_under = numeric(0)), by = "game_id") |>
    dplyr::arrange(week_seq, start_date, game_id)

  polls <- dplyr::bind_rows(polls, cbs_as_polls(cbs, games))
  list(season = as.integer(season), games = games, polls = polls)
}

read_teams <- function(db_path = "data/cfb.duckdb") {
  con <- db_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  if (!DBI::dbExistsTable(con, "teams")) return(tibble::tibble())
  tibble::as_tibble(DBI::dbReadTable(con, "teams"))
}

#' Weeks whose games are concluded: the week's last kickoff was >= `buffer_hours` ago and
#' >= `min_share` of its games have results (cancelled games never get one).
concluded_weeks <- function(games, now = Sys.time(), buffer_hours = 6, min_share = 0.9) {
  if (nrow(games) == 0) return(integer(0))
  starts <- as.POSIXct(games$start_date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  wk <- games |>
    dplyr::mutate(start = starts) |>
    dplyr::group_by(week_seq) |>
    dplyr::summarise(last_start = max(start, na.rm = TRUE),
                     share = mean(completed & !is.na(home_points)), n_done = sum(completed), .groups = "drop") |>
    dplyr::arrange(week_seq)
  ok <- wk$n_done > 0 & wk$share >= min_share &
    as.numeric(difftime(now, wk$last_start, units = "hours")) >= buffer_hours
  # Only a contiguous prefix of weeks counts (a later week can't be done if an earlier one isn't)
  n_ok <- if (all(ok)) length(ok) else which(!ok)[1] - 1L
  as.integer(wk$week_seq[seq_len(n_ok)])
}
