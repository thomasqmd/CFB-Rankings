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

#' The last season whose postseason is over (Feb 1 of the following year, the same rule as
#' season_is_final()): the paper's worked example and its supplementary refits both use it
last_complete_season <- function(today = Sys.Date()) {
  s <- current_cfb_season(today)
  if (today >= as.Date(sprintf("%d-02-01", s + 1L))) s else s - 1L
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

#' Week 0 games: CFBD files the games before the first full weekend under week 1 (in 2026 a whole
#' weekend, Aug 27-30, of mostly FCS games). They are the week-1 games kicked off (US Eastern)
#' before the Tuesday of week 1's busiest Saturday.
week0_games <- function(games) {
  w1 <- games$season_type == "regular" & games$week == 1L
  if (!"start_date" %in% names(games) || !any(w1)) return(rep(FALSE, nrow(games)))
  day <- as.Date(as.POSIXct(games$start_date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), tz = "America/New_York")
  sat <- day[w1 & !is.na(day) & format(day, "%u") == "6"]
  if (!length(sat)) return(rep(FALSE, nrow(games)))
  main <- as.Date(names(which.max(table(sat))))
  w1 & !is.na(day) & day < main - 4
}

#' Consecutive week index: postseason games (which CFBD numbers from week 1 again) all
#' share week_seq = last regular-season week + 1. Spring games (the 2020 FCS season, played
#' Feb-May 2021) follow the fall postseason in date order: spring week w is week_seq
#' last_reg + 1 + w, and the spring postseason comes after the last spring week.
#' A season with week 0 games (week0_games()) gives them week_seq 1 and moves every later week
#' up one, so week 0's results inform the week-1 predictions; CFBD's week stays in `week`.
add_week_seq <- function(games) {
  wk0 <- week0_games(games)
  shift <- as.integer(any(wk0))
  last_reg <- max(games$week[games$season_type == "regular"], na.rm = TRUE) + shift
  spring <- games$season_type == "spring_regular"
  last_spring <- if (any(spring)) max(games$week[spring], na.rm = TRUE) else 0L
  games |>
    dplyr::mutate(
      week_seq = dplyr::case_when(
        season_type == "postseason" ~ last_reg + 1L,
        season_type == "spring_regular" ~ last_reg + 1L + week,
        season_type == "spring_postseason" ~ last_reg + 2L + last_spring,
        wk0 ~ 1L,
        TRUE ~ week + shift
      ),
      week_label = dplyr::case_when(
        season_type == "postseason" ~ "Postseason",
        season_type == "spring_regular" ~ paste("Spring week", week),
        season_type == "spring_postseason" ~ "Spring postseason",
        wk0 ~ "Week 0",
        TRUE ~ paste("Week", week)
      )
    )
}

#' AP-style polls on the week_seq scale: poll_for_week() pairs snapshot w with regular poll w + 1,
#' and CFBD numbers polls by its own weeks (poll 1 = preseason). With a week 0 every poll moves up
#' one, and the preseason poll also stands for the preseason snapshot (no poll follows week 0).
polls_on_week_seq <- function(polls, games) {
  if (is.null(polls) || nrow(polls) == 0 || !any(games$week_label == "Week 0")) return(polls)
  reg <- polls$season_type == "regular"
  pre <- polls[reg & polls$week == 1L, ]
  polls$week[reg] <- polls$week[reg] + 1L
  dplyr::bind_rows(pre, polls)
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

#' Returning production per team-season (percent of PPA and of usage)
parse_returning <- function(raw) {
  if (length(raw) == 0) {
    return(tibble::tibble(season = integer(0), team = character(0), ret_ppa = numeric(0), ret_usage = numeric(0)))
  }
  rows <- lapply(raw, function(r) {
    tibble::tibble(season = int(r$season), team = chr(r$team), ret_ppa = num(r$percentPPA), ret_usage = num(r$usage))
  })
  dplyr::bind_rows(rows)
}

#' Talent composite per team-season (num() also reads the string form some years use)
parse_talent <- function(raw) {
  if (length(raw) == 0) {
    return(tibble::tibble(season = integer(0), team = character(0), talent = numeric(0)))
  }
  rows <- lapply(raw, function(t) {
    tibble::tibble(season = int(t$year), team = chr(t$team), talent = num(t$talent))
  })
  dplyr::bind_rows(rows)
}

#' SP+ per team (CFBD /ratings/sp, FBS only): overall, offence and defence ratings and the overall rank
parse_sp <- function(raw) {
  rows <- lapply(raw, function(x) {
    if (identical(chr(x$team), "nationalAverages")) return(NULL)
    tibble::tibble(team = chr(x$team), sp = num(x$rating), sp_rank = int(x$ranking),
                   sp_off = num(x$offense$rating), sp_def = num(x$defense$rating))
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) {
    return(tibble::tibble(team = character(0), sp = numeric(0), sp_rank = integer(0), sp_off = numeric(0), sp_def = numeric(0)))
  }
  dplyr::filter(out, !is.na(sp))
}

#' One row per coach per season entry (a coach who moved teams has one row for each team)
parse_coaches <- function(raw) {
  rows <- lapply(raw, function(coach) {
    dplyr::bind_rows(lapply(coach$seasons, function(s) {
      tibble::tibble(season = int(s$year), team_id = int(s$teamId), coach_id = int(coach$id))
    }))
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(tibble::tibble(season = integer(0), team_id = integer(0), coach_id = integer(0)))
  out
}

#' Coach change per team: 1 when none of the season's coaches also coached that team in `prev`
#' (last season's parse_coaches()), NA when the team has no rows in `prev`
coach_changes <- function(coaches, prev) {
  ids <- unique(coaches$team_id)
  change <- vapply(ids, function(id) {
    before <- prev$coach_id[prev$team_id %in% id]
    if (length(before) == 0) return(NA_integer_)
    as.integer(!any(coaches$coach_id[coaches$team_id %in% id] %in% before))
  }, integer(1))
  tibble::tibble(team_id = as.integer(ids), coach_change = unname(change))
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

#' team_id for each school name. CFBD's teams list repeats some names (an FBS Charlotte and an
#' unclassified one), so a name goes to its highest classification: FBS, FCS, II, III, then none.
school_team_id <- function(school, teams) {
  rank <- match(teams$classification, c("fbs", "fcs", "ii", "iii"), nomatch = 5L)
  t <- teams[order(rank), ]
  t$team_id[match(school, t$school)]
}

sync_teams <- function(con, refresh = FALSE, raw_dir = "data/raw") {
  season <- current_cfb_season()
  teams <- parse_teams(cfbd_cached("teams", season, "teams", list(), raw_dir = raw_dir,
                                   refresh = refresh, min_refresh_hours = 24 * 30)$data)
  DBI::dbWriteTable(con, "teams", teams, overwrite = TRUE)
  invisible(nrow(teams))
}

#' Preseason inputs (returning production, talent, coach change) for seasons not yet stored, or
#' stored without returning production or (from its first season) talent. A season whose
#' response is empty is skipped with a warning and its cached empty response dropped, so a later
#' sync asks again.
sync_preseason <- function(con, seasons, raw_dir = "data/raw") {
  if (!DBI::dbExistsTable(con, "teams")) return(invisible(0L))
  have <- if (DBI::dbExistsTable(con, "preseason")) {
    DBI::dbGetQuery(con, sprintf(
      "SELECT season FROM preseason GROUP BY season HAVING COUNT(ret_ppa) > 0 AND (season < %d OR COUNT(talent) > 0)",
      PRESEASON_FIRST_SEASON[["tal"]]))$season
  } else integer(0)
  teams <- DBI::dbReadTable(con, "teams")
  fetch <- function(name, path, s) cfbd_cached(name, s, path, list(year = s), raw_dir = raw_dir, refresh = FALSE)$data
  # An empty cached response may predate publication: ask the API once more before giving up
  fetch_nonempty <- function(name, path, s) {
    file <- file.path(raw_dir, sprintf("%s_%d.json", name, s))
    d <- fetch(name, path, s)
    if (length(d) == 0) { unlink(file); d <- fetch(name, path, s) }
    if (length(d) == 0) unlink(file)
    d
  }
  build <- function(s) {
    ret <- parse_returning(fetch_nonempty("returning", "player/returning", s))
    if (nrow(ret) == 0) {
      warning("[preseason] ", s, " skipped: no returning production yet", call. = FALSE)
      return(NULL)
    }
    talent <- parse_talent(if (s >= PRESEASON_FIRST_SEASON[["tal"]]) fetch_nonempty("talent", "talent", s) else list())
    if (nrow(talent) == 0 && s >= PRESEASON_FIRST_SEASON[["tal"]]) {
      warning("[preseason] ", s, " skipped: no talent composite yet", call. = FALSE)
      return(NULL)
    }
    coaches <- parse_coaches(fetch("coaches", "coaches", s))
    prev <- parse_coaches(fetch("coaches", "coaches", s - 1L))
    ret$team_id <- school_team_id(ret$team, teams)
    talent$team_id <- school_team_id(talent$team, teams)
    dplyr::full_join(dplyr::select(ret, team_id, ret_ppa, ret_usage), dplyr::select(talent, team_id, talent), by = "team_id") |>
      dplyr::full_join(coach_changes(coaches, prev), by = "team_id") |>
      dplyr::filter(team_id %in% teams$team_id) |>
      dplyr::mutate(season = as.integer(s), .before = team_id)
  }
  n <- 0L
  for (s in setdiff(seasons, have)) {
    df <- tryCatch(build(s), error = \(e) {
      warning("[preseason] ", s, " skipped: ", conditionMessage(e), call. = FALSE)
      NULL
    })
    n <- n + db_replace_season(con, "preseason", df, s)
  }
  invisible(n)
}

#' SP+ for each season. CFBD serves only a season's latest SP+ (no weekly history), so a finished
#' season is stored once (its final ratings) and a live season is refetched like the games (at most
#' every `min_refresh_hours`). A version is stored only when the ratings change, stamped with the
#' time it was fetched; readers use each season's latest version.
sync_sp <- function(con, seasons, live, raw_dir = "data/raw", min_refresh_hours = 6) {
  if (!DBI::dbExistsTable(con, "teams")) return(invisible(0L))
  teams <- DBI::dbReadTable(con, "teams")
  stored <- if (DBI::dbExistsTable(con, "sp_ratings")) DBI::dbReadTable(con, "sp_ratings") else NULL
  n <- 0L
  for (s in seasons) {
    is_live <- s %in% live
    if (!is_live && !is.null(stored) && s %in% stored$season) next
    raw <- tryCatch(cfbd_cached("ratings_sp", s, "ratings/sp", list(year = s), raw_dir = raw_dir,
                                refresh = is_live, min_refresh_hours = min_refresh_hours)$data,
                    error = \(e) { warning("[SP+] ", s, " skipped: ", conditionMessage(e), call. = FALSE); NULL })
    sp <- parse_sp(raw)
    if (nrow(sp) == 0) next
    sp <- sp |>
      dplyr::mutate(team_id = school_team_id(team, teams)) |>
      dplyr::filter(!is.na(team_id)) |>
      dplyr::transmute(season = as.integer(s), fetched = file.mtime(file.path(raw_dir, sprintf("ratings_sp_%d.json", s))),
                       team_id, sp, sp_rank, sp_off, sp_def) |>
      dplyr::arrange(team_id)
    if (!is.null(stored)) {
      prev <- stored[stored$season == s, ]
      prev <- prev[prev$fetched == max(prev$fetched), ] |> dplyr::arrange(team_id)
      if (nrow(prev) == nrow(sp) && isTRUE(all.equal(prev$sp, sp$sp)) && all(prev$team_id == sp$team_id)) next
    }
    if (DBI::dbExistsTable(con, "sp_ratings")) DBI::dbAppendTable(con, "sp_ratings", sp) else DBI::dbWriteTable(con, "sp_ratings", sp)
    n <- n + nrow(sp)
  }
  invisible(n)
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
  sync_preseason(con, seasons, raw_dir = raw_dir)
  live_seasons <- seasons[!vapply(seasons, \(s) { m <- meta[meta$season == s, ]; nrow(m) == 1 && season_is_final(s, m$last_sync) }, logical(1))]
  sync_sp(con, seasons, live = live_seasons, raw_dir = raw_dir, min_refresh_hours = if (full_refresh) 0 else min_refresh_hours)
  # CBS Sports 138: archive this week's live ranking while a season is being played
  live <- length(live_seasons) > 0
  sync_cbs138(con, raw_dir = raw_dir, live = live)

  fp <- DBI::dbGetQuery(con, sprintf(
    "SELECT season, COUNT(*) AS games, SUM(CASE WHEN completed THEN 1 ELSE 0 END) AS completed,
            SUM(COALESCE(home_points, 0) + COALESCE(away_points, 0)) AS points
     FROM games WHERE season IN (%s) GROUP BY season ORDER BY season", paste(seasons, collapse = ",")))
  # New polls and CBS rankings must invalidate that season's data target too (the AP poll comes
  # out on Sunday afternoon, often after a sync that already has every game of the week)
  if (DBI::dbExistsTable(con, "polls")) {
    pl <- DBI::dbGetQuery(con, "SELECT season, COUNT(*) AS polls FROM polls GROUP BY season")
    fp <- merge(fp, pl, all.x = TRUE)
  }
  if (DBI::dbExistsTable(con, "cbs_rankings")) {
    cbs <- DBI::dbGetQuery(con, "SELECT season, COUNT(DISTINCT updated) AS cbs_rankings FROM cbs_rankings GROUP BY season")
    fp <- merge(fp, cbs, all.x = TRUE)
  }
  if (DBI::dbExistsTable(con, "preseason")) {
    # Counts of non-missing values too, so filling a season's talent re-reads that season
    pre <- DBI::dbGetQuery(con, paste("SELECT season, COUNT(*) AS preseason, COUNT(ret_ppa) AS preseason_ret,",
                                      "COUNT(talent) AS preseason_tal FROM preseason GROUP BY season"))
    fp <- merge(fp, pre, all.x = TRUE)
  }
  if (DBI::dbExistsTable(con, "sp_ratings")) {
    spv <- DBI::dbGetQuery(con, "SELECT season, COUNT(DISTINCT fetched) AS sp_versions FROM sp_ratings GROUP BY season")
    fp <- merge(fp, spv, all.x = TRUE)
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
  # week_seq is recomputed here, so a change to its rule applies without refetching a season
  games <- q("SELECT * FROM games WHERE season = %d") |> add_week_seq()
  stats <- q("SELECT * FROM game_stats WHERE season = %d")
  lines <- if (DBI::dbExistsTable(con, "lines")) q("SELECT * FROM lines WHERE season = %d") else NULL
  polls <- if (DBI::dbExistsTable(con, "polls")) q("SELECT * FROM polls WHERE season = %d") else NULL
  cbs <- if (DBI::dbExistsTable(con, "cbs_rankings")) q("SELECT * FROM cbs_rankings WHERE season = %d") else NULL
  preseason <- if (DBI::dbExistsTable(con, "preseason")) {
    q("SELECT team_id, ret_ppa, ret_usage, talent, coach_change FROM preseason WHERE season = %d")
  } else {
    tibble::tibble(team_id = integer(0), ret_ppa = numeric(0), ret_usage = numeric(0),
                   talent = numeric(0), coach_change = integer(0))
  }

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

  # CBS rankings are aligned to week_seq already (cbs_as_polls() dates them against the games)
  polls <- dplyr::bind_rows(polls_on_week_seq(polls, games), cbs_as_polls(cbs, games))
  list(season = as.integer(season), games = games, polls = polls, preseason = preseason)
}

#' Each season's latest stored SP+ (final ratings for finished seasons, the newest fetch for a live one)
read_sp <- function(db_path = "data/cfb.duckdb") {
  con <- db_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  if (!DBI::dbExistsTable(con, "sp_ratings")) {
    return(tibble::tibble(season = integer(0), fetched = as.POSIXct(character(0)), team_id = integer(0),
                          sp = numeric(0), sp_rank = integer(0), sp_off = numeric(0), sp_def = numeric(0)))
  }
  tibble::as_tibble(DBI::dbGetQuery(con, "SELECT s.* FROM sp_ratings s
    WHERE s.fetched = (SELECT MAX(t.fetched) FROM sp_ratings t WHERE t.season = s.season)"))
}

read_teams <- function(db_path = "data/cfb.duckdb") {
  con <- db_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  if (!DBI::dbExistsTable(con, "teams")) return(tibble::tibble())
  tibble::as_tibble(DBI::dbReadTable(con, "teams"))
}

#' Weeks whose games are concluded: >= `min_share` of its games have results (cancelled games never
#' get one), and either the week's last kickoff was >= `buffer_hours` ago or every FBS game in it is
#' final with its advanced stats. The buffer exists to wait for those stats (CFBD posts them after
#' the score), so a week whose stats are all in counts without waiting it out.
concluded_weeks <- function(games, now = Sys.time(), buffer_hours = 6, min_share = 0.9) {
  if (nrow(games) == 0) return(integer(0))
  starts <- as.POSIXct(games$start_date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  # Stats count as posted when either side has them (CFBD may never post an FCS opponent's side)
  with_stats <- all(c("home_class", "away_class", "home_ppa", "away_ppa") %in% names(games))
  fbs_game <- if (with_stats) games$home_class %in% "fbs" | games$away_class %in% "fbs" else rep(FALSE, nrow(games))
  ready <- if (with_stats) games$completed & (!is.na(games$home_ppa) | !is.na(games$away_ppa)) else rep(FALSE, nrow(games))
  wk <- games |>
    dplyr::mutate(start = starts, fbs_game = fbs_game, ready = ready) |>
    dplyr::group_by(week_seq) |>
    dplyr::summarise(last_start = max(start, na.rm = TRUE),
                     share = mean(completed & !is.na(home_points)), n_done = sum(completed),
                     stats_in = any(fbs_game) && all(ready[fbs_game]), .groups = "drop") |>
    dplyr::arrange(week_seq)
  ok <- wk$n_done > 0 & wk$share >= min_share &
    (as.numeric(difftime(now, wk$last_start, units = "hours")) >= buffer_hours | wk$stats_in)
  # Only a contiguous prefix of weeks counts (a later week can't be done if an earlier one isn't)
  n_ok <- if (all(ok)) length(ok) else which(!ok)[1] - 1L
  as.integer(wk$week_seq[seq_len(n_ok)])
}
