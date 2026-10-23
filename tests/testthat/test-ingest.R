test_that("OT and regulation margin come from line scores, not notes", {
  reg <- line_score_summary(list(7, 7, 0, 3), list(0, 0, 3, 7))
  expect_false(reg$ot); expect_equal(reg$reg_margin, 7)
  ot <- line_score_summary(list(7, 7, 0, 3, 7), list(0, 10, 0, 7, 0))
  expect_true(ot$ot); expect_equal(ot$reg_margin, 0)
  expect_true(is.na(line_score_summary(NULL, NULL)$ot))
})

test_that("postseason games get their own week after the regular season", {
  g <- tibble::tibble(week = c(1L, 14L, 1L, 2L), season_type = c("regular", "regular", "postseason", "postseason"))
  out <- add_week_seq(g)
  expect_equal(out$week_seq, c(1L, 14L, 15L, 15L))
  expect_equal(out$week_label[3], "Postseason")
})

test_that("week 0 games get their own week and later weeks move up one", {
  # 2026: a weekend of games (Aug 29, plus a Sunday-UTC kickoff that is Saturday night Eastern)
  # filed under week 1, whose main Saturday is Sep 5; a Thursday Sep 3 opener stays in week 1
  g <- tibble::tibble(
    week = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L), season_type = c(rep("regular", 7), "postseason"),
    start_date = c("2026-08-29T17:00:00.000Z", "2026-08-30T02:30:00.000Z", "2026-09-03T23:00:00.000Z",
                   "2026-09-05T16:00:00.000Z", "2026-09-05T19:30:00.000Z", "2026-09-06T00:00:00.000Z",
                   "2026-09-12T16:00:00.000Z", "2026-12-20T20:00:00.000Z"))
  out <- add_week_seq(g)
  expect_equal(out$week_seq, c(1L, 1L, 2L, 2L, 2L, 2L, 3L, 4L))
  expect_equal(out$week_label[1:3], c("Week 0", "Week 0", "Week 1"))
  # No early games: weeks unchanged
  expect_equal(add_week_seq(g[3:7, ])$week_seq, c(1L, 1L, 1L, 1L, 2L))

  polls <- tibble::tibble(season_type = c("regular", "regular", "postseason"), week = c(1L, 2L, 1L), rank = 1L)
  p <- polls_on_week_seq(polls, out)
  expect_equal(p$week[p$season_type == "regular"], c(1L, 2L, 3L))   # preseason poll for snapshots 0 and 1
  expect_equal(p$week[p$season_type == "postseason"], 1L)
  expect_identical(polls_on_week_seq(polls, add_week_seq(g[3:7, ])), polls)
})

test_that("a season is final only after a sync on/after Feb 1 of the next year", {
  expect_false(season_is_final(2025, as.POSIXct("2026-01-10")))
  expect_true(season_is_final(2025, as.POSIXct("2026-02-02")))
  expect_false(season_is_final(2025, NA))
})

test_that("current season rolls over in August", {
  expect_equal(current_cfb_season(as.Date("2026-07-31")), 2025L)
  expect_equal(current_cfb_season(as.Date("2026-08-01")), 2026L)
})

test_that("concluded weeks need finished games and a buffer after the last kickoff", {
  games <- tibble::tibble(
    week_seq = c(1L, 1L, 2L, 2L, 3L),
    start_date = c("2026-09-05T16:00:00.000Z", "2026-09-05T23:00:00.000Z",
                   "2026-09-12T16:00:00.000Z", "2026-09-12T23:00:00.000Z", "2026-09-19T16:00:00.000Z"),
    completed = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    home_points = c(10L, 20L, 30L, NA, NA)
  )
  now <- as.POSIXct("2026-09-13T12:00:00", tz = "UTC")
  expect_equal(concluded_weeks(games, now = now), 1L)
  # a later week can't count while an earlier one is unfinished
  games$completed[5] <- TRUE; games$home_points[5] <- 3L
  expect_equal(concluded_weeks(games, now = as.POSIXct("2026-09-25", tz = "UTC")), 1L)
  expect_equal(concluded_weeks(games, now = as.POSIXct("2026-09-25", tz = "UTC"), min_share = 0.5), 1:3)
})

test_that("a week counts before the buffer once every FBS game is final with stats", {
  games <- tibble::tibble(
    week_seq = 1L, start_date = c("2026-09-05T16:00:00.000Z", "2026-09-06T03:00:00.000Z", "2026-09-05T18:00:00.000Z"),
    completed = TRUE, home_points = c(10L, 20L, 30L),
    home_class = c("fbs", "fbs", "fcs"), away_class = c("fbs", "fcs", "fcs"),
    home_ppa = c(0.1, 0.2, NA), away_ppa = c(-0.1, NA, NA)
  )
  soon <- as.POSIXct("2026-09-06 05:00:00", tz = "UTC")      # 2h after the last kickoff
  expect_equal(concluded_weeks(games, now = soon), 1L)          # FCS-only game needs no stats
  games$home_ppa[2] <- NA                                       # an FBS game still without stats
  expect_equal(concluded_weeks(games, now = soon), integer(0))
  expect_equal(concluded_weeks(games, now = soon + 5 * 3600), 1L)  # the buffer still applies
})

test_that("spring games follow the fall postseason in date order", {
  g <- tibble::tibble(week = c(1L, 14L, 1L, 1L, 3L, 1L),
                      season_type = c("regular", "regular", "postseason", "spring_regular", "spring_regular", "spring_postseason"))
  out <- add_week_seq(g)
  expect_equal(out$week_seq, c(1L, 14L, 15L, 16L, 18L, 19L))
  expect_equal(out$week_label[c(4, 6)], c("Spring week 1", "Spring postseason"))
})

test_that("the last complete season turns over on Feb 1", {
  expect_equal(last_complete_season(as.Date("2026-09-27")), 2025L)
  expect_equal(last_complete_season(as.Date("2027-01-20")), 2025L)
  expect_equal(last_complete_season(as.Date("2027-02-01")), 2026L)
  expect_equal(last_complete_season(as.Date("2027-07-31")), 2026L)
})

test_that("returning production and talent parse to typed columns, talent may be a string", {
  ret <- parse_returning(list(list(season = 2025, team = "Air Force", percentPPA = 0.63, usage = 0.556)))
  expect_equal(ret$season, 2025L); expect_equal(ret$ret_ppa, 0.63); expect_equal(ret$ret_usage, 0.556)
  tal <- parse_talent(list(list(year = 2025, team = "Georgia", talent = 1002.98),
                           list(year = 2025, team = "Alabama", talent = "993.55")))
  expect_equal(tal$talent, c(1002.98, 993.55)); expect_equal(tal$team, c("Georgia", "Alabama"))
})

test_that("coaches parse to one row per coach-season entry", {
  raw <- list(list(id = 10, seasons = list(list(teamId = 5, year = 2024), list(teamId = 6, year = 2025))))
  co <- parse_coaches(raw)
  expect_equal(co$season, c(2024L, 2025L)); expect_equal(co$team_id, c(5L, 6L)); expect_equal(co$coach_id, c(10L, 10L))
})

test_that("preseason parsers return typed zero-row tibbles for empty input", {
  types <- function(x) vapply(x, typeof, character(1))
  r <- parse_returning(list())
  expect_equal(nrow(r), 0); expect_equal(types(r), c(season = "integer", team = "character", ret_ppa = "double", ret_usage = "double"))
  tal <- parse_talent(list())
  expect_equal(nrow(tal), 0); expect_equal(types(tal), c(season = "integer", team = "character", talent = "double"))
  co <- parse_coaches(list())
  expect_equal(nrow(co), 0); expect_equal(types(co), c(season = "integer", team_id = "integer", coach_id = "integer"))
})

test_that("coach change: kept coach 0, new coach 1, same coach id on the same team 0, absent team NA", {
  co <- function(team, coach) tibble::tibble(season = 2025L, team_id = as.integer(team), coach_id = as.integer(coach))
  prev <- dplyr::bind_rows(co(1, 10), co(2, 20), co(3, 30), co(5, 50), co(6, 60))
  now <- dplyr::bind_rows(co(1, 10), co(2, 21), co(3, 30), co(4, 40), co(6, 50))
  out <- coach_changes(now, prev)
  expect_equal(out$team_id, c(1L, 2L, 3L, 4L, 6L))
  expect_equal(out$coach_change, c(0L, 1L, 0L, NA, 1L))
  expect_type(out$coach_change, "integer")
  expect_true(all(is.na(coach_changes(now, parse_coaches(list()))$coach_change)))
})

test_that("parse_sp keeps teams' overall, offence and defence SP+ and drops the national averages", {
  raw <- list(
    list(team = "Georgia", rating = 25.1, ranking = 2L, offense = list(rating = 38.2), defense = list(rating = 13.4)),
    list(team = "nationalAverages", rating = 0, ranking = NULL, offense = list(rating = 28), defense = list(rating = 28)),
    list(team = "Ohio State", rating = 27.9, ranking = 1L, offense = list(rating = 40.0), defense = list(rating = 12.0)),
    list(team = "Unrated", rating = NULL, ranking = NULL, offense = list(), defense = list())
  )
  sp <- parse_sp(raw)
  expect_equal(sp$team, c("Georgia", "Ohio State"))
  expect_equal(sp$sp_rank, c(2L, 1L))
  expect_equal(sp$sp_off, c(38.2, 40.0))
  expect_equal(nrow(parse_sp(list())), 0)
})

test_that("a repeated school name goes to its highest classification", {
  # CFBD lists an unclassified Charlotte (3253) before the FBS one (2429)
  teams <- tibble::tibble(team_id = c(3253L, 2429L, 3237L, 2653L, 10L, 11L, 12L),
                          school = c("Charlotte", "Charlotte", "Troy", "Troy", "Albany", "Albany", "Solo"),
                          classification = c(NA, "fbs", NA, "fbs", "ii", "fcs", NA))
  expect_equal(school_team_id(c("Troy", "Charlotte", "Albany", "Solo", "Nowhere"), teams),
               c(2653L, 2429L, 11L, 12L, NA))
})

test_that("data integrity flags a season out of line with the ones before it", {
  # n_fbs FBS-vs-FBS games and n_low FCS-vs-FCS games, all in week 1 and finished
  season <- function(s, n_fbs = 10, n_low = 6, talent = TRUE, dup = FALSE) {
    mk <- function(i, cls, id0) tibble::tibble(
      game_id = paste(s, cls, i), season = s, week_seq = 1L, completed = TRUE,
      start_date = sprintf("%d-09-0%dT18:00:00.000Z", s, 1 + i %% 5),
      home_id = id0 + 2L * i, away_id = id0 + 2L * i + 1L, home_class = cls, away_class = cls,
      home_points = 21L, away_points = 14L, home_ppa = 0.2, away_ppa = 0.1, spread = -3)
    g <- dplyr::bind_rows(mk(seq_len(n_fbs), "fbs", 0L), mk(seq_len(n_low), "fcs", 1000L))
    if (dup) g <- dplyr::bind_rows(g, dplyr::mutate(g[1, ], game_id = "copy"))
    fbs <- unique(c(g$home_id[g$home_class == "fbs"], g$away_id[g$away_class == "fbs"]))
    list(season = as.integer(s), games = g,
         preseason = tibble::tibble(team_id = fbs, ret_ppa = 0.6, talent = if (talent) 700 else NA_real_))
  }
  past <- lapply(2021:2024, season)
  ok <- data_integrity(c(past, list(season(2025))))
  expect_true(all(ok$level[ok$season == 2025] %in% c("PASS", "INFO")))

  live <- season(2025, n_low = 9, talent = FALSE, dup = TRUE)
  expect_warning(out <- data_integrity(c(past, list(live))), "Games without an FBS team")
  lv <- out |> dplyr::filter(season == 2025) |> dplyr::select(metric, level) |> tibble::deframe()
  expect_equal(unname(lv[c("Games without an FBS team", "Talent coverage (FBS)", "Duplicate games",
                           "FBS games", "Line coverage (FBS vs FBS)")]),
               c("WARN", "WARN", "FAIL", "PASS", "PASS"))
  expect_true(all(out$level[out$season == 2021] == "INFO" | out$kind[out$season == 2021] == "defect"))
})
