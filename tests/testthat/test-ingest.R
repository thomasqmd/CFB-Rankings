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
