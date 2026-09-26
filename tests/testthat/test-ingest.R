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
