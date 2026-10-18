# Conference games as (home, away, home won) over local team indices
cg <- function(...) {
  x <- matrix(c(...), ncol = 3, byrow = TRUE)
  list(home = x[, 1], away = x[, 2], win = x[, 3] == 1)
}

test_that("two-way tie goes to the head-to-head winner, not the rating", {
  g <- cg(1, 2, 0,   # 2 beat 1
          1, 3, 1, 2, 4, 1, 3, 4, 1)
  expect_equal(tiebreak_first(c(1L, 2L), g$home, g$away, g$win, rating = c(10, 0, 0, 0)), 2L)
})

test_that("three-way tie: full round robin uses the mini-standings, then restarts", {
  # 1 beat 2, 2 beat 3, 3 beat 1: all 1-1, so h2h does not separate them; common opponent 4:
  # 1 and 2 beat 4, 3 lost to 4 -> 1 and 2 remain, and the restart uses 1's win over 2
  g <- cg(1, 2, 1, 2, 3, 1, 3, 1, 1,
          1, 4, 1, 2, 4, 1, 4, 3, 1)
  expect_equal(tiebreak_first(1:3, g$home, g$away, g$win, rating = c(0, 10, 5, 0)), 1L)
})

test_that("incomplete round robin: only a team that beat all the others wins head-to-head", {
  # 1 beat 2 and 3; 2 and 3 did not meet
  g <- cg(1, 2, 1, 3, 1, 0)
  expect_equal(tiebreak_first(1:3, g$home, g$away, g$win, rating = c(0, 10, 5)), 1L)
  # nobody beat everyone, no common opponents: the rating decides
  g2 <- cg(1, 2, 1, 4, 3, 1)
  expect_equal(tiebreak_first(1:3, g2$home, g2$away, g2$win, rating = c(0, 10, 5)), 2L)
})

test_that("seeds: top two by win %, or division winners with the better record as seed 1", {
  g <- cg(1, 2, 1, 3, 4, 1, 1, 3, 1, 2, 4, 1)
  pct <- c(1, 0.5, 0.5, 0)
  expect_equal(conference_seeds(pct, g$home, g$away, g$win, rating = c(0, 0, 9, 0)), c(1L, 3L))
  expect_equal(conference_seeds(pct, g$home, g$away, g$win, rating = c(0, 0, 9, 0),
                                division = c("E", "W", "E", "W")), c(1L, 2L))
})

test_that("bowl eligibility counts one non-FBS win and needs a .500 record", {
  expect_true(bowl_eligible(wins = 6, wins_fbs = 5, games = 12))
  expect_false(bowl_eligible(wins = 6, wins_fbs = 4, games = 12))   # two FCS wins
  expect_true(bowl_eligible(wins = 7, wins_fbs = 5, games = 12))
  expect_false(bowl_eligible(wins = 6, wins_fbs = 6, games = 13))   # 6-7
})

test_that("CFP field: five best champions get in even when ranked low, seeds follow rank", {
  rk <- 1:20
  f <- cfp_field(rk, champs = c(1, 2, 18, 19, 20, 17))
  expect_equal(sum(!is.na(f$seed)), 12)
  expect_setequal(f$auto, c(1, 2, 17, 18, 19))
  expect_true(is.na(f$seed[20]))                 # sixth champion misses out
  expect_equal(which(!is.na(f$seed)), c(1:9, 17:19))   # seven at-large: ranks 3-9
  expect_equal(f$seed[c(1:9, 17:19)], 1:12)
})
