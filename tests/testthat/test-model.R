test_that("a team moving up a tier keeps its absolute level in d_i, with its sd", {
  teams <- tibble::tibble(team_id = 1:4, tier = c(1L, 1L, 1L, 2L))
  prior <- tibble::tibble(team_id = c(1L, 2L, 3L, 4L), rating = c(10, -10, -25, -30), rating_sd = c(2, 2, 3, 4))
  # team 3 was FCS last season (rating -25) and is FBS now
  d <- prev_deviation(teams, prior)
  expect_equal(d$dev[1:3], c(10, -10, -25) - mean(c(10, -10, -25)))
  expect_equal(d$dev[4], 0)                    # alone in its tier
  expect_equal(d$sd, c(2, 2, 3, 4))
  none <- prev_deviation(teams, NULL)
  expect_equal(none$dev, rep(0, 4)); expect_equal(none$sd, rep(0, 4))
})

test_that("each independent gets its own conference effect; lower divisions share one", {
  games <- tibble::tibble(
    home_id = c(1L, 3L, 5L), home_team = c("A", "C", "E"), home_conference = c("FBS Independents", NA, "SEC"),
    home_class = c("fbs", "fbs", "fbs"),
    away_id = c(2L, 4L, 6L), away_team = c("B", "D", "F"), away_conference = c("SEC", NA, NA),
    away_class = c("fbs", "ii", "iii"), week_seq = 1L)
  tm <- season_teams(games)
  keys <- setNames(tm$conf_key, tm$team)
  expect_false(keys[["A"]] == keys[["C"]])    # two FBS independents, two effects
  expect_equal(keys[["D"]], keys[["F"]])      # non-D1 pooled
  expect_equal(keys[["B"]], keys[["E"]])
})

test_that("the t CRPS matches a numerical integral", {
  y <- 3; loc <- -2; sc <- 9; nu <- 7
  F <- function(x) stats::pt((x - loc) / sc, nu)
  num <- integrate(\(x) F(x)^2, -Inf, y)$value + integrate(\(x) (1 - F(x))^2, y, Inf)$value
  expect_equal(crps_t(y, loc, sc, nu), num, tolerance = 1e-6)
})

test_that("least squares recovers ratings on a connected schedule", {
  teams <- tibble::tibble(tier = c(1L, 1L, 1L))
  played <- tibble::tibble(home = c(1L, 2L, 1L, 3L), away = c(2L, 3L, 3L, 1L), home_adv = c(1, 1, 0, 0),
                           margin = c(12, 7, 13, -13))
  fit <- massey_fit(played, teams, lambda = 1e-6)
  expect_equal(fit$r[1] - fit$r[3], 13, tolerance = 0.1)
})
