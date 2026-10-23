test_that("Poisson-binomial tail matches brute force", {
  p <- c(0.2, 0.7, 0.5)
  grid <- expand.grid(0:1, 0:1, 0:1)
  probs <- apply(grid, 1, function(x) prod(ifelse(x == 1, p, 1 - p)))
  expect_equal(prob_at_least(p, 2), sum(probs[rowSums(grid) >= 2]))
  expect_equal(prob_at_least(p, 0), 1)
})

toy <- function() {
  teams <- tibble::tibble(idx = 1:4, team_id = 101:104, team = c("A", "B", "C", "D"),
                          conference = "X", tier = 1L)
  played <- tibble::tibble(game_id = c("g1", "g2"), week_seq = 1L,
                           home = c(1L, 3L), away = c(2L, 4L), home_adv = c(1, 0),
                           home_points = c(21L, 10L), away_points = c(14L, 17L))
  list(teams = teams, played = played)
}

test_that("WAB = win minus benchmark win probability, summed per team", {
  t <- toy()
  S <- 50
  r <- matrix(rep(c(10, 5, 0, -5), each = S), S)
  par <- data.frame(hfa = rep(2, S), sigma_marg = rep(10, S), nu = rep(1e6, S))
  res <- summarise_resume(list(r = r, par = par), t$teams, t$played, benchmark_rank = 2L)
  # benchmark = 2nd best = 5. Team A beat B (rating 5) at home: 1 - P(5 + 2 - 5 > 0)
  pA <- pnorm((5 + 2 - 5) / 10)
  expect_equal(res$summary$wab[1], 1 - pA, tolerance = 1e-3)
  # Team D won at a neutral site vs C (rating 0): 1 - P(5 - 0 > 0)
  expect_equal(res$summary$wab[4], 1 - pnorm(0.5), tolerance = 1e-3)
  # losing team B: 0 - P(benchmark beats A on the road)
  expect_equal(res$summary$wab[2], -pnorm((5 - 2 - 10) / 10), tolerance = 1e-3)
  expect_equal(res$summary$resume_rank[res$summary$idx == 1], 1L)  # 0.42 WAB vs 0.31
})

test_that("Colley reduces to 1/2 for everyone with no games and rewards wins", {
  t <- toy()
  empty <- compute_colley(t$teams, t$played[0, ])
  expect_equal(empty$colley, rep(0.5, 4))
  c1 <- compute_colley(t$teams, t$played)
  expect_gt(c1$colley[1], c1$colley[2])
  expect_equal(sum(c1$colley), 2)
})

test_that("poll for week w is the regular-season poll numbered w + 1; postseason uses final poll", {
  polls <- tibble::tibble(poll = "AP Top 25", season_type = c("regular", "regular", "postseason"),
                          week = c(4L, 5L, 1L), team_id = c(1L, 2L, 3L), rank = 1L)
  expect_equal(poll_for_week(polls, 3L, 14L, "AP Top 25")$team_id, 1L)
  expect_equal(poll_for_week(polls, 4L, 14L, "AP Top 25")$team_id, 2L)
  expect_equal(poll_for_week(polls, 15L, 14L, "AP Top 25")$team_id, 3L)
  expect_null(poll_for_week(polls, 9L, 14L, "AP Top 25"))
})

test_that("model inputs don't depend on later results (cache stability)", {
  games <- tibble::tibble(
    game_id = c("a", "b"), season = 2025L, week = c(1L, 2L), season_type = "regular",
    week_seq = c(1L, 2L), week_label = c("Week 1", "Week 2"),
    start_date = "2025-09-01T00:00:00.000Z", completed = c(TRUE, FALSE), neutral_site = FALSE,
    home_id = c(1L, 2L), home_team = c("A", "B"), home_conference = "X", home_class = "fbs",
    away_id = c(2L, 1L), away_team = c("B", "A"), away_conference = "X", away_class = "fbs",
    home_points = c(20L, NA), away_points = c(10L, NA), reg_margin = c(10, NA), ot = FALSE,
    home_ppa = 0.2, home_plays = 60L, away_ppa = 0.1, away_plays = 60L, spread = -3
  )
  pre <- tibble::tibble(team_id = 1:2, ret_ppa = c(0.7, 0.5), talent = c(800, 600), coach_change = 0L)
  a <- prepare_week_input(list(season = 2025L, games = games, preseason = pre), 1L)
  games$completed[2] <- TRUE; games$home_points[2] <- 7L; games$away_points[2] <- 3L; games$reg_margin[2] <- 4
  b <- prepare_week_input(list(season = 2025L, games = games, preseason = pre), 1L)
  expect_identical(a, b)
  expect_equal(a$stan_data$N_games, 1L)
  expect_equal(a$stan_data$margin, 10)
  expect_equal(a$stan_data$eff, 0.2 * 60 - 0.1 * 60)
})

test_that("preseason snapshot (no games) summarises without error", {
  t <- toy()
  S <- 20
  r <- matrix(rnorm(S * 4), S)
  par <- data.frame(hfa = rep(2, S), sigma_marg = rep(10, S), nu = rep(8, S))
  res <- summarise_resume(list(r = r, par = par), t$teams, t$played[0, ])
  expect_equal(res$summary$games, rep(0L, 4))
  expect_equal(nrow(res$games), 0)
})
