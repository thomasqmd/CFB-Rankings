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

test_that("SP+ resume: WAB against the b-th best SP+ team, other opponents valued through power", {
  teams <- tibble::tibble(team_id = 101:105, tier = c(1L, 1L, 1L, 1L, 2L), power = c(10, 5, 0, -5, -20),
                          conference = c("X", "X", "X", "Y", NA))
  sp <- tibble::tibble(team_id = 101:104, sp = c(20, 10, 0, -10))   # SP+ = 2 x power; the FCS team has none
  tg <- tibble::tibble(team_id = c(101L, 102L, 103L, 105L), opp_id = c(102L, 101L, 105L, 103L),
                       venue = c(1, -1, 0, 0), win = c(1L, 0L, 1L, 0L))
  par <- data.frame(hfa = 2, sigma_marg = 10, nu = 1e6, gamma_m = 0, p_wide = 0, sigma_marg_w = 10)
  res <- sp_resume(teams, tg, sp, par, family = "t", benchmark_rank = 2L)
  expect_equal(res$team_id, 101:103)                                 # FBS teams with a game
  # benchmark = 2nd best SP+ = 10. A beat B (SP+ 10) at home; B lost at A (SP+ 20)
  expect_equal(res$sp_wab[1], 1 - pnorm((10 + 2 - 10) / 10), tolerance = 1e-4)
  expect_equal(res$sp_wab[2], -pnorm((10 - 2 - 20) / 10), tolerance = 1e-4)
  # C beat the FCS team on a neutral field: its SP+ is read off the fit, 2 x -20 = -40
  expect_equal(res$sp_wab[3], 1 - pnorm((10 + 40) / 10), tolerance = 1e-4)
  expect_equal(res$sp_resume_rank, c(1L, 3L, 2L))
  expect_equal(nrow(sp_resume(teams, tg[0, ], sp, par, benchmark_rank = 2L)), 0)
  expect_equal(nrow(sp_resume(teams, tg, sp[1:2, ], par, benchmark_rank = 12L)), 0)
  # With automatic bids. One bid: it goes to the better champion, A (X), so the benchmark is the 2nd
  # best of B, C, D = C (0). Two bids, A and D (champion of Y): the benchmark is the best of B, C = B
  two <- sp_resume(teams, tg, sp, par, family = "t", benchmark_rank = 3L, n_auto = 2L, champions = 4L)
  expect_equal(two$sp_wab, res$sp_wab, tolerance = 1e-8)
  auto <- sp_resume(teams, tg, sp, par, family = "t", benchmark_rank = 3L, n_auto = 1L, champions = 4L)
  expect_equal(auto$sp_wab[1], 1 - pnorm((0 + 2 - 10) / 10), tolerance = 1e-4)
})

test_that("benchmark: the last at-large team once the best-rated champions take the automatic bids", {
  teams <- tibble::tibble(idx = 1:8, team_id = 201:208, tier = c(rep(1L, 7), 2L),
                          conference = c("A", "A", "A", "B", "B", "FBS Independents", "C", "A"))
  r <- matrix(c(30, 25, 5, 20, 10, 22, 0, 99), nrow = 2, ncol = 8, byrow = TRUE)
  # No automatic bids: the 3rd best FBS team (the FCS team's 99 never counts)
  expect_equal(benchmark_strength(r, teams, field = 3L), c(22, 22))
  # Two bids go to the best-rated champions, A's 30 and B's 20; C's champion (0) misses out and
  # the independent (22) cannot be one. The 2nd best of the rest (25, 22, 10, 5, 0) is 22
  expect_equal(benchmark_strength(r, teams, field = 4L, n_auto = 2L), c(22, 22))
  # A title game upset: team 3 (5) is A's champion, so 30 and 25 are both at-large candidates
  expect_equal(benchmark_strength(r, teams, field = 4L, n_auto = 2L, champions = 3L), c(25, 25))
  # More bids than conferences: every champion gets one
  expect_equal(benchmark_strength(r, teams, field = 5L, n_auto = 9L), c(22, 22))
  # Champions are picked draw by draw
  r2 <- rbind(r[1, ], c(25, 30, 5, 20, 10, 22, 0, 99))
  expect_equal(benchmark_strength(r2, teams, field = 3L, n_auto = 2L), c(25, 25))
})

test_that("decided champions: title games played by the snapshot week, as team positions", {
  games <- tibble::tibble(season_type = "regular", completed = c(TRUE, TRUE, FALSE, TRUE),
                          notes = c("SEC Championship", NA, "ACC Championship", "Some Championship"),
                          home_class = "fbs", away_class = "fbs", home_id = c(1L, 3L, 5L, 7L), away_id = c(2L, 4L, 6L, 8L),
                          home_conference = c("SEC", "SEC", "ACC", "MAC"), away_conference = c("SEC", "SEC", "ACC", "Sun Belt"),
                          neutral_site = TRUE, home_points = c(10L, 20L, NA, 30L), away_points = c(24L, 3L, NA, 7L),
                          week_seq = c(16L, 15L, 16L, 16L))
  ch <- conference_champions(games)   # the unplayed ACC game and the cross-conference game do not count
  expect_equal(ch$team_id, 2L)
  # Without labels: a round robin of four, then the top two meet again in the last week
  rr <- t(combn(11:14, 2))
  sched <- tibble::tibble(season_type = "regular", completed = TRUE, notes = NA_character_, home_class = "fbs", away_class = "fbs",
                          home_id = c(rr[, 1], 11L), away_id = c(rr[, 2], 12L), home_conference = "X", away_conference = "X",
                          neutral_site = c(rep(FALSE, 6), TRUE), home_points = 21L, away_points = c(rep(7L, 6), 28L),
                          week_seq = c(1L, 2L, 3L, 3L, 2L, 1L, 4L))
  expect_equal(conference_champions(sched), tibble::tibble(team_id = 12L, week_seq = 4L))
  expect_equal(nrow(conference_champions(sched[1:6, ])), 0)    # no title game: no decided champion
  open <- sched; open$completed[5] <- FALSE                    # a conference game still to play: not decided
  expect_equal(nrow(conference_champions(open)), 0)
  teams <- tibble::tibble(idx = 1:3, team_id = c(9L, 2L, 1L))
  expect_equal(decided_champions(ch, teams, 16L), 2L)
  expect_equal(decided_champions(ch, teams, 15L), integer(0))
  expect_equal(decided_champions(NULL, teams, 16L), integer(0))
})
