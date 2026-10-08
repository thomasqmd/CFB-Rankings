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

test_that("random-walk states follow each team's games in week order", {
  played <- tibble::tibble(home = c(1L, 2L, 1L), away = c(2L, 3L, 3L), week_seq = c(1L, 2L, 4L))
  ws <- walk_states(played, n_teams = 4L, week = 5L)
  st <- ws$states
  # team 1 plays weeks 1 and 4: first state steps 0 weeks from the week-1 level, the next 3
  t1 <- st[st$team == 1L, ]
  expect_equal(t1$gap, c(0L, 3L))
  expect_equal(t1$prev, c(0L, t1$state[1]))
  # team 3 first plays in week 2: one week from the week-1 level
  expect_equal(st$gap[st$team == 3L], c(1L, 2L))
  # games point at their teams' states for that week
  expect_equal(st$team[ws$home_state], played$home)
  expect_equal(st$week_seq[ws$away_state], played$week_seq)
  # now (week 5): team 1 last played week 4; team 4 never played (5 - 1 weeks from week 1)
  expect_equal(ws$now_gap, c(1, 3, 1, 4))
  expect_equal(ws$now_state[4], 0L)
})

test_that("margin families: win probabilities, CDFs and draws agree", {
  par <- data.frame(sigma_marg = 12, sigma_marg_w = 25, p_wide = 0.2, gamma_m = 0.15, nu = 8)
  for (fam in c("normal", "t", "mixture", "scale_t")) {
    expect_equal(win_prob(0, par, fam), 0.5)
    expect_equal(win_prob(5, par, fam) + win_prob(-5, par, fam), 1, tolerance = 1e-12)
    set.seed(1)
    y <- margin_rng(rep(5, 2e5), par, fam)
    expect_equal(mean(y <= 10), margin_cdf(10, 5, par, fam), tolerance = 5e-3)
  }
  # the mixture is the weighted sum of its two normals
  expect_equal(margin_cdf(3, 0, par, "mixture"), 0.8 * pnorm(3 / 12) + 0.2 * pnorm(3 / 25))
  # matrices keep their shape with per-draw parameters
  pm <- data.frame(sigma_marg = c(10, 20), sigma_marg_w = 30, p_wide = 0.1, gamma_m = 0.1, nu = c(5, 50))
  mu <- matrix(c(1, 2, 3, 4, 5, 6), 2)
  out <- win_prob(mu, pm, "t")
  expect_equal(dim(out), c(2L, 3L))
  expect_equal(out[2, 3], pt(6 / 20, 50))
})

test_that("quantile CRPS approximates the closed-form t CRPS", {
  q <- list(5 + 9 * qt(pred_probs, 7))
  expect_equal(crps_quantiles(12, q), crps_t(12, 5, 9, 7), tolerance = 0.02)
  expect_equal(cdf_from_quantiles(5, q), 0.5, tolerance = 1e-6)
  expect_equal(quantile_at(q, 0.9), 5 + 9 * qt(0.9, 7), tolerance = 1e-6)
})

test_that("total tendency carries over like the rating", {
  teams <- tibble::tibble(team_id = 1:3, tier = c(1L, 1L, 2L))
  prior <- tibble::tibble(team_id = 1:3, rating = c(5, -5, -30), rating_sd = 1, total_t = c(4, -2, 1), total_t_sd = 2)
  d <- prev_deviation(teams, prior, "total_t")
  expect_equal(d$dev, c(3, -3, 0))
  expect_equal(d$sd, c(2, 2, 2))
  expect_equal(prev_deviation(teams, prior[, 1:3], "total_t")$dev, rep(0, 3))   # older priors lack it
})
