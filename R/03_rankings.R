# ==============================================================================
# R/03_rankings.R
# Turn posterior draws into the published products for one snapshot:
#   Power  : rating (points vs. average FBS team, neutral field) + rank distribution
#   Resume : Wins Above Benchmark (WAB) vs. the #12 FBS team + rank distribution
# plus expected wins / luck, schedule strength, Colley, and poll ranks.
# ==============================================================================

# ---- Margin families ------------------------------------------------------------------
# The predictive law of the regulation margin given its expected value mu, by family (see
# stan/power.stan). `par` holds one row per posterior draw with sigma_marg, nu, p_wide,
# sigma_marg_w and gamma_m (the marginal scales, efficiency integrated out); mu is a vector
# (one draw per element) or an S x G matrix (draws in rows), and per-draw parameter vectors
# recycle down its columns.

#' Predictive scale of the margin at expected margin mu (the narrow component for the mixture)
margin_scale <- function(mu, par, family) {
  if (identical(family, "scale_t")) sqrt(par$sigma_marg^2 + (par$gamma_m * mu)^2) else par$sigma_marg + 0 * mu
}

#' P(margin <= x) per draw
margin_cdf <- function(x, mu, par, family = "t") {
  z <- x - mu
  out <- switch(family,
    normal = stats::pnorm(z / par$sigma_marg),
    t = stats::pt(z / par$sigma_marg, df = rep_len(par$nu, length(z))),
    mixture = (1 - par$p_wide) * stats::pnorm(z / par$sigma_marg) + par$p_wide * stats::pnorm(z / par$sigma_marg_w),
    scale_t = stats::pt(z / margin_scale(mu, par, family), df = rep_len(par$nu, length(z))),
    stop("Unknown margin family: ", family))
  dim(out) <- dim(mu)   # pt() drops dims when df is a vector
  out
}

#' P(margin > 0) under the model's predictive distribution, per draw
win_prob <- function(mu, par, family = "t") {
  out <- 1 - margin_cdf(0, mu, par, family)
  dim(out) <- dim(mu)
  out
}

#' One predictive draw of the margin per element of mu
margin_rng <- function(mu, par, family = "t") {
  n <- length(mu)
  per <- function(v) rep_len(v, n)
  e <- switch(family,
    normal = stats::rnorm(n) * per(par$sigma_marg),
    t = stats::rt(n, df = per(par$nu)) * per(par$sigma_marg),
    mixture = stats::rnorm(n) * ifelse(stats::runif(n) < per(par$p_wide), per(par$sigma_marg_w), per(par$sigma_marg)),
    scale_t = stats::rt(n, df = per(par$nu)) * sqrt(per(par$sigma_marg)^2 + (per(par$gamma_m) * mu)^2),
    stop("Unknown margin family: ", family))
  out <- mu + e
  dim(out) <- dim(mu)
  out
}

#' sd / scale of a Student-t per draw; 1.3 where the variance is undefined (nu <= 2, which only
#' prior-dominated fits such as week 0 reach) or nu is infinite. Computed only where defined, so
#' those draws don't raise "NaNs produced"
t_sd_factor <- function(nu) {
  out <- rep(1.3, length(nu))
  ok <- is.finite(nu) & nu > 2
  out[ok] <- sqrt(nu[ok] / (nu[ok] - 2))
  out
}

#' Standard deviation of the game noise at mu = 0 per draw, and its growth with |mu| (scale_t);
#' a normal approximation used by the dashboard's matchup tool
noise_sd <- function(par, family = "t") {
  tf <- t_sd_factor(par$nu)
  switch(family,
    normal = par$sigma_marg,
    t = par$sigma_marg * tf,
    mixture = sqrt((1 - par$p_wide) * par$sigma_marg^2 + par$p_wide * par$sigma_marg_w^2),
    scale_t = par$sigma_marg * tf)
}

#' Quantile levels stored for next-week predictions (scored by the backtest)
pred_probs <- seq(0.005, 0.995, by = 0.005)

#' Team-perspective rows for games in `played` (one row per team per game)
team_game_rows <- function(played) {
  dplyr::bind_rows(
    played |> dplyr::transmute(game_id, week_seq, team = home, opp = away, venue = home_adv,
                               win = as.integer(home_points > away_points),
                               pts = home_points, opp_pts = away_points),
    played |> dplyr::transmute(game_id, week_seq, team = away, opp = home, venue = -home_adv,
                               win = as.integer(away_points > home_points),
                               pts = away_points, opp_pts = home_points)
  )
}

rank_draws <- function(mat) {
  # rank within each draw (row), 1 = best
  t(apply(-mat, 1, rank, ties.method = "min"))
}

rank_summary <- function(rk, prefix) {
  tibble::tibble(
    # type = 1 quantiles are actual ranks (integers), not interpolations
    "{prefix}_rank_med" := as.integer(apply(rk, 2, stats::quantile, 0.5, type = 1, names = FALSE)),
    "{prefix}_rank_lo" := as.integer(apply(rk, 2, stats::quantile, 0.1, type = 1, names = FALSE)),
    "{prefix}_rank_hi" := as.integer(apply(rk, 2, stats::quantile, 0.9, type = 1, names = FALSE)),
    "{prefix}_p1" := colMeans(rk <= 1),
    "{prefix}_p4" := colMeans(rk <= 4),
    "{prefix}_p12" := colMeans(rk <= 12)
  )
}

#' Poisson-binomial upper tail: P(sum of Bernoulli(p) >= k)
prob_at_least <- function(p, k) {
  dist <- 1
  for (pi in p) dist <- c(dist * (1 - pi), 0) + c(0, dist * pi)
  if (k <= 0) return(1)
  sum(dist[(k + 1):length(dist)])
}

#' Power ratings: centred per draw on the FBS mean, ranked within tier
summarise_power <- function(draws, teams) {
  r <- draws$r
  fbs <- teams$tier == 1L
  r <- r - rowMeans(r[, fbs, drop = FALSE])
  out <- tibble::tibble(
    idx = teams$idx,
    power = colMeans(r),
    power_sd = apply(r, 2, stats::sd),
    power_lo = apply(r, 2, stats::quantile, 0.1, names = FALSE),
    power_hi = apply(r, 2, stats::quantile, 0.9, names = FALSE)
  )
  rk <- matrix(NA_real_, nrow(r), ncol(r))
  for (tr in unique(teams$tier)) {
    cols <- which(teams$tier == tr)
    rk[, cols] <- rank_draws(r[, cols, drop = FALSE])
  }
  out <- dplyr::bind_cols(out, rank_summary(rk, "power"))
  out$power_rank <- NA_integer_
  for (tr in unique(teams$tier)) {
    cols <- teams$tier == tr
    out$power_rank[cols] <- rank(-out$power[cols], ties.method = "first")
  }
  if (!is.null(draws$t)) out <- dplyr::bind_cols(out, summarise_off_def(r, draws$t, draws$par$base, teams))
  list(summary = out, r_centered = r)
}

#' Offence and defence in points against an average FBS team on a neutral field.
#' With off = (r + t) / 2 and def = (r - t) / 2, team i scores base + ((r_i - rbar) + (t_i + tbar)) / 2
#' against the average FBS team and allows base + ((rbar - r_i) + (t_i + tbar)) / 2, so
#' offence minus defence is the power rating. r must be FBS-centred (rbar = 0).
#' total_t (the raw total tendency) is carried to next season like the rating.
summarise_off_def <- function(r, t, base, teams) {
  fbs <- teams$tier == 1L
  tbar <- rowMeans(t[, fbs, drop = FALSE])
  common <- base + (t + tbar) / 2          # S x N: the shared part of both scores
  off_d <- common + r / 2
  def_d <- common - r / 2
  out <- tibble::tibble(
    off = colMeans(off_d), off_sd = apply(off_d, 2, stats::sd),
    def = colMeans(def_d), def_sd = apply(def_d, 2, stats::sd),
    total_t = colMeans(t), total_t_sd = apply(t, 2, stats::sd),
    off_rank = NA_integer_, def_rank = NA_integer_
  )
  for (tr in unique(teams$tier)) {
    cols <- teams$tier == tr
    out$off_rank[cols] <- rank(-out$off[cols], ties.method = "first")
    out$def_rank[cols] <- rank(out$def[cols], ties.method = "first")   # fewer points allowed ranks higher
  }
  out
}

#' Resume: Wins Above Benchmark per draw.
#' For each game, WAB adds (win - P(benchmark team wins that game at that venue)), where the
#' benchmark is the `benchmark_rank`-th best FBS team in that posterior draw.
summarise_resume <- function(draws, teams, played, benchmark_rank = 12L) {
  r <- draws$r
  par <- draws$par
  family <- draws$family %||% "t"
  S <- nrow(r)
  fbs_cols <- which(teams$tier == 1L)
  bench <- apply(r[, fbs_cols, drop = FALSE], 1, function(x) sort(x, decreasing = TRUE)[benchmark_rank])

  tg <- team_game_rows(played)
  if (nrow(tg) == 0) {
    # Preseason (no games): everyone is level at zero
    summary <- tibble::tibble(idx = teams$idx, wab = 0, wab_lo = 0, wab_hi = 0, games = 0L, wins = 0L, losses = 0L,
                              xwins = 0, sos = NA_real_, bench_xlosses = 0, sor_prob = NA_real_,
                              close_w = 0L, close_l = 0L, luck = 0, resume_rank = NA_integer_, sos_rank = NA_integer_)
    return(list(summary = summary, games = tibble::tibble(game_id = character(0), team = integer(0),
                                                          wab_game = numeric(0), p_bench = numeric(0), p_win = numeric(0))))
  }
  opp_r <- r[, tg$opp, drop = FALSE]
  venue <- matrix(tg$venue, S, nrow(tg), byrow = TRUE)
  p_bench <- win_prob(bench + par$hfa * venue - opp_r, par, family)   # S x rows
  p_self <- win_prob(r[, tg$team, drop = FALSE] + par$hfa * venue - opp_r, par, family)

  win_mat <- matrix(tg$win, S, nrow(tg), byrow = TRUE)
  by_team <- function(m) {
    # sum columns of an S x rows matrix by team -> S x N_teams
    out <- matrix(0, S, nrow(teams))
    agg <- t(rowsum(t(m), tg$team))
    out[, as.integer(colnames(agg))] <- agg
    out
  }
  wab_mat <- by_team(win_mat - p_bench)
  p_bench_mean <- colMeans(p_bench)

  per_team <- tg |>
    dplyr::mutate(p_bench = p_bench_mean, p_self = colMeans(p_self),
                  close = abs(pts - opp_pts) <= 8) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      games = dplyr::n(), wins = sum(win), losses = sum(1L - win),
      xwins = sum(p_self),
      sos = 1 - mean(p_bench),                      # benchmark's expected loss rate vs. this schedule
      bench_xlosses = sum(1 - p_bench),
      sor_prob = prob_at_least(p_bench, sum(win)),  # P(benchmark matches this record)
      close_w = sum(close & win == 1L), close_l = sum(close & win == 0L),
      .groups = "drop"
    )

  out <- tibble::tibble(idx = teams$idx, wab = colMeans(wab_mat),
                        wab_lo = apply(wab_mat, 2, stats::quantile, 0.1, names = FALSE),
                        wab_hi = apply(wab_mat, 2, stats::quantile, 0.9, names = FALSE)) |>
    dplyr::left_join(per_team, by = c("idx" = "team")) |>
    dplyr::mutate(dplyr::across(c(games, wins, losses, close_w, close_l), \(x) dplyr::coalesce(x, 0L)),
                  xwins = dplyr::coalesce(xwins, 0), luck = wins - xwins)

  rk <- matrix(NA_real_, S, nrow(teams))
  out$resume_rank <- NA_integer_
  out$sos_rank <- NA_integer_
  for (tr in unique(teams$tier)) {
    cols <- which(teams$tier == tr)
    rk[, cols] <- rank_draws(wab_mat[, cols, drop = FALSE])
    out$resume_rank[cols] <- rank(-out$wab[cols], ties.method = "first")
    out$sos_rank[cols] <- rank(-dplyr::coalesce(out$sos[cols], -Inf), ties.method = "first")
  }
  out <- dplyr::bind_cols(out, rank_summary(rk, "resume"))

  # Per-game resume contribution (win - P(benchmark wins)) for the team pages
  game_wab <- tg |> dplyr::transmute(game_id, team, wab_game = win - p_bench_mean,
                                     p_bench = p_bench_mean, p_win = colMeans(p_self))
  list(summary = out, games = game_wab)
}

#' Spread, win probability, total and projected score for unplayed games (home perspective).
#' Ratings are those at the snapshot week `now`; a game h weeks later adds h weeks of random-walk
#' drift for each team. Games in week now + 1 also get predictive quantiles of the margin
#' (pred_q, at pred_probs), which the backtest scores.
predict_games <- function(draws, games, now) {
  empty <- tibble::tibble(game_id = character(0), pred_margin = numeric(0), pred_margin_sd = numeric(0),
                          pred_home_wp = numeric(0), pred_q = list())
  if (nrow(games) == 0) return(empty)
  r <- draws$r
  par <- draws$par
  family <- draws$family %||% "t"
  S <- nrow(r)
  G <- nrow(games)
  gap <- matrix(pmax(games$week_seq - max(now, 1L), 0), S, G, byrow = TRUE)
  drift <- function(tau) sqrt(2 * gap) * tau * matrix(stats::rnorm(S * G), S, G)
  mu <- r[, games$home, drop = FALSE] - r[, games$away, drop = FALSE] +
    par$hfa * matrix(games$home_adv, S, G, byrow = TRUE) + drift(par$tau_r)
  out <- tibble::tibble(
    game_id = games$game_id,
    pred_margin = colMeans(mu),
    pred_margin_sd = apply(mu, 2, stats::sd),
    pred_home_wp = colMeans(win_prob(mu, par, family))
  )
  if (!is.null(draws$t)) {
    tt <- draws$t[, games$home, drop = FALSE] + draws$t[, games$away, drop = FALSE] + drift(par$tau_t)
    out$pred_total <- colMeans(2 * par$base + tt)
    out$pred_total_sd <- apply(2 * par$base + tt, 2, stats::sd)
    out$pred_home_pts <- colMeans(par$base + (mu + tt) / 2)
    out$pred_away_pts <- colMeans(par$base + (tt - mu) / 2)
  }
  nxt <- which(games$week_seq == now + 1L)
  out$pred_q <- vector("list", G)
  if (length(nxt)) {
    reps <- 4L   # predictive draws per posterior draw
    mu_n <- mu[rep(seq_len(S), reps), nxt, drop = FALSE]
    par_n <- par[rep(seq_len(S), reps), , drop = FALSE]
    y <- margin_rng(mu_n, par_n, family)
    q <- apply(y, 2, stats::quantile, probs = pred_probs, names = FALSE, type = 7)
    out$pred_q[nxt] <- lapply(seq_along(nxt), \(j) q[, j])
  }
  out
}

#' CRPS of a predictive distribution given by its quantiles at pred_probs (twice the mean
#' quantile score over the levels, which approximates the integral over all levels)
crps_quantiles <- function(y, q, probs = pred_probs) {
  vapply(seq_along(y), function(i) {
    if (is.null(q[[i]]) || is.na(y[i])) return(NA_real_)
    u <- y[i] - q[[i]]
    2 * mean(ifelse(u >= 0, probs * u, (probs - 1) * u))
  }, numeric(1))
}

#' Predictive CDF at x from stored quantiles (linear interpolation, clamped)
cdf_from_quantiles <- function(x, q, probs = pred_probs) {
  vapply(seq_along(x), function(i) {
    if (is.null(q[[i]]) || is.na(x[i])) return(NA_real_)
    qi <- q[[i]]
    if (x[i] <= qi[1]) return(probs[1] / 2)
    if (x[i] >= qi[length(qi)]) return(1 - (1 - probs[length(probs)]) / 2)
    keep <- !duplicated(qi)
    stats::approx(qi[keep], probs[keep], x[i], ties = "ordered")$y
  }, numeric(1))
}

#' Predictive quantile at level p from stored quantiles
quantile_at <- function(q, p, probs = pred_probs) {
  vapply(q, function(qi) if (is.null(qi)) NA_real_ else stats::approx(probs, qi, p, rule = 2)$y, numeric(1))
}

#' Colley matrix (record only, schedule-adjusted, no margin); FBS ranks
compute_colley <- function(teams, played) {
  n <- nrow(teams)
  C <- diag(2, n)
  b <- rep(1, n)
  for (k in seq_len(nrow(played))) {
    h <- played$home[k]; a <- played$away[k]
    hw <- if (played$home_points[k] > played$away_points[k]) 1 else -1
    C[h, h] <- C[h, h] + 1; C[a, a] <- C[a, a] + 1
    C[h, a] <- C[h, a] - 1; C[a, h] <- C[a, h] - 1
    b[h] <- b[h] + hw / 2; b[a] <- b[a] - hw / 2
  }
  rating <- solve(C, b)
  out <- tibble::tibble(idx = teams$idx, colley = rating, colley_rank = NA_integer_)
  for (tr in unique(teams$tier)) {
    cols <- teams$tier == tr
    out$colley_rank[cols] <- rank(-rating[cols], ties.method = "first")
  }
  out
}

#' The poll reflecting games through `week`: the regular-season poll released after week w is
#' CFBD week w + 1; the postseason snapshot uses the final (postseason) poll.
poll_for_week <- function(polls, week, last_regular_week, poll_name) {
  if (is.null(polls) || nrow(polls) == 0) return(NULL)
  p <- polls[polls$poll == poll_name, ]
  if (week > last_regular_week) {
    p <- p[p$season_type == "postseason", ]
  } else {
    p <- p[p$season_type == "regular" & p$week == week + 1L, ]
  }
  if (nrow(p) == 0) return(NULL)
  p[p$week == max(p$week), c("team_id", "rank")]
}

#' Full snapshot summary (one row per team) plus per-game rows. Poll ranks are attached
#' later (attach_polls) so a new poll never invalidates a cached fit.
summarise_snapshot <- function(input, draws, health, cfg = model_config()) {
  teams <- input$teams
  draws$family <- draws$family %||% cfg$margin_family %||% "t"
  pw <- summarise_power(draws, teams)
  thin <- unique(round(seq(1, nrow(draws$r), length.out = min(cfg$summary_draws, nrow(draws$r)))))
  thinned <- list(r = pw$r_centered[thin, , drop = FALSE], par = draws$par[thin, ], family = draws$family,
                  t = if (!is.null(draws$t)) draws$t[thin, , drop = FALSE])
  rs <- summarise_resume(thinned, teams, input$played, cfg$benchmark_rank)
  colley <- compute_colley(teams, input$played)

  ratings <- teams |>
    dplyr::select(idx, team_id, team, conference, tier) |>
    dplyr::left_join(pw$summary, by = "idx") |>
    dplyr::left_join(rs$summary, by = "idx") |>
    dplyr::left_join(colley, by = "idx") |>
    dplyr::mutate(season = input$season, week = input$week, .before = 1)

  par <- draws$par
  m <- function(x) if (is.null(x)) NA_real_ else mean(x)
  t_family <- draws$family %in% c("t", "scale_t")
  params <- tibble::tibble(
    season = input$season, week = input$week, week_label = input$week_label,
    n_games = nrow(input$played), has_prior = input$has_prior, family = draws$family,
    hfa = mean(par$hfa), sigma = mean(par$sigma_marg), nu = if (t_family) mean(par$nu) else NA_real_,
    sigma_w = mean(par$sigma_marg_w), p_wide = mean(par$p_wide), gamma = mean(par$gamma_m),
    noise_sd = mean(noise_sd(par, draws$family)),
    noise_slope = if (draws$family == "scale_t") mean(par$gamma_m * t_sd_factor(par$nu)) else 0,
    tau = mean(par$tau_r),
    lambda = mean(par$lambda), kappa = mean(par$kappa), b_prev = mean(par$b_prev),
    fcs_offset = mean(par$`mu_tier[2]`), nond1_offset = mean(par$`mu_tier[3]`),
    sigma_m = mean(par$sigma_m), sigma_eff = mean(par$sigma_eff), sigma_conf = mean(par$sigma_conf),
    sigma_team_fbs = mean(par$`sigma_team[1]`), sigma_team_fcs = mean(par$`sigma_team[2]`),
    sigma_team_nond1 = mean(par$`sigma_team[3]`),
    base = m(par$base), tau_t = m(par$tau_t), sigma_tot = m(par$sigma_tot_marg), b_prev_t = m(par$b_prev_t),
    b_prev_low = m(par$b_prev_low), b_ret = m(par$b_ret), c_ret = m(par$c_ret), b_tal = m(par$b_tal),
    b_prev2 = m(par$b_prev2), delta_coach = m(par$delta_coach)
  ) |> dplyr::bind_cols(health)

  game_ids <- function(df) df |> dplyr::mutate(team_id = teams$team_id[team]) |> dplyr::select(-team)
  preds <- predict_games(thinned, input$future, input$week)

  list(
    ratings = ratings,
    params = params,
    game_resume = game_ids(rs$games) |> dplyr::mutate(season = input$season, week = input$week),
    predictions = preds |> dplyr::mutate(season = input$season, week = input$week, .before = 1)
  )
}
