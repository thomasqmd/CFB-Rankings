# ==============================================================================
# R/03_rankings.R
# Turn posterior draws into the published products for one snapshot:
#   Power  : rating (points vs. average FBS team, neutral field) + rank distribution
#   Resume : Wins Above Benchmark (WAB) vs. the #12 FBS team + rank distribution
# plus expected wins / luck, schedule strength, Colley, and poll ranks.
# ==============================================================================

#' P(margin > 0) under the model's predictive distribution; mu and the parameter vectors
#' are per posterior draw (recycled against columns when mu is a matrix)
win_prob <- function(mu, sigma, nu) {
  out <- stats::pt(mu / sigma, df = nu)
  dim(out) <- dim(mu)   # pt() drops dims when df is a vector
  out
}

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
  list(summary = out, r_centered = r)
}

#' Resume: Wins Above Benchmark per draw.
#' For each game, WAB adds (win - P(benchmark team wins that game at that venue)), where the
#' benchmark is the `benchmark_rank`-th best FBS team in that posterior draw.
summarise_resume <- function(draws, teams, played, benchmark_rank = 12L) {
  r <- draws$r
  par <- draws$par
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
  p_bench <- win_prob(bench + par$hfa * venue - opp_r, par$sigma_marg, par$nu)   # S x rows
  p_self <- win_prob(r[, tg$team, drop = FALSE] + par$hfa * venue - opp_r, par$sigma_marg, par$nu)

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

#' Spread and win probability for unplayed games (home perspective)
predict_games <- function(draws, games) {
  if (nrow(games) == 0) return(tibble::tibble(game_id = character(0), pred_margin = numeric(0), pred_home_wp = numeric(0)))
  r <- draws$r
  par <- draws$par
  S <- nrow(r)
  mu <- r[, games$home, drop = FALSE] - r[, games$away, drop = FALSE] +
    par$hfa * matrix(games$home_adv, S, nrow(games), byrow = TRUE)
  tibble::tibble(
    game_id = games$game_id,
    pred_margin = colMeans(mu),
    pred_margin_sd = apply(mu, 2, stats::sd),
    pred_home_wp = colMeans(win_prob(mu, par$sigma_marg, par$nu))
  )
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
  pw <- summarise_power(draws, teams)
  thin <- unique(round(seq(1, nrow(draws$r), length.out = min(cfg$summary_draws, nrow(draws$r)))))
  thinned <- list(r = pw$r_centered[thin, , drop = FALSE], par = draws$par[thin, ])
  rs <- summarise_resume(thinned, teams, input$played, cfg$benchmark_rank)
  colley <- compute_colley(teams, input$played)

  ratings <- teams |>
    dplyr::select(idx, team_id, team, conference, tier) |>
    dplyr::left_join(pw$summary, by = "idx") |>
    dplyr::left_join(rs$summary, by = "idx") |>
    dplyr::left_join(colley, by = "idx") |>
    dplyr::mutate(season = input$season, week = input$week, .before = 1)

  par <- draws$par
  params <- tibble::tibble(
    season = input$season, week = input$week, week_label = input$week_label,
    n_games = nrow(input$played), has_prior = input$has_prior,
    hfa = mean(par$hfa), sigma = mean(par$sigma_marg), nu = mean(par$nu),
    lambda = mean(par$lambda), kappa = mean(par$kappa), b_prev = mean(par$b_prev),
    fcs_offset = mean(par$`mu_tier[2]`), nond1_offset = mean(par$`mu_tier[3]`)
  ) |> dplyr::bind_cols(health)

  game_ids <- function(df) df |> dplyr::mutate(team_id = teams$team_id[team]) |> dplyr::select(-team)
  preds <- predict_games(thinned, input$future)

  list(
    ratings = ratings,
    params = params,
    game_resume = game_ids(rs$games) |> dplyr::mutate(season = input$season, week = input$week),
    predictions = preds |> dplyr::mutate(season = input$season, week = input$week, .before = 1)
  )
}
