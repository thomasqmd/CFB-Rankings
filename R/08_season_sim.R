# ==============================================================================
# R/08_season_sim.R
# Rest-of-season simulation for the live season, from the latest snapshot's posterior:
#   conference races  : standings, generic tiebreakers, title game
#   postseason        : bowl eligibility, CFP field (five highest-ranked champions + seven
#                       at-large; the final resume ranking stands in for the committee)
#   paths             : odds after a win vs. a loss in each remaining game, rooting interests
# ==============================================================================

#' Title-game format per FBS conference: `divisions` = the two division winners meet (otherwise
#' the top two by conference win %); `host` = "neutral" or "top_seed" (the higher seed hosts)
ccg_formats <- function() {
  tibble::tribble(
    ~conference,         ~divisions, ~host,
    "ACC",               FALSE,      "neutral",
    "Big 12",            FALSE,      "neutral",
    "Big Ten",           FALSE,      "neutral",
    "SEC",               FALSE,      "neutral",
    "Mid-American",      FALSE,      "neutral",
    "American Athletic", FALSE,      "top_seed",
    "Conference USA",    FALSE,      "top_seed",
    "Mountain West",     FALSE,      "top_seed",
    "Pac-12",            FALSE,      "top_seed",
    "Sun Belt",          TRUE,       "top_seed"
  )
}

#' Each team's division (CFBD /teams, read from the cache sync_teams() keeps; NA when none)
team_divisions <- function(season, raw_dir = "data/raw") {
  raw <- cfbd_cached("teams", season, "teams", list(), raw_dir = raw_dir, refresh = FALSE)$data
  tibble::tibble(team_id = vapply(raw, \(t) int(t$id), integer(1)),
                 division = vapply(raw, \(t) chr(t$division), character(1)))
}

# ---- Tiebreakers ------------------------------------------------------------------------
# Teams are local indices; `home`, `away` and `win` describe one simulation's conference games
# (win = the home team won).

#' Win % of each team in `tied` over the games `gi`, NA for a team without any
record_pct <- function(tied, gi, home, away, win) {
  vapply(tied, function(t) {
    h <- gi[home[gi] == t]
    a <- gi[away[gi] == t]
    n <- length(h) + length(a)
    if (n == 0) NA_real_ else (sum(win[h]) + sum(!win[a])) / n
  }, numeric(1))
}

#' The team that comes first among `tied` under the generic chain: head-to-head (the round-robin
#' record if every pair met, otherwise only a team that beat all the others stands out), then the
#' record against common conference opponents, then `rating` (a stand-in for each conference's
#' computer-metric step). Once a step separates a smaller group, the chain restarts for it.
tiebreak_first <- function(tied, home, away, win, rating) {
  if (length(tied) == 1L) return(tied)
  among <- which(home %in% tied & away %in% tied)
  met <- function(t, o) among[(home[among] == t & away[among] == o) | (home[among] == o & away[among] == t)]
  h2h <- function() {
    pairs <- utils::combn(tied, 2)
    if (all(apply(pairs, 2, \(p) length(met(p[1], p[2])) > 0))) return(record_pct(tied, among, home, away, win))
    vapply(tied, function(t) {
      beat <- vapply(setdiff(tied, t), function(o) {
        g <- met(t, o)
        length(g) > 0 && all(ifelse(home[g] == t, win[g], !win[g]))
      }, logical(1))
      as.numeric(all(beat))
    }, numeric(1))
  }
  common <- function() {
    opps <- lapply(tied, function(t) setdiff(c(away[home == t], home[away == t]), tied))
    shared <- Reduce(intersect, opps)
    if (length(shared) == 0) return(rep(0, length(tied)))
    record_pct(tied, which((home %in% tied & away %in% shared) | (away %in% tied & home %in% shared)),
               home, away, win)
  }
  for (step in list(h2h, common)) {
    v <- step()
    v[is.na(v)] <- -Inf
    keep <- tied[v == max(v)]
    if (length(keep) == 1L) return(keep)
    if (length(keep) < length(tied)) return(tiebreak_first(keep, home, away, win, rating))
  }
  tied[which.max(rating[tied])]
}

#' Title-game seeds for one simulation: the top two by conference win %, or the two division
#' winners (seed 1 = the better record, ties broken by the same chain)
conference_seeds <- function(pct, home, away, win, rating, division = NULL) {
  best <- function(cand) tiebreak_first(cand[pct[cand] == max(pct[cand])], home, away, win, rating)
  if (is.null(division)) {
    s1 <- best(seq_along(pct))
    return(c(s1, best(setdiff(seq_along(pct), s1))))
  }
  winners <- unname(vapply(split(seq_along(pct), division), best, integer(1)))
  s1 <- best(winners)
  c(s1, setdiff(winners, s1)[1])
}

#' Bowl eligibility: 6 wins with at most one win over a non-FBS team counted, and no more losses
#' than wins (regular season, title game excluded)
bowl_eligible <- function(wins, wins_fbs, games) {
  counted <- wins_fbs + pmin(wins - wins_fbs, 1)
  counted >= 6 & wins >= games - wins
}

#' Seeds of the 12-team CFP field for one simulation: the five highest-ranked conference champions
#' plus the seven highest-ranked others, seeded straight by rank
#' @param rk ranks (1 = best) of the FBS teams; `champs` their positions in `rk`
cfp_field <- function(rk, champs, n_auto = 5L, n_field = 12L) {
  champs <- unique(champs[!is.na(champs)])
  auto <- champs[order(rk[champs])][seq_len(min(n_auto, length(champs)))]
  at_large <- setdiff(order(rk), auto)[seq_len(n_field - length(auto))]
  field <- c(auto, at_large)
  seed <- rep(NA_integer_, length(rk))
  seed[field[order(rk[field])]] <- seq_along(field)
  list(seed = seed, auto = auto)
}

# ---- Simulation -------------------------------------------------------------------------

#' Simulate the rest of the regular season and the title games
#' @param draws thinned posterior draws (r, par, family) at the snapshot week
#' @param input the snapshot's prepare_week_input()
#' @param games the season's games (read_season()$games): every result already in, including
#'   games after the snapshot week
#' @param divisions tibble(team_id, division) or NULL
#' @param reps simulated seasons per posterior draw
#' @return list of per-simulation matrices over FBS teams, and the games behind them
simulate_season <- function(draws, input, games, divisions = NULL, reps = 4L, benchmark_rank = 12L) {
  teams <- input$teams
  family <- draws$family %||% "t"
  S0 <- nrow(draws$r)
  pick <- rep(seq_len(S0), reps)
  r <- draws$r[pick, , drop = FALSE]
  par <- draws$par[pick, , drop = FALSE]
  S <- nrow(r)
  now <- max(input$week, 1L)
  fbs <- teams$tier == 1L
  fbs_idx <- which(fbs)
  nf <- length(fbs_idx)

  g <- games |>
    dplyr::filter(season_type == "regular") |>
    dplyr::mutate(home = match(home_id, teams$team_id), away = match(away_id, teams$team_id),
                  home_adv = ifelse(neutral_site, 0, 1),
                  conference_game = dplyr::coalesce(conference_game, FALSE),
                  ccg = conference_game & grepl("Championship", notes),
                  done = completed & !is.na(home_points) & !is.na(away_points)) |>
    dplyr::filter(!is.na(home), !is.na(away)) |>
    dplyr::filter(fbs[home] | fbs[away])
  ccg_week <- max(g$week_seq)
  reg <- g |> dplyr::filter(!ccg)
  fut <- which(!reg$done)

  # One random walk per team from the snapshot week: week w adds w - now weekly steps of sd tau_r
  steps <- max(c(reg$week_seq[fut], ccg_week) - now, 1L)
  inv <- sort(unique(c(reg$home[fut], reg$away[fut], fbs_idx)))
  col <- match(seq_len(nrow(teams)), inv)
  walk <- array(0, c(S, length(inv), steps))
  cur <- matrix(0, S, length(inv))
  for (k in seq_len(steps)) {
    cur <- cur + par$tau_r * matrix(stats::rnorm(S * length(inv)), S)
    walk[, , k] <- cur
  }
  rating_cols <- function(team, week) {     # S x length(team): fixed teams
    out <- r[, team, drop = FALSE]
    if (week > now) out <- out + matrix(walk[, col[team], week - now], S)
    out
  }
  rating_rows <- function(team, week) {     # length S: one team per simulation
    out <- r[cbind(seq_len(S), team)]
    if (week > now) out <- out + walk[cbind(seq_len(S), col[team], week - now)]
    out
  }

  # Home-team wins, S x regular-season games (results already in are the same in every row)
  W <- matrix(rep(reg$done & dplyr::coalesce(reg$home_points > reg$away_points, FALSE), each = S), S)
  for (w in unique(reg$week_seq[fut])) {
    j <- fut[reg$week_seq[fut] == w]
    mu <- rating_cols(reg$home[j], w) - rating_cols(reg$away[j], w) +
      par$hfa * matrix(reg$home_adv[j], S, length(j), byrow = TRUE)
    W[, j] <- margin_rng(mu, par, family) > 0
  }

  # Indicators: game x FBS team, as home and as away side
  side <- function(team, keep = rep(TRUE, length(team))) {
    m <- matrix(0, length(team), nf)
    i <- which(keep & fbs[team])
    m[cbind(i, match(team[i], fbs_idx))] <- 1
    m
  }
  H <- side(reg$home)
  A <- side(reg$away)
  wins <- W %*% H + (!W) %*% A
  wins_fbs <- W %*% (H * fbs[reg$away]) + (!W) %*% (A * fbs[reg$home])
  n_games <- matrix(colSums(H) + colSums(A), S, nf, byrow = TRUE)
  eligible <- bowl_eligible(wins, wins_fbs, n_games)

  # Resume after the regular season: each game adds win - P(benchmark wins it), with the
  # benchmark (the draw's #benchmark_rank FBS team) and opponents at their snapshot ratings
  r0 <- draws$r
  bench0 <- apply(r0[, fbs_idx, drop = FALSE], 1, \(x) sort(x, decreasing = TRUE)[benchmark_rank])
  adv0 <- matrix(reg$home_adv, S0, nrow(reg), byrow = TRUE)
  pb_home <- win_prob(bench0 + draws$par$hfa * adv0 - r0[, reg$away, drop = FALSE], draws$par, family)
  pb_away <- win_prob(bench0 - draws$par$hfa * adv0 - r0[, reg$home, drop = FALSE], draws$par, family)
  wab <- wins - (pb_home %*% H + pb_away %*% A)[pick, , drop = FALSE]
  bench <- bench0[pick]

  # Conference races
  fmt <- ccg_formats() |> dplyr::filter(conference %in% teams$conference[fbs])
  nc <- nrow(fmt)
  seed1 <- seed2 <- champ <- matrix(NA_integer_, S, nc, dimnames = list(NULL, fmt$conference))
  conf_wins <- matrix(NA_real_, S, nf)
  conf_games <- rep(NA_integer_, nf)
  for (ci in seq_len(nc)) {
    members <- which(fbs & teams$conference == fmt$conference[ci])
    if (length(members) < 2) next
    cg <- which(reg$conference_game & reg$home %in% members & reg$away %in% members)
    lh <- match(reg$home[cg], members)
    la <- match(reg$away[cg], members)
    Wc <- W[, cg, drop = FALSE]
    Hc <- matrix(0, length(cg), length(members)); Hc[cbind(seq_along(cg), lh)] <- 1
    Ac <- matrix(0, length(cg), length(members)); Ac[cbind(seq_along(cg), la)] <- 1
    cw <- Wc %*% Hc + (!Wc) %*% Ac
    cn <- colSums(Hc) + colSums(Ac)
    pos <- match(members, fbs_idx)
    conf_wins[, pos] <- cw
    conf_games[pos] <- as.integer(cn)
    pct <- cw / matrix(pmax(cn, 1), S, length(members), byrow = TRUE)
    rc <- r[, members, drop = FALSE]
    div <- NULL
    if (fmt$divisions[ci] && !is.null(divisions)) {
      div <- divisions$division[match(teams$team_id[members], divisions$team_id)]
      if (anyNA(div) || length(unique(div)) != 2) div <- NULL
    }

    title <- g |> dplyr::filter(ccg, home %in% members, away %in% members)
    if (nrow(title) >= 1) {
      # Title game already set: its teams (and result, once played)
      title <- title[1, ]
      seed1[, ci] <- title$home
      seed2[, ci] <- title$away
      adv <- title$home_adv
    } else {
      for (s in seq_len(S)) {
        sd <- conference_seeds(pct[s, ], lh, la, Wc[s, ], rc[s, ], div)
        seed1[s, ci] <- members[sd[1]]
        seed2[s, ci] <- members[sd[2]]
      }
      adv <- if (fmt$host[ci] == "top_seed") 1 else 0
    }
    if (nrow(title) >= 1 && title$done) {
      champ[, ci] <- if (title$home_points > title$away_points) title$home else title$away
      home_won <- champ[, ci] == seed1[, ci]
    } else {
      mu <- rating_rows(seed1[, ci], ccg_week) - rating_rows(seed2[, ci], ccg_week) + par$hfa * adv
      home_won <- margin_rng(mu, par, family) > 0
      champ[, ci] <- ifelse(home_won, seed1[, ci], seed2[, ci])
    }
    # The title game counts in the resume too
    rows <- seq_len(S)
    p1 <- win_prob(bench + par$hfa * adv - r[cbind(rows, seed2[, ci])], par, family)
    p2 <- win_prob(bench - par$hfa * adv - r[cbind(rows, seed1[, ci])], par, family)
    i1 <- cbind(rows, match(seed1[, ci], fbs_idx))
    i2 <- cbind(rows, match(seed2[, ci], fbs_idx))
    wab[i1] <- wab[i1] + home_won - p1
    wab[i2] <- wab[i2] + !home_won - p2
  }

  # CFP field from the final resume ranking
  rk <- rank_draws(wab)
  champ_pos <- matrix(match(champ, fbs_idx), S)
  cfp_seed <- matrix(NA_integer_, S, nf)
  auto <- matrix(FALSE, S, nf)
  for (s in seq_len(S)) {
    f <- cfp_field(rk[s, ], champ_pos[s, ])
    cfp_seed[s, ] <- f$seed
    auto[s, f$auto] <- TRUE
  }

  # Bowl pecking order: position among the conference's bowl-eligible teams outside the CFP,
  # by final resume rank
  avail <- eligible & is.na(cfp_seed)
  bowl_pick <- matrix(NA_integer_, S, nf)
  for (cf in fmt$conference) {
    pos <- which(teams$conference[fbs_idx] == cf)
    for (j in pos) {
      ahead <- rowSums(avail[, pos, drop = FALSE] & rk[, pos, drop = FALSE] < rk[, j])
      bowl_pick[, j] <- ifelse(avail[, j], ahead + 1L, NA_integer_)
    }
  }

  in_ccg <- matrix(FALSE, S, nf)
  is_champ <- matrix(FALSE, S, nf)
  for (ci in seq_len(nc)) {
    if (all(is.na(seed1[, ci]))) next
    rows <- seq_len(S)
    in_ccg[cbind(rows, match(seed1[, ci], fbs_idx))] <- TRUE
    in_ccg[cbind(rows, match(seed2[, ci], fbs_idx))] <- TRUE
    is_champ[cbind(rows, match(champ[, ci], fbs_idx))] <- TRUE
  }

  list(teams = teams, fbs_idx = fbs_idx, reg = reg, fut = fut, W = W, wins = wins, eligible = eligible,
       conf_wins = conf_wins, conf_games = conf_games, in_ccg = in_ccg, is_champ = is_champ,
       wab = wab, rk = rk, cfp_seed = cfp_seed, auto = auto, bowl_pick = bowl_pick,
       seed1 = seed1, seed2 = seed2, formats = fmt)
}

#' Per-team odds, final-record splits, paths and rooting interests for the dashboard
summarise_season_sim <- function(sim, input, divisions = NULL) {
  teams <- sim$teams
  fbs_idx <- sim$fbs_idx
  reg <- sim$reg
  W <- sim$W
  S <- nrow(W)
  nf <- length(fbs_idx)
  in_cfp <- !is.na(sim$cfp_seed)
  r3 <- \(x) round(x, 3)
  cmean <- function(x, cond) if (any(cond)) mean(x[cond]) else NA_real_

  # Team-perspective win indicator for each remaining game of team j (S x games)
  team_fut <- lapply(seq_len(nf), function(j) {
    t <- fbs_idx[j]
    gi <- sim$fut[reg$home[sim$fut] == t | reg$away[sim$fut] == t]
    won <- W[, gi, drop = FALSE]
    away <- reg$away[gi] == t
    won[, away] <- !won[, away]
    list(gi = gi, won = won)
  })
  done <- reg[reg$done, ]
  record_now <- function(t, conf_only = FALSE) {
    d <- if (conf_only) done[done$conference_game & teams$conference[done$home] == teams$conference[done$away], ] else done
    w <- sum((d$home == t & d$home_points > d$away_points) | (d$away == t & d$away_points > d$home_points))
    l <- sum((d$home == t & d$home_points < d$away_points) | (d$away == t & d$away_points < d$home_points))
    c(w, l)
  }

  team_rows <- lapply(seq_len(nf), function(j) {
    t <- fbs_idx[j]
    tf <- team_fut[[j]]
    win_out <- if (length(tf$gi)) rowSums(!tf$won) == 0 else rep(TRUE, S)
    rec <- record_now(t)
    crec <- record_now(t, conf_only = TRUE)
    picks <- sim$bowl_pick[, j]
    tibble::tibble(
      id = teams$team_id[t], team = teams$team[t], conf = teams$conference[t],
      div = if (is.null(divisions)) NA_character_ else divisions$division[match(teams$team_id[t], divisions$team_id)],
      w = rec[1], l = rec[2], cw = crec[1], cl = crec[2], cg = sim$conf_games[j], left = length(tf$gi),
      xw = r3(mean(sim$wins[, j])), xcw = r3(mean(sim$conf_wins[, j])),
      p_ccg = r3(mean(sim$in_ccg[, j])), p_title = r3(mean(sim$is_champ[, j])),
      p_bowl = r3(mean(sim$eligible[, j])), p_cfp = r3(mean(in_cfp[, j])),
      p_bye = r3(mean(in_cfp[, j] & sim$cfp_seed[, j] <= 4)),
      p_host = r3(mean(in_cfp[, j] & sim$cfp_seed[, j] >= 5 & sim$cfp_seed[, j] <= 8)),
      p_auto = r3(mean(sim$auto[, j])),
      wab = r3(mean(sim$wab[, j])), rrk = as.integer(stats::median(sim$rk[, j])),
      p_pick = r3(mean(!is.na(picks))),
      pick = if (any(!is.na(picks))) as.integer(stats::median(picks, na.rm = TRUE)) else NA_integer_,
      p_winout = r3(mean(win_out)),
      ccg_winout = r3(cmean(sim$in_ccg[, j], win_out)), title_winout = r3(cmean(sim$is_champ[, j], win_out)),
      cfp_winout = r3(cmean(in_cfp[, j], win_out))
    )
  })

  # P(title game) by final conference record
  records <- lapply(seq_len(nf), function(j) {
    final <- sim$conf_wins[, j]
    if (all(is.na(final))) return(NULL)
    k <- sort(unique(final))
    by <- \(x) vapply(k, \(v) mean(x[final == v]), numeric(1))
    tibble::tibble(id = teams$team_id[fbs_idx[j]], cw = as.integer(k), cl = as.integer(sim$conf_games[j] - k),
                   p = r3(vapply(k, \(v) mean(final == v), numeric(1))),
                   ccg = r3(by(sim$in_ccg[, j])), title = r3(by(sim$is_champ[, j])), cfp = r3(by(in_cfp[, j])))
  })

  # Each remaining game: odds after a win vs. a loss
  paths <- lapply(seq_len(nf), function(j) {
    tf <- team_fut[[j]]
    if (!length(tf$gi)) return(NULL)
    t <- fbs_idx[j]
    dplyr::bind_rows(lapply(seq_along(tf$gi), function(k) {
      gi <- tf$gi[k]
      won <- tf$won[, k]
      home <- reg$home[gi] == t
      opp <- if (home) reg$away[gi] else reg$home[gi]
      tibble::tibble(
        id = teams$team_id[t], game_id = reg$game_id[gi], wk = reg$week_seq[gi], label = reg$week_label[gi],
        opp_id = teams$team_id[opp], opp = teams$team[opp],
        venue = if (reg$home_adv[gi] == 0) "N" else if (home) "H" else "A",
        conf_game = reg$conference_game[gi],
        p_win = r3(mean(won)),
        ccg_w = r3(cmean(sim$in_ccg[, j], won)), ccg_l = r3(cmean(sim$in_ccg[, j], !won)),
        title_w = r3(cmean(sim$is_champ[, j], won)), title_l = r3(cmean(sim$is_champ[, j], !won)),
        cfp_w = r3(cmean(in_cfp[, j], won)), cfp_l = r3(cmean(in_cfp[, j], !won))
      )
    }))
  })

  # Rooting interests: the other remaining conference games that move a team's title-game odds most
  root <- lapply(seq_len(nf), function(j) {
    t <- fbs_idx[j]
    x <- sim$in_ccg[, j]
    if (mean(x) < 0.01 || mean(x) > 0.99) return(NULL)
    cf <- teams$conference[t]
    gi <- sim$fut[reg$conference_game[sim$fut] & teams$conference[reg$home[sim$fut]] == cf &
                    reg$home[sim$fut] != t & reg$away[sim$fut] != t]
    if (!length(gi)) return(NULL)
    hw <- W[, gi, drop = FALSE]
    swing <- vapply(seq_along(gi), \(k) cmean(x, hw[, k]) - cmean(x, !hw[, k]), numeric(1))
    keep <- order(-abs(swing))[seq_len(min(4, sum(!is.na(swing))))]
    keep <- keep[abs(swing[keep]) >= 0.01]
    if (!length(keep)) return(NULL)
    tibble::tibble(
      id = teams$team_id[t], game_id = reg$game_id[gi[keep]], wk = reg$week_seq[gi[keep]],
      root_id = teams$team_id[ifelse(swing[keep] > 0, reg$home[gi[keep]], reg$away[gi[keep]])],
      vs_id = teams$team_id[ifelse(swing[keep] > 0, reg$away[gi[keep]], reg$home[gi[keep]])],
      p_root = r3(vapply(keep, \(k) cmean(x, if (swing[k] > 0) hw[, k] else !hw[, k]), numeric(1))),
      p_vs = r3(vapply(keep, \(k) cmean(x, if (swing[k] > 0) !hw[, k] else hw[, k]), numeric(1)))
    )
  })

  # CFP seed distribution
  seeds <- dplyr::bind_rows(lapply(seq_len(nf), function(j) {
    s <- sim$cfp_seed[, j]
    k <- sort(unique(s[!is.na(s)]))
    if (!length(k)) return(NULL)
    tibble::tibble(id = teams$team_id[fbs_idx[j]], seed = as.integer(k), p = r3(vapply(k, \(x) mean(s %in% x), numeric(1))))
  }))

  list(
    teams = dplyr::bind_rows(team_rows),
    records = dplyr::bind_rows(records),
    paths = dplyr::bind_rows(paths),
    root = dplyr::bind_rows(root),
    seeds = seeds,
    formats = sim$formats,
    meta = list(season = input$season, week = input$week, week_label = input$week_label, sims = S,
                games_left = length(sim$fut), generated = format(Sys.time(), "%Y-%m-%d %H:%M %Z"))
  )
}

#' Refit the live season's latest snapshot and simulate the rest of its season. Same data, config
#' and seed as the published snapshot (run_snapshot()), so the posterior is the same; only this
#' one week is refit, because snapshots keep summaries, not draws.
run_season_sim <- function(inputs, season_data, cfg = model_config(), reps = 4L, seed = 2026L) {
  input <- inputs[[length(inputs)]]
  f <- fit_power(input, cfg)
  draws <- extract_draws(f$fit, family = cfg$margin_family)
  unlink(f$fit$output_files())
  thin <- unique(round(seq(1, nrow(draws$r), length.out = min(cfg$summary_draws, nrow(draws$r)))))
  draws <- list(r = draws$r[thin, , drop = FALSE], par = draws$par[thin, ], family = draws$family)
  divisions <- tryCatch(team_divisions(input$season), error = \(e) NULL)
  set.seed(seed)
  sim <- simulate_season(draws, input, season_data$games, divisions, reps = reps, benchmark_rank = cfg$benchmark_rank)
  summarise_season_sim(sim, input, divisions)
}
