# ==============================================================================
# R/05_export.R
# Compact, column-oriented data for the Observable JS dashboard (passed via ojs_define).
# ==============================================================================

#' Power conferences (P5 through 2023, P4 from 2024), Group of 5/6, independents
conference_group <- function(conf, season, tier) {
  power <- ifelse(season < 2024, conf %in% c("SEC", "Big Ten", "Big 12", "ACC", "Pac-12"),
                  conf %in% c("SEC", "Big Ten", "Big 12", "ACC"))
  dplyr::case_when(tier != 1L ~ "FCS", conf == "FBS Independents" ~ "Independent",
                   power ~ "Power", TRUE ~ "Group")
}

clean_color <- function(x) ifelse(is.na(x) | !grepl("^#[0-9A-Fa-f]{6}$", x), NA_character_, x)

#' @param sp each season's latest SP+ (read_sp()). CFBD has no weekly SP+ history, so it is shown on
#'   each season's latest snapshot only: final SP+ for finished seasons, the newest fetch for the live one.
#' @param game_times kickoff-time-TBD flags (read_game_times()); dates and times are US Central
#' @return list of data frames, each small enough to inline in a static HTML page
build_dashboard_data <- function(ratings, snapshots, games_all, teams_meta, backtest, cfg, sp = NULL,
                                 game_times = NULL) {
  params <- snapshots$params

  last_week <- params |> dplyr::group_by(season) |> dplyr::summarise(last = max(week), .groups = "drop")

  # FBS rows for every snapshot; FCS rows only for each season's latest snapshot (matchups)
  rank_rows <- ratings |>
    dplyr::left_join(last_week, by = "season") |>
    dplyr::filter(tier == 1L | (tier == 2L & week == last)) |>
    dplyr::transmute(
      s = season, w = week, id = team_id, team, conf = conference, tier,
      g = games, wins, losses,
      pw = round(power, 1), pw_sd = round(power_sd, 2), pw_lo = round(power_lo, 1), pw_hi = round(power_hi, 1),
      prk = power_rank, prk_lo = power_rank_lo, prk_hi = power_rank_hi, p_pw1 = round(power_p1, 3),
      wab = round(wab, 2), wab_lo = round(wab_lo, 2), wab_hi = round(wab_hi, 2),
      rrk = resume_rank, rrk_lo = resume_rank_lo, rrk_hi = resume_rank_hi,
      p4 = round(resume_p4, 3), p12 = round(resume_p12, 3),
      sor = signif(sor_prob, 3), sos = round(sos, 3), sos_rk = sos_rank,
      xw = round(xwins, 1), luck = round(luck, 1), cw = close_w, cl = close_l,
      colley = colley_rank, ap = ap_rank, coaches = coaches_rank, cfp = cfp_rank, cbs = cbs_rank,
      off = round(off, 1), off_sd = round(off_sd, 2), off_rk = off_rank,
      def = round(def, 1), def_sd = round(def_sd, 2), def_rk = def_rank
    ) |>
    dplyr::group_by(s, w, tier) |>
    dplyr::mutate(sor_rk = ifelse(g > 0, rank(sor, ties.method = "min", na.last = "keep"), NA_integer_)) |>
    dplyr::ungroup() |>
    dplyr::mutate(grp = conference_group(conf, s, tier), .after = conf)
  if (!is.null(sp) && nrow(sp) > 0) {
    sp_rows <- sp |>
      dplyr::inner_join(last_week, by = "season") |>
      dplyr::transmute(s = season, w = last, id = team_id, sp = round(sp, 1), sp_rk = sp_rank,
                       sp_off = round(sp_off, 1), sp_def = round(sp_def, 1))
    rank_rows <- rank_rows |> dplyr::left_join(sp_rows, by = c("s", "w", "id"))
  }

  snaps_meta <- params |>
    dplyr::transmute(s = season, w = week, label = week_label, n_games, has_prior, family,
                     hfa = round(hfa, 2), sigma = round(sigma, 2), nu = round(nu, 1),
                     noise_sd = round(noise_sd, 2), noise_slope = round(noise_slope, 4),
                     tau = round(tau, 2), base = round(base, 2),
                     b_prev = round(b_prev, 2), passed, max_rhat = round(max_rhat, 3),
                     min_ess = round(pmin(min_ess_bulk, min_ess_tail)), divergences)

  # Pre-game view of each game: the prediction made by the previous week's snapshot
  preds <- snapshots$predictions
  for (nm in c("pred_home_pts", "pred_away_pts")) if (!nm %in% names(preds)) preds[[nm]] <- NA_real_
  pre <- preds |>
    dplyr::inner_join(games_all |> dplyr::select(season, game_id, week_seq), by = c("season", "game_id")) |>
    dplyr::filter(week_seq == week + 1L) |>
    dplyr::select(season, game_id, pre_margin = pred_margin, pre_wp = pred_home_wp,
                  pre_hp = pred_home_pts, pre_ap = pred_away_pts)
  # Remaining games of the live season: prediction from the latest snapshot
  latest <- params |> dplyr::filter(season == max(season)) |> dplyr::pull(week) |> max()
  future <- preds |>
    dplyr::filter(season == max(params$season), week == latest) |>
    dplyr::select(season, game_id, fut_margin = pred_margin, fut_wp = pred_home_wp,
                  fut_hp = pred_home_pts, fut_ap = pred_away_pts)
  # Resume contribution of each played game, valued with the latest opponent strength
  contrib <- snapshots$game_resume |>
    dplyr::left_join(last_week, by = "season") |>
    dplyr::filter(week == last) |>
    dplyr::select(season, game_id, team_id, wab_game, p_bench)

  # The closing line as a home win probability (the backtest recentres the model's predictive
  # distribution on the line), so the dashboard can compare Brier scores with Vegas
  line_wp <- backtest$games |> dplyr::filter(!is.na(line_wp)) |> dplyr::select(season, game_id, line_wp)

  fbs_ids <- unique(ratings$team_id[ratings$tier == 1L])
  tbd <- if (is.null(game_times)) FALSE else
    dplyr::left_join(games_all[c("season", "game_id")], game_times, by = c("season", "game_id"))$start_time_tbd
  g <- dplyr::bind_cols(games_all, kickoff_central(games_all$start_date, tbd)) |>
    dplyr::left_join(pre, by = c("season", "game_id")) |>
    dplyr::left_join(future, by = c("season", "game_id")) |>
    dplyr::left_join(line_wp, by = c("season", "game_id")) |>
    dplyr::mutate(eff_margin = ifelse(!is.na(home_ppa) & !is.na(away_ppa),
                                      home_ppa * home_plays - away_ppa * away_plays, NA_real_))
  side <- function(df, home) {
    sgn <- if (home) 1 else -1
    df |> dplyr::transmute(
      s = season, wk = week_seq, label = week_label, date, time, game_id,
      id = if (home) home_id else away_id, opp_id = if (home) away_id else home_id,
      team = if (home) home_team else away_team, opp = if (home) away_team else home_team,
      venue = dplyr::case_when(neutral_site ~ "N", home ~ "H", TRUE ~ "A"),
      pts = if (home) home_points else away_points, opp_pts = if (home) away_points else home_points,
      ot = ot,
      pre_wp = round(if (home) pre_wp else 1 - pre_wp, 3),
      pre_margin = round(sgn * pre_margin, 1),
      fut_wp = round(if (home) fut_wp else 1 - fut_wp, 3),
      fut_margin = round(sgn * fut_margin, 1),
      # projected score: this team's points, then the opponent's
      pre_pts = round(if (home) pre_hp else pre_ap, 1), pre_opp_pts = round(if (home) pre_ap else pre_hp, 1),
      fut_pts = round(if (home) fut_hp else fut_ap, 1), fut_opp_pts = round(if (home) fut_ap else fut_hp, 1),
      ou = over_under,
      eff = round(sgn * eff_margin, 1),
      line = round(-sgn * spread, 1),
      line_wp = round(if (home) line_wp else 1 - line_wp, 3)
    )
  }
  game_rows <- dplyr::bind_rows(side(g, TRUE), side(g, FALSE)) |>
    dplyr::filter(id %in% fbs_ids) |>
    dplyr::left_join(contrib |> dplyr::rename(s = season, id = team_id), by = c("s", "game_id", "id")) |>
    dplyr::mutate(wab_game = round(wab_game, 2), p_bench = round(p_bench, 3),
                  result = dplyr::case_when(is.na(pts) | is.na(opp_pts) ~ NA_character_,
                                            pts > opp_pts ~ "W", TRUE ~ "L")) |>
    dplyr::arrange(s, id, wk, date)

  teams <- teams_meta |>
    dplyr::filter(team_id %in% unique(c(ratings$team_id))) |>
    dplyr::transmute(id = team_id, school, abbr = abbreviation, mascot,
                     color = clean_color(color), alt = clean_color(alt_color), logo, logo_dark)

  list(
    snaps = snaps_meta,
    ranks = rank_rows,
    games = game_rows,
    teams = teams,
    backtest = list(
      overall = backtest$overall |> dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3))),
      by_season = backtest$by_season |> dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3))),
      calibration = backtest$calibration |> dplyr::mutate(bin = as.character(bin),
                                                          dplyr::across(where(is.numeric), \(x) round(x, 3)))
    ),
    config = list(benchmark_rank = cfg$benchmark_rank, margin_family = cfg$margin_family,
                  generated = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
                  # "%-d" (no leading zero) isn't portable, so build "Oct 6" by hand
                  sp_asof = if (is.null(sp) || nrow(sp) == 0) NULL else {
                    at <- max(sp$fetched[sp$season == max(sp$season)])
                    paste(format(at, "%b"), as.integer(format(at, "%d")))
                  })
  )
}
