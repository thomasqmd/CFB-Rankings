# Builds assets/course-data.js for the scoring course from the project's targets store.
# Run from the project root: Rscript assets/prep-course-data.R
suppressMessages(library(dplyr))
source("R/03_rankings.R")
set.seed(2026)

r1 <- function(x, d = 1) round(x, d)
clust_se <- function(d, cl) {
  m <- mean(d); u <- tapply(d - m, cl, sum)
  c(mean = m, se_naive = sd(d) / sqrt(length(d)), se_cl = sqrt(sum(u^2)) / length(d), n = length(d),
    n_clusters = length(u))
}

# ---------------------------------------------------------------- backtest
bt <- targets::tar_read(backtest)$games
pr <- targets::tar_read(snapshots)$predictions
x <- pr |> select(season, week, game_id, pred_q) |>
  inner_join(bt |> select(season, week, game_id, week_seq, margin, pred_margin, crps, brier, logloss,
                          fbs_game, vegas_margin, home_win),
             by = c("season", "week", "game_id"))
stopifnot(nrow(x) == nrow(bt))
x <- x |> filter(!vapply(pred_q, is.null, logical(1)))
cat("games with quantiles:", nrow(x), "\n")

x$pit <- cdf_from_quantiles(x$margin, x$pred_q)
qs <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
Q <- sapply(qs, function(p) quantile_at(x$pred_q, p))
# Log score: kernel density on the 199 stored quantiles (as scoringRules::logs_sample does with draws)
x$logs <- vapply(seq_len(nrow(x)), function(i) {
  q <- x$pred_q[[i]]; bw <- stats::bw.nrd0(q)
  -log(mean(stats::dnorm(x$margin[i], q, bw)))
}, numeric(1))
x$logs_norm <- vapply(seq_len(nrow(x)), function(i) {
  q <- x$pred_q[[i]]; -stats::dnorm(x$margin[i], mean(q), sd(q), log = TRUE)
}, numeric(1))
x$sd_q <- vapply(x$pred_q, sd, numeric(1))

# Climatology: each game forecast by the margins of FBS-FBS games in all *other* seasons
fb <- x |> filter(fbs_game)
clim_crps <- numeric(nrow(fb)); clim_pit <- numeric(nrow(fb))
for (s in unique(fb$season)) {
  pool <- fb$margin[fb$season != s]
  smp <- sample(pool, 3000)
  half <- 0.5 * mean(abs(smp - sample(smp)))
  ii <- which(fb$season == s)
  clim_crps[ii] <- vapply(fb$margin[ii], function(y) mean(abs(smp - y)) - half, numeric(1))
  # Margins are integers that pile up on 3 and 7, so use the randomised PIT for this step-function forecast
  Fc <- stats::ecdf(pool)
  clim_pit[ii] <- Fc(fb$margin[ii] - 1) + stats::runif(length(ii)) * (Fc(fb$margin[ii]) - Fc(fb$margin[ii] - 1))
}
# The line as a distributional forecast: the model's own distribution, recentred on the line
lined <- fb |> filter(!is.na(vegas_margin))
lined$crps_line <- crps_quantiles(lined$margin, Map(function(q, s) q + s, lined$pred_q, lined$vegas_margin - lined$pred_margin))
line_cmp <- clust_se(lined$crps - lined$crps_line, paste(lined$season, lined$week_seq))

# Interval sample for the interval-score explorer: FBS-FBS games
idx <- sample(which(x$fbs_game), 2500)
smp <- list(q = lapply(idx, function(i) r1(Q[i, ], 2)), y = x$margin[idx])

# Scatter sample: CRPS vs log score
sc <- sample(nrow(x), 1500)

# Mean Brier score at each threshold t over all games: its area is the mean CRPS
tgrid <- seq(-80, 80, by = 1)
bs_sum <- numeric(length(tgrid))
for (i in seq_len(nrow(x))) {
  q <- x$pred_q[[i]]; keep <- !duplicated(q)
  Ft <- stats::approx(q[keep], pred_probs[keep], tgrid, yleft = pred_probs[1] / 2,
                      yright = 1 - (1 - pred_probs[length(pred_probs)]) / 2, ties = "ordered")$y
  bs_sum <- bs_sum + (Ft - (x$margin[i] <= tgrid))^2
}
bs_curve <- list(t = tgrid, bs = round(bs_sum / nrow(x), 5))

pit_bins <- function(p, k = 20) as.integer(table(cut(p, seq(0, 1, length.out = k + 1), include.lowest = TRUE)))

bt_out <- list(
  n = nrow(x), n_fbs = sum(x$fbs_game),
  pit = r1(x$pit, 3),
  fbs = as.integer(x$fbs_game),
  crps_mean = mean(x$crps), crps_mean_fbs = mean(x$crps[x$fbs_game]),
  mae = mean(abs(x$pred_margin - x$margin)),
  brier_mean = mean(x$brier),
  logloss_mean = mean(x$logloss),
  logs_mean = mean(x$logs), logs_norm_mean = mean(x$logs_norm),
  sd_mean = mean(x$sd_q),
  width = c(w50 = mean(Q[, 5] - Q[, 3]), w80 = mean(Q[, 6] - Q[, 2]), w95 = mean(Q[, 7] - Q[, 1])),
  cover = c(c50 = mean(x$margin >= Q[, 3] & x$margin <= Q[, 5]),
            c80 = mean(x$margin >= Q[, 2] & x$margin <= Q[, 6]),
            c95 = mean(x$margin >= Q[, 1] & x$margin <= Q[, 7])),
  interval_sample = smp,
  scatter = list(crps = r1(x$crps[sc], 2), logs = r1(x$logs[sc], 3)),
  logs_cor = cor(x$crps, x$logs, method = "spearman"),
  clim = list(n = nrow(fb), crps_mean = mean(clim_crps), model_crps_same = mean(fb$crps),
              pit_bins = pit_bins(clim_pit), model_pit_bins = pit_bins(fb$pit),
              width80 = unname(diff(quantile(fb$margin, c(0.1, 0.9)))),
              model_width80 = mean(Q[x$fbs_game, 6] - Q[x$fbs_game, 2])),
  line = c(as.list(line_cmp), model_crps = mean(lined$crps), line_crps = mean(lined$crps_line)),
  bs_curve = bs_curve
)

# Example games (same two as Lesson 1)
ex <- function(i) list(y = x$margin[i], pit = x$pit[i], crps = x$crps[i], brier = x$brier[i],
                       logs = x$logs[i], logs_norm = x$logs_norm[i], mean = x$pred_margin[i],
                       sd = x$sd_q[i], q = r1(Q[i, ], 2))
g <- targets::tar_read(games_all)
key <- g |> filter((season == 2024 & home_team == "Ohio State" & away_team == "Michigan") |
                     (season == 2023 & home_team == "Florida State" & away_team == "Georgia")) |>
  select(season, game_id, home_team)
i_game <- which(x$game_id == key$game_id[key$home_team == "Ohio State"])
i_ob <- which(x$game_id == key$game_id[key$home_team == "Florida State"])
examples <- list(the_game = ex(i_game), orange_bowl = ex(i_ob))

# ---------------------------------------------------------------- model comparison (ablations)
ab <- targets::tar_read(ablation_scores)
pub <- ab |> filter(variant == "Published model") |> select(season, game_id, week_seq, c0 = crps, l0 = logloss, e0 = err, b0 = brier)
cmp <- lapply(setdiff(unique(ab$variant), "Published model"), function(v) {
  j <- ab |> filter(variant == v) |> select(season, game_id, c1 = crps, l1 = logloss, e1 = err, b1 = brier) |>
    inner_join(pub, by = c("season", "game_id"))
  cl <- paste(j$season, j$week_seq)
  cc <- clust_se(j$c1 - j$c0, cl); ll <- clust_se(j$l1 - j$l0, cl); mm <- clust_se(abs(j$e1) - abs(j$e0), cl)
  list(variant = v, n = nrow(j), n_clusters = unname(cc["n_clusters"]),
       crps = as.list(cc[c("mean", "se_naive", "se_cl")]), logloss = as.list(ll[c("mean", "se_naive", "se_cl")]),
       mae = as.list(mm[c("mean", "se_naive", "se_cl")]),
       crps_pub = mean(j$c0), crps_var = mean(j$c1),
       diffs = if (v %in% c("No carry-over", "Normal margin")) r1(j$c1 - j$c0, 2) else NULL,
       cluster = if (v %in% c("No carry-over", "Normal margin")) as.integer(factor(cl)) else NULL,
       by_week = j |> mutate(d = c1 - c0) |> group_by(week_seq) |>
         summarise(d = round(mean(d), 3), n = n(), .groups = "drop") |> as.list())
})
ab_seasons <- range(pub$season)

# ---------------------------------------------------------------- credible sets (2025 final posterior)
s <- targets::tar_read(supplement)$final
r <- unclass(posterior::as_draws_matrix(s$r))
tm <- s$teams; fbs <- tm$tier == 1
r <- r - rowMeans(r[, fbs])
rf <- r[, fbs]; nm <- tm$team[fbs]
rk <- t(apply(-rf, 1, rank, ties.method = "first"))
col <- function(team) which(nm == team)
par <- unclass(posterior::as_draws_matrix(s$par))
# Indiana vs Notre Dame on a neutral field, per draw, from the margin law (scale-t, unconditional
# on the efficiency signal); ignores a week of rating drift
mu <- rf[, col("Indiana")] - rf[, col("Notre Dame")]
sig_marg <- sqrt(par[, "sigma_m"]^2 + (par[, "kappa"] * par[, "sigma_eff"])^2)
sig_g <- sqrt(sig_marg^2 + par[, "gamma_m"]^2 * mu^2)
wp <- stats::pt(mu / sig_g, df = par[, "nu"])
# Same, with the game-noise parameters held at their posterior means: a monotone function of mu,
# so interval endpoints can be carried across the transformation draw by draw
sm_bar <- mean(sig_marg); g_bar <- mean(par[, "gamma_m"]); nu_bar <- mean(par[, "nu"])
wp_fixed <- stats::pt(mu / sqrt(sm_bar^2 + g_bar^2 * mu^2), df = nu_bar)

top <- order(colMeans(rf), decreasing = TRUE)[1:12]
hdiset <- function(v, lev = 0.8) {
  tb <- sort(table(v) / length(v), decreasing = TRUE); k <- which(cumsum(tb) >= lev)[1]
  sort(as.integer(names(tb)[1:k]))
}
tbl <- lapply(top, function(j) list(
  team = nm[j], mean = r1(mean(rf[, j]), 1), sd = r1(sd(rf[, j]), 2),
  lo = r1(quantile(rf[, j], 0.1), 1), hi = r1(quantile(rf[, j], 0.9), 1),
  rk_med = unname(quantile(rk[, j], 0.5, type = 1)),
  rk_lo = unname(quantile(rk[, j], 0.1, type = 1)), rk_hi = unname(quantile(rk[, j], 0.9, type = 1)),
  rk_hdi = hdiset(rk[, j]), p1 = mean(rk[, j] == 1), p12 = mean(rk[, j] <= 12)))

cred <- list(
  season = s$season, week_label = s$week_label, n_draws = nrow(rf),
  indiana_rating = r1(rf[, col("Indiana")], 2),
  nd_rank = as.integer(rk[, col("Notre Dame")]),
  ttu_rank = as.integer(rk[, col("Texas Tech")]),
  ind_nd_diff = r1(mu, 2), ind_nd_wp = r1(wp_fixed, 4),
  wp_params = list(sigma = sm_bar, gamma = g_bar, nu = nu_bar),
  nu = r1(par[, "nu"], 2),
  table = tbl
)

out <- list(bt = bt_out, examples = examples, cmp = cmp, ab_seasons = ab_seasons, cred = cred,
            built = as.character(Sys.Date()))
js <- paste0("// Generated by prep_course_data.R from the targets store on ", Sys.Date(), ". Do not edit.\n",
             "window.COURSE_DATA = ", jsonlite::toJSON(out, auto_unbox = TRUE, digits = NA, null = "null"), ";\n")
writeLines(js, "assets/course-data.js")
cat("bytes:", file.size("assets/course-data.js"), "\n")
str(bt_out[c("n", "crps_mean", "mae", "logs_mean", "logs_norm_mean", "width", "cover", "logs_cor")])
str(bt_out$clim[c("crps_mean", "model_crps_same", "width80", "model_width80")])
str(bt_out$line)
str(examples, max.level = 2)
for (c in cmp) cat(sprintf("%-20s n=%d clusters=%d dCRPS=%.3f se_naive=%.3f se_cl=%.3f | dLL=%.4f se_cl=%.4f | dMAE=%.3f\n",
                           c$variant, c$n, c$n_clusters, c$crps$mean, c$crps$se_naive, c$crps$se_cl, c$logloss$mean, c$logloss$se_cl, c$mae$mean))
cat("ab seasons", ab_seasons, "\n")
cat("bs_curve area", sum(bs_curve$bs), "at 0:", bs_curve$bs[bs_curve$t == 0], "\n")
cat("wp_fixed: mean", mean(wp_fixed), "q10", quantile(wp_fixed, .1), "q90", quantile(wp_fixed, .9), "\n")
cat("wp: mean", mean(wp), "q10", quantile(wp, .1), "q90", quantile(wp, .9), "\n")
for (t in tbl[1:8]) cat(t$team, t$rk_lo, t$rk_hi, paste(range(t$rk_hdi), collapse = "-"), length(t$rk_hdi), "\n")
