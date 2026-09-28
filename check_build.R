# ==============================================================================
# check_build.R: post-build checks. Run after targets::tar_make():
#   Rscript check_build.R
# Prints PASS / WARN / FAIL per check; exits with status 1 if anything FAILs.
# ==============================================================================

suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  source("R/01_ingest.R")
  source("R/02_model.R")
  source("R/07_analysis.R")
})

status <- character()
report <- function(level, what, detail = "") {
  status[length(status) + 1] <<- level
  cat(sprintf("%-4s  %-34s %s\n", level, what, detail))
}
section <- function(x) cat("\n==", x, "==\n")

# ---- 1. Pipeline --------------------------------------------------------------
section("Pipeline")
meta <- tar_meta(fields = c(name, error, warnings, time), complete_only = FALSE)
errs <- meta |> filter(!is.na(error))
if (nrow(errs)) {
  report("FAIL", "Errored targets", paste(errs$name, collapse = ", "))
  for (i in seq_len(nrow(errs))) cat("      ", errs$name[i], ":", errs$error[i], "\n")
} else {
  report("PASS", "Errored targets", "none")
}

# ---- 2. Convergence gates -------------------------------------------------------
section("Convergence (every weekly snapshot)")
snaps <- tar_read(snapshots)
p <- snaps$params
cur <- max(p$season)
bad <- p |> filter(!passed)
if (nrow(bad) == 0) {
  report("PASS", "Gates", sprintf("%d / %d snapshots", nrow(p), nrow(p)))
} else {
  report(if (any(bad$season == cur)) "FAIL" else "WARN", "Gates",
         sprintf("%d / %d snapshots failed", nrow(bad), nrow(p)))
  for (i in seq_len(nrow(bad))) with(bad[i, ], cat(sprintf(
    "      %d %-11s R-hat %.3f  ESS %.0f/%.0f  div %d  E-BFMI %.2f\n",
    season, week_label, max_rhat, min_ess_bulk, min_ess_tail, divergences, min_ebfmi)))
}
retried <- sum(p$attempts > 1)
report(if (retried > 0.1 * nrow(p)) "WARN" else "PASS", "Retries needed", sprintf("%d snapshots", retried))

# ---- 3. Freshness ------------------------------------------------------------------
section("Freshness")
latest <- p |> filter(season == cur) |> slice_max(week, n = 1)
report("INFO", "Latest snapshot", sprintf("%d %s (%d games)", cur, latest$week_label, latest$n_games))
sd_cur <- tar_read_raw(paste0("data_", cur))
wk_done <- concluded_weeks(sd_cur$games)
expected <- if (length(wk_done)) max(wk_done) else 0L
report(if (latest$week >= expected) "PASS" else "FAIL", "Covers all concluded weeks",
       sprintf("snapshot week %d, concluded through %d", latest$week, expected))
unplayed <- sd_cur$games |> filter(week_seq <= latest$week, !completed)
report(if (nrow(unplayed)) "WARN" else "PASS", "Unfinished games in snapshot weeks",
       if (nrow(unplayed)) paste(nrow(unplayed), "(postponed/cancelled?)") else "none")

rt <- tar_read(ratings) |> filter(season == cur, week == latest$week, tier == 1)
n_cbs <- sum(!is.na(rt$cbs_rank))
report(if (n_cbs >= 130) "PASS" else if (n_cbs > 0) "WARN" else "FAIL", "CBS Sports 138 for latest week",
       sprintf("%d teams ranked%s", n_cbs, if (n_cbs > 0 && n_cbs < 130) " (top 25 only: no full-page capture)" else ""))

# ---- 4. Ratings sanity -----------------------------------------------------------
section("Ratings sanity (latest snapshot)")
r <- snaps$ratings |> filter(season == cur, week == latest$week)
fbs <- r |> filter(tier == 1)
report(if (nrow(fbs) >= 125 && nrow(fbs) <= 140) "PASS" else "WARN", "FBS team count", nrow(fbs))
report(if (anyNA(fbs$power) || anyNA(fbs$wab)) "FAIL" else "PASS", "Missing ratings",
       sum(is.na(fbs$power)) + sum(is.na(fbs$wab)))
report(if (anyDuplicated(fbs$power_rank) || anyDuplicated(fbs$resume_rank)) "FAIL" else "PASS",
       "Ranks unique", "")
top <- fbs |> arrange(power_rank) |> head(10)
cat("      Power top 10:  ", paste(top$team, collapse = ", "), "\n")
top <- fbs |> arrange(resume_rank) |> head(10)
cat("      Resume top 10: ", paste(top$team, collapse = ", "), "\n")
cat(sprintf("      hfa %.2f | b_prev %.2f | FCS offset %.1f | nu %.1f\n",
            latest$hfa, latest$b_prev, latest$fcs_offset, latest$nu))
report(if (between(latest$hfa, 0.5, 5)) "PASS" else "WARN", "Home field plausible", sprintf("%.2f pts", latest$hfa))
pt <- tar_read(teams_meta)
nologo <- setdiff(fbs$team_id, pt$team_id[!is.na(pt$logo)])
report(if (length(nologo)) "WARN" else "PASS", "FBS logos", sprintf("%d missing", length(nologo)))

# ---- 5. Model checks (each season's final snapshot) ------------------------------
section("Model checks")
ck <- Filter(Negate(is.null), tar_read(checks))
ppc <- bind_rows(lapply(ck, \(x) mutate(x$ppc, season = x$season)))
extreme <- ppc |> filter(p_value < 0.01 | p_value > 0.99)
report(if (nrow(extreme)) "WARN" else "PASS", "Posterior predictive p-values",
       sprintf("%d / %d extreme", nrow(extreme), nrow(ppc)))
if (nrow(extreme)) print(count(extreme, statistic, name = "seasons"))
sens <- bind_rows(lapply(ck, \(x) mutate(x$sensitivity, season = x$season)))
if ("diagnosis" %in% names(sens)) {
  flagged <- sens |> filter(diagnosis != "-") |> count(variable, diagnosis, name = "seasons")
  report(if (nrow(flagged)) "WARN" else "PASS", "Prior sensitivity (priorsense)",
         sprintf("%d parameter flags", sum(flagged$seasons)))
  if (nrow(flagged)) print(as.data.frame(flagged), row.names = FALSE)
}
lo <- bind_rows(lapply(ck, \(x) mutate(x$loo, season = x$season)))
report(if (any(lo$n_bad_k > 25, na.rm = TRUE)) "WARN" else "PASS", "PSIS-LOO Pareto k > 0.7",
       paste(sprintf("%d:%d", lo$season, lo$n_bad_k), collapse = " "))

# ---- 6. Backtest -----------------------------------------------------------------
section("Backtest (predicted from the previous week)")
bt <- tar_read(backtest)
o <- bt$overall |> filter(scope == "FBS vs FBS")
gap <- o$mae_model_lined - o$mae_vegas
report(if (gap > 1.5) "WARN" else "PASS", "MAE vs Vegas (FBS)",
       sprintf("model %.2f, Vegas %.2f (gap %.2f)", o$mae_model_lined, o$mae_vegas, gap))
cal <- bt$calibration |> filter(n >= 50)
worst <- max(abs(cal$pred - cal$obs))
report(if (worst > 0.05) "WARN" else "PASS", "Calibration", sprintf("worst bin off by %.1f pts", 100 * worst))
bs <- bt$by_season |> filter(season == cur)
if (nrow(bs)) cat(sprintf("      %d so far: %d games, MAE %.2f (Vegas %.2f), accuracy %.1f%%\n",
                          cur, bs$games, bs$mae_model_lined, bs$mae_vegas, 100 * bs$accuracy))

# ---- 7. Priors ------------------------------------------------------------------
# The published priors were set from the vague-prior refits; after a rebuild the refits move a
# little (new data, new carry-over), so flag a prior mean that has drifted by > 0.5 prior sd
section("Priors vs. the vague-prior refits")
if (tar_exist_objects("prior_refits") && !"prior_refits" %in% errs$name) {
  pooled <- pool_priors(tar_read(prior_refits))
  cfg <- tar_read(cfg)
  now <- priors_from_pool(cfg, pooled)
  pairs <- list(hfa = c(cfg$hfa_mean, now$hfa_mean, cfg$hfa_sd), b_prev = c(cfg$b_prev_mean, now$b_prev_mean, cfg$b_prev_sd),
                fcs = c(cfg$tier_mean[1], now$tier_mean[1], cfg$tier_sd[1]), nond1 = c(cfg$tier_mean[2], now$tier_mean[2], cfg$tier_sd[2]),
                sigma_conf = c(cfg$sigma_conf_prior[1], now$sigma_conf_prior[1], cfg$sigma_conf_prior[2]),
                lambda = c(cfg$lambda_prior[1], now$lambda_prior[1], cfg$lambda_prior[2]),
                kappa = c(cfg$kappa_prior[1], now$kappa_prior[1], cfg$kappa_prior[2]),
                sigma_eff = c(cfg$sigma_eff_prior[1], now$sigma_eff_prior[1], cfg$sigma_eff_prior[2]),
                sigma_m = c(cfg$sigma_m_prior[1], now$sigma_m_prior[1], cfg$sigma_m_prior[2]))
  drift <- vapply(pairs, \(x) abs(x[2] - x[1]) / x[3], numeric(1))
  report(if (any(drift > 0.5)) "WARN" else "PASS", "Prior means vs. pooled refits",
         sprintf("largest drift %.2f prior sd (%s)", max(drift), names(which.max(drift))))
} else {
  report("WARN", "Prior means vs. pooled refits", "prior_refits not built")
}

# ---- 8. Outputs ------------------------------------------------------------------
section("Outputs")
built <- max(meta$time[meta$name == "snapshots"], na.rm = TRUE)
for (f in c("docs/dashboard.html", "docs/reports/season_retro.html", "docs/reports/model_diagnostics.html",
            "paper/cfb-paper.pdf")) {
  if (!file.exists(f)) { report("FAIL", f, "missing"); next }
  info <- file.info(f)
  fresh <- info$mtime >= built
  report(if (fresh) "PASS" else "FAIL", f, sprintf("%.1f MB, %s", info$size / 1e6, format(info$mtime, "%b %d %H:%M")))
}

cat(sprintf("\n%d FAIL, %d WARN\n", sum(status == "FAIL"), sum(status == "WARN")))
if (any(status == "FAIL")) quit(status = 1)
