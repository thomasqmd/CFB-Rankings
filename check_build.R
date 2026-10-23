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
  source("R/06_supplement.R")
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
cat(sprintf("      hfa %.2f | b_prev %.2f | FCS offset %.1f | family %s | nu %.1f | tau %.2f | base %.1f\n",
            latest$hfa, latest$b_prev, latest$fcs_offset, latest$family, latest$nu, latest$tau, latest$base))
report(if (anyNA(fbs$off) || anyNA(fbs$def)) "FAIL" else "PASS", "Offence / defence ratings",
       sprintf("max |off - def - power| = %.2f", max(abs(fbs$off - fbs$def - fbs$power))))
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

# ---- 5b. Margin family -------------------------------------------------------------
section("Margin family (pooled LOO over complete seasons)")
if (tar_exist_objects("margin_families")) {
  mf <- tar_read(margin_families)
  cfg_now <- tar_read(cfg)
  report("INFO", "Published family", sprintf("%s (incumbent %s)", cfg_now$margin_family, cfg_now$margin_choice$incumbent))
  print(as.data.frame(mf$pooled |> mutate(across(where(is.numeric), \(x) round(x, 1)))), row.names = FALSE)
  report(if (all(mf$per_season$passed)) "PASS" else "WARN", "Comparison fits passed gates",
         sprintf("%d / %d", sum(mf$per_season$passed), nrow(mf$per_season)))
  w7 <- mf$ppc |> filter(statistic == "Share within 7 pts") |> group_by(family) |>
    summarise(extreme = sum(p_value < 0.01 | p_value > 0.99), seasons = n(), .groups = "drop")
  cat("      'Share within 7 pts' extreme (p < .01 or > .99):",
      paste(sprintf("%s %d/%d", w7$family, w7$extreme, w7$seasons), collapse = " | "), "\n")
}

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
if (!is.na(o$mae_total_model_ou)) report("INFO", "Total MAE vs over/under (FBS)",
       sprintf("model %.2f, O/U %.2f on %d games", o$mae_total_model_ou, o$mae_total_vegas, o$games_with_ou))
report(if (abs(o$cover80 - 0.8) > 0.05) "WARN" else "PASS", "80% interval coverage (FBS)", sprintf("%.1f%%", 100 * o$cover80))
bs <- bt$by_season |> filter(season == cur)
if (nrow(bs)) cat(sprintf("      %d so far: %d games, MAE %.2f (Vegas %.2f), accuracy %.1f%%\n",
                          cur, bs$games, bs$mae_model_lined, bs$mae_vegas, 100 * bs$accuracy))

# ---- 7. Priors ------------------------------------------------------------------
# The published priors were set from the vague-prior refits; after a rebuild the refits move a
# little (new data, new carry-over), so flag a prior mean that has drifted by > 0.5 prior sd
section("Priors vs. the vague-prior refits")
if (tar_exist_objects("prior_refits") && !"prior_refits" %in% errs$name) {
  cfg <- tar_read(cfg)
  pooled <- pool_priors(tar_read(prior_refits), pool = cfg$pool)
  now <- priors_from_pool(cfg, pooled)
  pairs <- list(hfa = c(cfg$hfa_mean, now$hfa_mean, cfg$hfa_sd), b_prev = c(cfg$b_prev_mean, now$b_prev_mean, cfg$b_prev_sd),
                fcs = c(cfg$tier_mean[1], now$tier_mean[1], cfg$tier_sd[1]), nond1 = c(cfg$tier_mean[2], now$tier_mean[2], cfg$tier_sd[2]),
                sigma_conf = c(cfg$sigma_conf_prior[1], now$sigma_conf_prior[1], cfg$sigma_conf_prior[2]),
                lambda = c(cfg$lambda_prior[1], now$lambda_prior[1], cfg$lambda_prior[2]),
                kappa = c(cfg$kappa_prior[1], now$kappa_prior[1], cfg$kappa_prior[2]),
                sigma_eff = c(cfg$sigma_eff_prior[1], now$sigma_eff_prior[1], cfg$sigma_eff_prior[2]),
                sigma_m = c(cfg$sigma_m_prior[1], now$sigma_m_prior[1], cfg$sigma_m_prior[2]),
                tau_r = c(cfg$tau_r_prior[1], now$tau_r_prior[1], cfg$tau_r_prior[2]),
                tau_t = c(cfg$tau_t_prior[1], now$tau_t_prior[1], cfg$tau_t_prior[2]),
                base = c(cfg$base_prior[1], now$base_prior[1], cfg$base_prior[2]),
                sigma_tot = c(cfg$sigma_tot_prior[1], now$sigma_tot_prior[1], cfg$sigma_tot_prior[2]),
                sigma_es = c(cfg$sigma_es_prior[1], now$sigma_es_prior[1], cfg$sigma_es_prior[2]),
                lambda_t = c(cfg$lambda_t_prior[1], now$lambda_t_prior[1], cfg$lambda_t_prior[2]),
                kappa_t = c(cfg$kappa_t_prior[1], now$kappa_t_prior[1], cfg$kappa_t_prior[2]),
                sigma_t_team = c(cfg$sigma_t_team_prior[1], now$sigma_t_team_prior[1], cfg$sigma_t_team_prior[2]),
                b_prev_t = c(cfg$b_prev_t_prior[1], now$b_prev_t_prior[1], cfg$b_prev_t_prior[2]),
                nu = c(cfg$nu_prior[1] / cfg$nu_prior[2], now$nu_prior[1] / now$nu_prior[2], sqrt(cfg$nu_prior[1]) / cfg$nu_prior[2]))
  for (k in names(preseason_labels)) if (isTRUE(cfg[[k]])) {
    for (f in switch(k, b_prev_tier = "b_prev_low_prior", use_ret = c("b_ret_prior", "c_ret_prior"), use_tal = "b_tal_prior",
                     use_prev2 = "b_prev2_prior", use_coach = "delta_coach_prior"))
      pairs[[sub("_prior$", "", f)]] <- c(cfg[[f]][1], now[[f]][1], cfg[[f]][2])
  }
  drift <- vapply(pairs, \(x) abs(x[2] - x[1]) / x[3], numeric(1))
  report(if (any(drift > 0.5)) "WARN" else "PASS", "Prior means vs. pooled refits",
         sprintf("largest drift %.2f prior sd (%s)", max(drift), names(which.max(drift))))
  if (any(drift > 0.5)) {
    cat("      Pooled priors to copy into model_config():\n")
    for (nm in c("hfa_mean", "hfa_sd", "tier_mean", "tier_sd", "b_prev_mean", "b_prev_sd", "sigma_team_mean",
                 "sigma_team_sd", "sigma_conf_prior", "lambda_prior", "kappa_prior", "sigma_eff_prior", "sigma_m_prior",
                 "tau_r_prior", "tau_t_prior", "gamma_prior", "base_prior", "t_tier_mean", "t_tier_sd",
                 "sigma_t_team_prior", "sigma_conf_t_prior", "b_prev_t_prior", "lambda_t_prior", "alpha_es_prior",
                 "kappa_t_prior", "sigma_es_prior", "sigma_tot_prior", "nu_prior", "b_prev_low_prior", "b_ret_prior",
                 "c_ret_prior", "b_tal_prior", "b_prev2_prior", "delta_coach_prior"))
      cat(sprintf("        %s = c(%s)\n", nm, paste(signif(now[[nm]], 4), collapse = ", ")))
  }
} else {
  report("WARN", "Prior means vs. pooled refits", "prior_refits not built")
}

# ---- 8. Outputs ------------------------------------------------------------------
section("Outputs")
built <- max(meta$time[meta$name == "snapshots"], na.rm = TRUE)
for (f in c("docs/index.html", "docs/reports/season_retro.html", "docs/reports/model_diagnostics.html",
            "paper/cfb-paper.pdf")) {
  if (!file.exists(f)) { report("FAIL", f, "missing"); next }
  info <- file.info(f)
  fresh <- info$mtime >= built
  report(if (fresh) "PASS" else "FAIL", f, sprintf("%.1f MB, %s", info$size / 1e6, format(info$mtime, "%b %d %H:%M")))
}

cat(sprintf("\n%d FAIL, %d WARN\n", sum(status == "FAIL"), sum(status == "WARN")))
if (any(status == "FAIL") && !interactive()) quit(status = 1)
