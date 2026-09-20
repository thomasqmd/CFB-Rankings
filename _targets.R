# ==============================================================================
# Pipeline Definition: _targets.R
# Purpose: Orchestrates CFB Bayesian 4-Pipeline Ranking Engine workflow via targets
#          (WLD: Actual Discrete, PD: Actual Continuous,
#           XWLD: Expected Discrete, XPD: Expected Continuous)
# ==============================================================================

library(targets)
library(tarchetypes)

# Set target pipeline options
tar_option_set(
  packages = c(
    "cfbfastR",
    "tidyverse",
    "duckdb",
    "DBI",
    "posterior",
    "cmdstanr",
    "gt",
    "gtExtras",
    "plotly",
    "quarto"
  ),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE
)

# Source all modular R scripts
tar_source("R")

# Pipeline Target Graph Definition
list(
  # 1. Season & Week tracker
  tar_target(
    name = current_season_info,
    command = get_latest_cfb_week(),
    cue = tar_cue(mode = "always")
  ),
  
  # 2. Sync analytical store (DuckDB)
  tar_target(
    name = duckdb_sync,
    command = pull_and_sync_duckdb(
      season = current_season_info$season, 
      db_path = "data/cfb.duckdb"
    )
  ),
  
  # 3. Transform PBP & Schedules into 4 Modeling Datasets
  tar_target(
    name = modeling_data,
    command = transform_all_pipelines(
      season = current_season_info$season,
      db_path = duckdb_sync$db_path,
      filter_garbage_time = TRUE
    )
  ),
  
  # 4. Fit Pipeline 1: WLD (Ordered Logit Actual Outcome Resume Engine)
  tar_target(
    name = fit_wld,
    command = fit_wld_model(
      stan_data = modeling_data$stan_data_wld,
      stan_file = "stan/wld_ordered_logit.stan",
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      adapt_delta = 0.95,
      warm_start = TRUE,
      warm_start_warmup = 300
    )
  ),

  # 5. Fit Pipeline 2: PD (Robust Student-t Actual Point Differential Engine)
  tar_target(
    name = fit_pd,
    command = fit_pd_model(
      stan_data = modeling_data$stan_data_pd,
      stan_file = "stan/pd_student_t.stan",
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      adapt_delta = 0.95,
      warm_start = TRUE,
      warm_start_warmup = 300
    )
  ),

  # 6. Fit Pipeline 3: XWLD (Ordered Logit Expected Discrete Outcome Engine)
  tar_target(
    name = fit_xwld,
    command = fit_xwld_model(
      stan_data = modeling_data$stan_data_xwld,
      stan_file = "stan/xwld_ordered_logit.stan",
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      adapt_delta = 0.95,
      warm_start = TRUE,
      warm_start_warmup = 300
    )
  ),
  
  # 7. Fit Pipeline 4: XPD (Robust Student-t Net EPA Differential Engine)
  tar_target(
    name = fit_xpd,
    command = fit_xpd_model(
      stan_data = modeling_data$stan_data_xpd,
      stan_file = "stan/xpd_student_t.stan",
      chains = 4,
      iter_warmup = 1000,
      iter_sampling = 1000,
      adapt_delta = 0.95,
      warm_start = TRUE,
      warm_start_warmup = 300
    )
  ),
  
  # 8. MCMC Convergence Diagnostics Gates for all 4 Pipelines
  tar_target(
    name = diagnostics_wld,
    command = evaluate_mcmc_health(
      fit = fit_wld,
      model_name = "WLD Ordered Logit (Pipeline 1)",
      strict_halt = FALSE
    )
  ),

  tar_target(
    name = diagnostics_pd,
    command = evaluate_mcmc_health(
      fit = fit_pd,
      model_name = "PD Student-t (Pipeline 2)",
      strict_halt = FALSE
    )
  ),

  tar_target(
    name = diagnostics_xwld,
    command = evaluate_mcmc_health(
      fit = fit_xwld,
      model_name = "XWLD Ordered Logit (Pipeline 3)",
      strict_halt = FALSE
    )
  ),
  
  tar_target(
    name = diagnostics_xpd,
    command = evaluate_mcmc_health(
      fit = fit_xpd,
      model_name = "XPD Student-t (Pipeline 4)",
      strict_halt = FALSE
    )
  ),
  
  # 9. Prior Sensitivity Sweep (FCS Hyperparameters)
  tar_target(
    name = prior_sensitivity_wld,
    command = run_prior_sensitivity_sweep(
      stan_data = modeling_data$stan_data_wld,
      stan_file = "stan/wld_ordered_logit.stan",
      team_lookup = modeling_data$team_lookup,
      model_type = "wld"
    )
  ),
  
  # 10. Sonneborn-Berger & Buchholz Tie-Break Derivation
  tar_target(
    name = sb_ratings,
    command = compute_sonneborn_berger(
      fit_wld = fit_wld,
      wld_df = modeling_data$wld_df,
      team_lookup = modeling_data$team_lookup
    )
  ),
  
  # 11. 4-Pipeline Composite Rankings (WLD + PD + XWLD + XPD Fusion)
  tar_target(
    name = composite_rankings,
    command = compute_composite_rankings(
      sb_ratings = sb_ratings,
      fit_pd = fit_pd,
      fit_xwld = fit_xwld,
      fit_xpd = fit_xpd,
      team_lookup = modeling_data$team_lookup,
      weights = c(wld = 0.35, pd = 0.25, xwld = 0.15, xpd = 0.25)
    )
  ),
  
  # 12. Colley Matrix Non-Predictive Benchmark
  tar_target(
    name = colley_rankings,
    command = compute_colley_matrix(
      wld_df = modeling_data$wld_df,
      team_lookup = modeling_data$team_lookup
    )
  ),
  
  # 13. External Polling Data (AP Top 25, Coaches, CFP)
  tar_target(
    name = external_polls,
    command = fetch_external_polls(
      season = current_season_info$season,
      week = current_season_info$week
    )
  ),
  
  # 14. Benchmark Comparison & Divergence Diagnostics
  tar_target(
    name = benchmark_comparison,
    command = align_and_benchmark_rankings(
      composite_rankings = composite_rankings,
      colley_rankings = colley_rankings,
      external_polls = external_polls
    )
  ),
  
  # 15. Quarto Weekly Briefing Report
  tar_quarto(
    name = render_weekly_briefing,
    path = "docs/reports/weekly_briefing.qmd"
  ),
  
  # 16. Quarto Season Retrospective Report
  tar_quarto(
    name = render_season_retro,
    path = "docs/reports/season_retro.qmd"
  ),
  
  # 17. Revealjs Methodology Presentation
  tar_quarto(
    name = render_methodology_presentation,
    path = "docs/presentation/methodology.qmd"
  )
)
