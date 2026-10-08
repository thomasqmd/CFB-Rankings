# ==============================================================================
# _targets.R — CFB Bayesian Rankings
#
#   sync (CFBD -> DuckDB)
#     -> data_<s>                 one season's games, efficiency, lines, polls
#     -> weeks_<s>                concluded weeks
#     -> inputs_<s> [per week]    Stan inputs (prior = final ratings of season s-1)
#     -> snaps_<s>  [per week]    Power + Resume summaries for that week
#     -> checks_<s>               PPC, prior sensitivity, LOO for the latest week
#   -> snapshots, backtest, dashboard_data -> Quarto dashboard + reports
#   -> supplement                 full-draw refits for the paper's supplementary checks
#   -> prior_refits, ablation, ... analyses for the paper (R/07_analysis.R) -> paper
#
#   cfg_base -> margin_comparison (each margin family on every complete season) -> cfg: the
#   published margin family is chosen before any snapshot is fit
#
# Seasons: CFB_FIRST_SEASON (env, default 2014) through the current season.
# ==============================================================================

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "tidyr", "tibble", "posterior", "cmdstanr"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE
)

tar_source("R")

FIRST_SEASON <- as.integer(Sys.getenv("CFB_FIRST_SEASON", "2014"))
CURRENT_SEASON <- current_cfb_season()
LAST_FULL <- last_complete_season()      # the paper's worked example and supplementary refits
SEASONS <- FIRST_SEASON:CURRENT_SEASON
COMPLETE <- FIRST_SEASON:LAST_FULL
ABLATION_SEASONS <- (LAST_FULL - 3L):LAST_FULL
DB_PATH <- "data/cfb.duckdb"

season_targets <- function(s) {
  sym <- function(x) as.symbol(paste0(x, "_", s))
  prior <- if (s == FIRST_SEASON) quote(NULL) else call("final_prior", as.symbol(paste0("snaps_", s - 1L)))
  is_current <- s == CURRENT_SEASON
  list(
    tar_target_raw(paste0("data_", s), bquote({ sync; read_season(.(s), db_path = .(DB_PATH)) })),
    # Week 0 = preseason snapshot (prior only), available once a previous season exists
    tar_target_raw(paste0("weeks_", s), bquote(c(
      .(if (s > FIRST_SEASON) 0L else integer(0)),
      concluded_weeks(.(sym("data"))$games, min_share = .(if (is_current) 0.9 else 0))
    ))),
    tar_target_raw(paste0("inputs_", s),
      bquote(prepare_week_input(.(sym("data")), .(sym("weeks")), prior = .(prior), cfg = cfg)),
      pattern = bquote(map(.(sym("weeks")))), iteration = "list"),
    tar_target_raw(paste0("snaps_", s),
      bquote(run_snapshot(.(sym("inputs")), cfg = cfg)),
      pattern = bquote(map(.(sym("inputs")))), iteration = "list"),
    # The published (current-season) snapshot must pass the convergence gates
    tar_target_raw(paste0("checks_", s),
      bquote(run_checks(.(sym("inputs")), cfg = cfg, strict = .(is_current))))
  )
}

per_season <- lapply(SEASONS, season_targets)
snap_syms <- lapply(paste0("snaps_", SEASONS), as.symbol)
data_syms <- lapply(paste0("data_", SEASONS), as.symbol)
check_syms <- lapply(paste0("checks_", SEASONS), as.symbol)
input_syms <- function(s) lapply(paste0("inputs_", s), as.symbol)
weeks_syms <- lapply(paste0("weeks_", SEASONS), as.symbol)
last_full_inputs <- as.symbol(paste0("inputs_", LAST_FULL))
complete_data_call <- as.call(c(quote(list), setNames(lapply(paste0("data_", COMPLETE), as.symbol), COMPLETE)))
# Final-week input of each complete season
final_inputs_call <- bquote(lapply(stats::setNames(.(as.call(c(quote(list), input_syms(COMPLETE)))), COMPLETE),
                                   \(x) x[[length(x)]]))

list(
  tar_target(stan_file, "stan/power.stan", format = "file"),
  tar_target(cfg_base, { cfg <- model_config(); cfg$stan_file <- stan_file; cfg }),
  # Margin family: compared on every complete season (vague priors, no carry-over, so this does
  # not depend on the published fits); the winner by pooled LOO becomes the published family
  tar_target_raw("margin_comparison", bquote(run_margin_comparison(.(complete_data_call), cfg = cfg_base))),
  tar_target(margin_families, compare_margin_families(margin_comparison, cfg_base$margin_family)),
  tar_target(cfg, choose_margin_family(cfg_base, margin_comparison)),

  # Refreshes live seasons each run (final seasons make no API calls); returns a data
  # fingerprint so downstream targets only rerun when something actually changed
  tar_target(sync, sync_all(SEASONS, db_path = DB_PATH), cue = tar_cue(mode = "always")),
  tar_target(teams_meta, { sync; read_teams(DB_PATH) }),

  per_season,

  tar_target_raw("season_data", as.call(c(quote(list), setNames(data_syms, SEASONS)))),
  tar_target_raw("snapshots", as.call(c(quote(combine_snapshots), snap_syms))),
  tar_target_raw("checks", as.call(c(quote(list), setNames(check_syms, SEASONS)))),
  # Full-draw refits (week 1 and final week of the last complete season) for the
  # paper's supplementary posterior checks
  tar_target(last_full, LAST_FULL),
  tar_target_raw("supplement", bquote(run_supplement(.(last_full_inputs), cfg = cfg))),
  # Refits under widened / shifted priors (joint and one at a time), and for an older season;
  # and the priors' induced distributions
  tar_target_raw("prior_sensitivity", bquote(run_prior_sensitivity(.(last_full_inputs), cfg = cfg))),
  tar_target(prior_sensitivity_2020, run_prior_sensitivity(inputs_2020, cfg = cfg, one_at_a_time = FALSE)),
  tar_target_raw("induced_prior", bquote({
    inp <- .(last_full_inputs)
    induced_priors(inp[[length(inp)]], cfg = cfg)
  })),
  tar_target(ratings, attach_polls(snapshots$ratings, season_data)),
  tar_target(games_all, dplyr::bind_rows(lapply(season_data, `[[`, "games"))),
  tar_target(backtest, run_backtest(snapshots$predictions, games_all, snapshots$params)),

  # ---- Analyses for the paper (R/07_analysis.R) ----
  tar_target_raw("weeks_all", as.call(c(quote(list), setNames(weeks_syms, SEASONS)))),
  # Full-season refits under vague priors: the evidence for the published priors
  tar_target_raw("prior_refits", bquote(run_prior_refits(.(final_inputs_call), cfg = cfg))),
  # Weeks 0-3 of each season under priors pooled from the other seasons only
  tar_target_raw("loso_backtest", bquote(run_loso_backtest(
    .(as.call(c(quote(list), setNames(input_syms((FIRST_SEASON + 1L):LAST_FULL), (FIRST_SEASON + 1L):LAST_FULL)))),
    prior_refits, cfg = cfg))),
  tar_target_raw("ablation", bquote(run_ablation(
    season_data[as.character(.(ABLATION_SEASONS))],
    .(as.call(c(quote(list), setNames(lapply(ABLATION_SEASONS - 1L, \(s) call("final_prior", as.symbol(paste0("snaps_", s)))),
                                      ABLATION_SEASONS)))),
    weeks_all[as.character(.(ABLATION_SEASONS))], cfg = cfg))),
  tar_target(baselines, run_baselines(season_data, weeks_all)),
  tar_target(external_ratings, run_external_ratings(COMPLETE, teams_meta)),
  tar_target_raw("final_refits", bquote(run_final_refits(.(final_inputs_call), cfg = cfg))),
  tar_target_raw("margin_leakage", bquote({
    inp <- .(last_full_inputs)
    run_margin_leakage(inp[[length(inp)]], cfg = cfg)
  })),
  tar_target_raw("sbc", bquote({
    inp <- .(last_full_inputs)
    run_sbc(inp[[which(vapply(inp, `[[`, integer(1), "week") == 8L)]], cfg = cfg)
  })),
  tar_target(ablation_scores, score_predictions(ablation, games_all)),
  tar_target(loso_scores, score_predictions(loso_backtest, games_all)),
  tar_target(baseline_scores, score_predictions(baselines, games_all)),
  tar_target(schedule_graph, schedule_components(games_all, snapshots$params)),
  tar_target(resume_check, resume_validation(ratings, games_all, last_full)),
  tar_target(data_hashes, data_fingerprint(season_data)),
  tar_target(dashboard_data, build_dashboard_data(ratings, snapshots, games_all, teams_meta, backtest, cfg)),

  tar_quarto(dashboard, path = "docs/dashboard.qmd", quiet = TRUE),
  tar_quarto(report_season, path = "docs/reports/season_retro.qmd", quiet = TRUE),
  tar_quarto(report_diagnostics, path = "docs/reports/model_diagnostics.qmd", quiet = TRUE),
  tar_quarto(paper, path = "paper/cfb-paper.qmd", quiet = TRUE)
)
