# ==============================================================================
# _targets.R — CFB Bayesian Rankings
#
#   sync (CFBD -> DuckDB)
#     -> data_<s>                 one season's games, efficiency, lines, polls
#     -> weeks_<s>                concluded weeks
#     -> inputs_<s> [per week]    Stan inputs (priors = final ratings of seasons s-1, s-2)
#     -> snaps_<s>  [per week]    Power + Resume summaries for that week
#     -> checks_<s>               PPC, prior sensitivity, LOO for the latest week
#   -> season_sim                 rest-of-season simulation (live season's latest week only)
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

# Core budget (CFB_CORES, default all cores). Each Stan fit runs 4 chains, so CORES / 4 targets
# fit at once on crew workers. The analysis targets fork their own fits (`workers`), so they run
# in the main process, one at a time, and size their workers to the same budget.
CORES <- as.integer(Sys.getenv("CFB_CORES", parallel::detectCores()))
FIT_WORKERS <- max(1L, CORES %/% 4L)

tar_option_set(
  packages = c("dplyr", "tidyr", "tibble", "posterior", "cmdstanr"),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = FIT_WORKERS, seconds_idle = 60)
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
  prior2 <- if (s - 2L < FIRST_SEASON) quote(NULL) else call("final_prior", as.symbol(paste0("snaps_", s - 2L)))
  is_current <- s == CURRENT_SEASON
  list(
    tar_target_raw(paste0("data_", s), bquote({ sync; read_season(.(s), db_path = .(DB_PATH)) })),
    # Week 0 = preseason snapshot (prior only), available once a previous season exists
    tar_target_raw(paste0("weeks_", s), bquote(c(
      .(if (s > FIRST_SEASON) 0L else integer(0)),
      concluded_weeks(.(sym("data"))$games, min_share = .(if (is_current) 0.9 else 0))
    ))),
    tar_target_raw(paste0("inputs_", s),
      bquote(prepare_week_input(.(sym("data")), .(sym("weeks")), prior = .(prior), cfg = cfg, prior2 = .(prior2))),
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
# Ablation seasons only, not season_data / weeks_all: those include the live season, which
# changes every sync and would rerun the whole ablation each week
ablation_call <- function(prefix) {
  as.call(c(quote(list), setNames(lapply(paste0(prefix, "_", ABLATION_SEASONS), as.symbol), ABLATION_SEASONS)))
}
# Final ratings of season s - k for each season s in `seasons` (NULL before the first season)
prior_list_call <- function(seasons, k) {
  as.call(c(quote(list), setNames(lapply(seasons, \(s) if (s - k < FIRST_SEASON) NULL else
    call("final_prior", as.symbol(paste0("snaps_", s - k)))), seasons)))
}
VARIANT_SEASONS <- (FIRST_SEASON + 1L):LAST_FULL
# Final-week input of each complete season
final_inputs_call <- bquote(lapply(stats::setNames(.(as.call(c(quote(list), input_syms(COMPLETE)))), COMPLETE),
                                   \(x) x[[length(x)]]))

list(
  tar_target(stan_file, "stan/power.stan", format = "file"),
  # Compiles the model once here, so parallel fits never race to rebuild the same executable
  tar_target(cfg_base, {
    cmdstanr::cmdstan_model(stan_file)
    cfg <- model_config(); cfg$stan_file <- stan_file; cfg
  }),
  # Margin family: compared on every complete season (vague priors, no carry-over, so this does
  # not depend on the published fits); the winner by pooled LOO becomes the published family
  tar_target_raw("margin_comparison",
    bquote(run_margin_comparison(.(complete_data_call), cfg = cfg_base, workers = FIT_WORKERS)),
    deployment = "main"),
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
  tar_target_raw("prior_sensitivity",
    bquote(run_prior_sensitivity(.(last_full_inputs), cfg = cfg, workers = FIT_WORKERS)),
    deployment = "main"),
  tar_target(prior_sensitivity_2020,
    run_prior_sensitivity(inputs_2020, cfg = cfg, one_at_a_time = FALSE, workers = FIT_WORKERS),
    deployment = "main"),
  tar_target_raw("induced_prior", bquote({
    inp <- .(last_full_inputs)
    induced_priors(inp[[length(inp)]], cfg = cfg)
  })),
  # Rest-of-season simulation (conference races, CFP, bowls) for the live season only: refits its
  # latest week (snapshots keep summaries, not draws), so no other season reruns
  tar_target_raw("season_sim", bquote(run_season_sim(.(as.symbol(paste0("inputs_", CURRENT_SEASON))),
                                                     .(as.symbol(paste0("data_", CURRENT_SEASON))), cfg = cfg))),
  tar_target(ratings, attach_polls(snapshots$ratings, season_data)),
  tar_target(games_all, dplyr::bind_rows(lapply(season_data, `[[`, "games"))),
  tar_target(backtest, run_backtest(snapshots$predictions, games_all, snapshots$params)),

  # ---- Analyses for the paper (R/07_analysis.R) ----
  tar_target_raw("weeks_all", as.call(c(quote(list), setNames(weeks_syms, SEASONS)))),
  # Full-season refits under vague priors: the evidence for the published priors
  tar_target_raw("prior_refits",
    bquote(run_prior_refits(.(final_inputs_call), cfg = cfg, workers = FIT_WORKERS)),
    deployment = "main"),
  # The priors the refits imply under the published pooling rule (check_build.R compares them
  # with model_config())
  tar_target(pooled_priors, pool_priors(prior_refits, pool = cfg$pool)),
  tar_target(prior_loso_check, loso_prior_check(prior_refits, pool = cfg$pool)),
  # Weeks 0-3 of each season under priors pooled from the other seasons only
  tar_target_raw("loso_backtest", bquote(run_loso_backtest(
    .(as.call(c(quote(list), setNames(input_syms((FIRST_SEASON + 1L):LAST_FULL), (FIRST_SEASON + 1L):LAST_FULL)))),
    prior_refits, cfg = cfg, workers = FIT_WORKERS)), deployment = "main"),
  # Ablation fits run 2 chains (fast_config), so twice as many fit at once
  tar_target_raw("ablation", bquote(run_ablation(
    .(ablation_call("data")),
    .(as.call(c(quote(list), setNames(lapply(ABLATION_SEASONS - 1L, \(s) call("final_prior", as.symbol(paste0("snaps_", s)))),
                                      ABLATION_SEASONS)))),
    .(ablation_call("weeks")), cfg = cfg, workers = 2L * FIT_WORKERS,
    priors2 = .(prior_list_call(ABLATION_SEASONS, 2L)))), deployment = "main"),
  # Early-week comparison of prior settings: the priors before the 2026-10 rework, the published
  # ones, and the published model without each preseason term (plans/todo.md)
  tar_target_raw("prior_variants", bquote(run_prior_variants(
    .(as.call(c(quote(list), setNames(lapply(paste0("data_", VARIANT_SEASONS), as.symbol), VARIANT_SEASONS)))),
    .(prior_list_call(VARIANT_SEASONS, 1L)), .(prior_list_call(VARIANT_SEASONS, 2L)),
    .(as.call(c(quote(list), setNames(lapply(paste0("weeks_", VARIANT_SEASONS), as.symbol), VARIANT_SEASONS)))),
    prior_variant_set(cfg), cfg = cfg, workers = 2L * FIT_WORKERS)), deployment = "main"),
  tar_target(prior_variant_scores, score_variant_predictions(prior_variants$preds, games_all)),
  tar_target(prior_variant_summary, list(
    overall = compare_variants(prior_variant_scores, "Published"),
    by_week = compare_variants(prior_variant_scores, "Published", by_week = TRUE))),
  tar_target(baselines, run_baselines(season_data, weeks_all)),
  tar_target(external_ratings, run_external_ratings(COMPLETE, teams_meta)),
  tar_target_raw("final_refits",
    bquote(run_final_refits(.(final_inputs_call), cfg = cfg, workers = FIT_WORKERS)),
    deployment = "main"),
  tar_target_raw("margin_leakage", bquote({
    inp <- .(last_full_inputs)
    run_margin_leakage(inp[[length(inp)]], cfg = cfg, workers = FIT_WORKERS)
  }), deployment = "main"),
  tar_target_raw("sbc", bquote({
    inp <- .(last_full_inputs)
    run_sbc(inp[[which(vapply(inp, `[[`, integer(1), "week") == 8L)]], cfg = cfg, workers = FIT_WORKERS)
  }), deployment = "main"),
  tar_target(ablation_scores, score_predictions(ablation, games_all)),
  tar_target(loso_scores, score_predictions(loso_backtest, games_all)),
  tar_target(baseline_scores, score_predictions(baselines, games_all)),
  tar_target(schedule_graph, schedule_components(games_all, snapshots$params)),
  tar_target(resume_check, resume_validation(ratings, games_all, last_full)),
  tar_target(data_hashes, data_fingerprint(season_data)),
  tar_target(dashboard_data, build_dashboard_data(ratings, snapshots, games_all, teams_meta, backtest, cfg)),

  # The stylesheets and navbar logo are tracked too, so editing only them re-renders the dashboard
  tar_quarto(dashboard, path = "docs/dashboard.qmd", quiet = TRUE,
             extra_files = c("docs/styles/dashboard.scss", "docs/styles/dashboard-light.scss", "docs/styles/dashboard-dark.scss",
                             "logos/TRlogo-light.svg")),
  tar_quarto(report_season, path = "docs/reports/season_retro.qmd", quiet = TRUE),
  tar_quarto(report_diagnostics, path = "docs/reports/model_diagnostics.qmd", quiet = TRUE),
  tar_quarto(paper, path = "paper/cfb-paper.qmd", quiet = TRUE)
)
