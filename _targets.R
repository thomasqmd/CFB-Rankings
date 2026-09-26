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
SEASONS <- FIRST_SEASON:CURRENT_SEASON
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

list(
  tar_target(stan_file, "stan/power.stan", format = "file"),
  tar_target(cfg, { cfg <- model_config(); cfg$stan_file <- stan_file; cfg }),

  # Refreshes live seasons each run (final seasons make no API calls); returns a data
  # fingerprint so downstream targets only rerun when something actually changed
  tar_target(sync, sync_all(SEASONS, db_path = DB_PATH), cue = tar_cue(mode = "always")),
  tar_target(teams_meta, { sync; read_teams(DB_PATH) }),

  per_season,

  tar_target_raw("season_data", as.call(c(quote(list), setNames(data_syms, SEASONS)))),
  tar_target_raw("snapshots", as.call(c(quote(combine_snapshots), snap_syms))),
  tar_target_raw("checks", as.call(c(quote(list), setNames(check_syms, SEASONS)))),
  tar_target(ratings, attach_polls(snapshots$ratings, season_data)),
  tar_target(games_all, dplyr::bind_rows(lapply(season_data, `[[`, "games"))),
  tar_target(backtest, run_backtest(snapshots$predictions, games_all)),
  tar_target(dashboard_data, build_dashboard_data(ratings, snapshots, games_all, teams_meta, backtest, cfg)),

  tar_quarto(dashboard, path = "docs/dashboard.qmd", quiet = TRUE),
  tar_quarto(report_season, path = "docs/reports/season_retro.qmd", quiet = TRUE),
  tar_quarto(report_diagnostics, path = "docs/reports/model_diagnostics.qmd", quiet = TRUE)
)
