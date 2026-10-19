# CFB Bayesian Rankings

Two college football rankings, rebuilt every week from 2014 on:

| | Question | Unit |
| :--- | :--- | :--- |
| **Power** | How good is a team right now? | Points vs. an average FBS team on a neutral field (A − B = predicted spread) |
| **Resume** | What has a team earned? | Wins Above Benchmark (WAB): wins beyond what the #12 FBS team would have managed with the same schedule |

Output: a static Quarto + Observable JS dashboard (`docs/dashboard.html`), two reports
(`docs/reports/season_retro.html`, `docs/reports/model_diagnostics.html`) and a methods paper
(`paper/cfb-paper.pdf`) whose every number is computed from the pipeline at render time.

## Method

**Power model** (`stan/power.stan`). Each team has one latent strength *r* (points). Every game
informs it through two signals that share it:

- *efficiency margin*: net predicted points added (CFBD PPA, garbage time excluded) ~ Normal(λ·μ, σₑ)
- *regulation scoreboard margin* (OT games count as ties) ~ Student-t(ν, μ + κ·(eff − λμ), σₘ)

with μ = r_home − r_away + hfa·(not neutral). Conditioning the margin on the efficiency residual
keeps the two signals from being counted as independent evidence; λ, κ, σ's are estimated, so
there are no hand-set weights.

Hierarchy: r = μ_conference + b·(last season's rating relative to its conference) + σ_tier·z,
with conferences nested in tiers (FBS anchored at 0, FCS, non-D1). *b* is the year-to-year
carry-over, estimated from the data, so early-season ratings lean on last season and the
data takes over as games are played. FBS, FCS and lower-division games are all included.

**Resume.** For each posterior draw, the benchmark is that draw's #12 FBS team. A win adds
1 − P(benchmark wins that game at that venue); a loss subtracts P(benchmark wins). Summing gives
WAB. Ranks, 80% intervals and P(top 12) come from ranking every draw. Also reported: SOR
probability (chance the benchmark matches the record), schedule strength (benchmark's expected
loss rate), expected wins and luck (wins − expected wins), Colley (record-only reference), and
AP / CFP / Coaches polls aligned to the same games.

**Validation.** Every weekly snapshot must pass convergence gates (R̂ ≤ 1.01, ESS ≥ 400,
no divergences, E-BFMI > 0.2); the published current-season snapshot halts the pipeline if it
fails after one careful retry. Each season's latest snapshot also gets posterior predictive
checks, `priorsense` prior sensitivity and PSIS-LOO. A rolling backtest predicts every game from
the previous week's snapshot and scores it against results and closing lines (MAE, Brier,
log loss, CRPS, interval coverage, calibration, ATS). For the paper: ablations (margin only,
Normal margin, no carry-over, no conference level, recency half-lives), baselines (least
squares, CFBD Elo, SP+, FPI), leave-one-season-out priors, simulation-based calibration,
benchmark ranks 4/12/25, and agreement with CFP committee selections.

**Priors** on the structural parameters (home field, tier offsets, carry-over, scales) are
centred on full-season refits of 2015–2025 under vague priors, with sd = 2× the between-season
sd (`prior_refits` target → `pool_priors()` → `priors_from_pool()`, rounded into
`model_config()`). `check_build.R` warns when a rebuild's refits drift from the configured
values.

## Pipeline (`targets`)

```
sync (CFBD API → data/raw JSON cache → data/cfb.duckdb)
  └ per season s: data_s → weeks_s → inputs_s[week] → snaps_s[week]      (dynamic branches)
                                      ↑ prior = final ratings of season s−1
                                    → checks_s (PPC, sensitivity, LOO)
  → snapshots → ratings (+ polls, CBS 138) → backtest → dashboard_data → dashboard, reports
  → analysis targets (prior_refits, ablation, baselines, final_refits, sbc, …) → paper
```

Only new weeks are refit: a week's inputs contain nothing from later weeks. Finished seasons
(synced after Feb 1 of the next year) never hit the API again.

| File | Contents |
| :--- | :--- |
| `R/01_ingest.R` | CFBD client, JSON cache, DuckDB tables (`games`, `game_stats`, `lines`, `polls`, `teams`, `sp_ratings`), concluded-week rule. SP+ is stored per published version (CFBD keeps no weekly history) and shown on each season's latest dashboard week only |
| `R/01_ingest_cbs.R` | CBS Sports 138: archive, Wayback/article backfill, poll alignment |
| `R/02_model.R` | Model config, Stan inputs per snapshot, fitting with gates |
| `R/03_rankings.R` | Power and Resume summaries, Colley, poll alignment |
| `R/04_validation.R` | Snapshot runner, PPC, prior sensitivity, backtest |
| `R/05_export.R` | Compact data for the dashboard |
| `R/06_supplement.R` | Full-draw refits and prior-perturbation study for the paper's appendix |
| `R/07_analysis.R` | Paper analyses: vague-prior refits and pooled priors, ablations, baselines, SP+/FPI, benchmark sensitivity, margin leakage, SBC, committee validation, data hashes |
| `R/08_season_sim.R` | Rest-of-season simulation for the live season (refits its latest week only): conference title races with generic tiebreakers, bowl eligibility, CFP field by final resume, paths and rooting interests for the dashboard's Conference and Postseason tabs |
| `paper/cfb-paper.qmd` | Methods paper (`tar_quarto` target; references in `paper/references.bib`) |
| `check_build.R` | Post-build checks (below) |
| `tests/testthat/` | Unit tests (`Rscript -e 'testthat::test_dir("tests/testthat")'`) |

## Running

```r
# once
renv::restore()                        # R packages pinned in renv.lock (or source("install_packages.R"))
cmdstanr::install_cmdstan()
# CFBD_API_KEY=... in .Renviron        (free key: collegefootballdata.com)

targets::tar_make()                    # sync, fit, render
```

Then `Rscript check_build.R`: errored targets, convergence gates for every snapshot, whether the
latest snapshot covers every concluded week, ratings sanity (team count, ranks, top 10s),
posterior predictive / prior-sensitivity / LOO flags, backtest vs. Vegas and calibration, and
output freshness. Exits non-zero on any FAIL; WARNs are worth a look but don't block publishing.

- A full rebuild fits ~225 weekly snapshots plus the paper's analysis targets (several hours
  on 8 cores; the analysis targets alone take ~3 h). Later runs fit only the new week (a few
  minutes); the analysis targets rerun only when the model code or priors change.
- `check_build.R` can also be `source()`d in an interactive session (it only exits non-zero
  under `Rscript`).
- `CFB_FIRST_SEASON=2024 Rscript -e 'targets::tar_make()'` for a quick partial build.
- The free CFBD tier allows ~1000 calls/month; a full backfill uses ~80, a weekly update ~6,
  and the `external_ratings` target (SP+/FPI) 24 when it reruns.
- Settings (sampler, priors, benchmark rank, recency half-life) live in `model_config()`.
- **CBS Sports 138** has no API: each sync archives the live ranking page to
  `data/raw/cbs138/`. Run at least once per week in season, or that week's full 138 is only
  available later as a Wayback Machine capture or an article's top 25. One-off backfill:
  `cbs_backfill_wayback(from = 2022)` and `cbs_fetch_articles(urls)` (`R/01_ingest_cbs.R`).

**Viewing the dashboard.** `docs/dashboard.html` is a single self-contained file, but Observable
JS will not run from `file://`. Serve it: `python3 -m http.server -d docs 8000` then open
<http://localhost:8000/dashboard.html> (or publish `docs/` with GitHub Pages / Quarto Pub).
The reports in `docs/reports/` open directly.
