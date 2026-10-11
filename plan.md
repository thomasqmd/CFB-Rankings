# CFB Bayesian Rankings — Overhaul Plan

Status: review completed 2026-09-26 against commit `c374378`; overhaul implemented the same day.
Last updated 2026-10-06 (offence/defence ratings, margin families, weekly random walk; see
TODO). Previously 2026-09-28 (CBS Sports 138 reinstated, D8; paper review P1–P5 implemented,
new priors set, full rebuild done and paper rendered).
The previous spec (4-pipeline WLD/PD/XWLD/XPD matrix + Φ-blended composites) is in git history.

Decisions: Resume benchmark = #12 FBS team; Colley is the record-only reference; seasons start
at 2014; dashboard is Quarto + Observable JS.

Two published products, kept strictly separate:

| Product | Question | Built from |
| :--- | :--- | :--- |
| **Power rating** (points) | Who wins on a neutral field next week, and by how much? | One joint Bayesian model of scoreboard margin + opponent-adjusted efficiency |
| **Resume ranking** | Given what happened, what did each team earn? | Actual W/L results valued against opponent strength (from the Power posterior) |

---

## A. Verified defects (fix regardless of redesign)

Each item was reproduced against the current DuckDB / `_targets` store.

- [x] **A1. Team-name matching corrupts EPA for any game with "miss" in both names.**
  `R/02_transform.R:222-253` and `R/01_ingest_duckdb.R:723-755` use
  `home LIKE '%miss%' AND posteam LIKE '%miss%'`, which also matches Missouri, Mississippi State,
  Ole Miss, Southern Miss, Mississippi Valley State. Every play gets assigned to the home side, so
  the whole game collapses into one drive: e.g. 2025 Missouri–Mississippi State, 2025 Egg Bowl,
  2024 Mississippi State–Missouri all show `team_epa = 7, opp_epa = 0`. These games feed XPD/XWLD.
  Fix: key everything on CFBD team IDs (`home_id`/`away_id`, `pos_team_id`/`offense_id`), store
  one canonical name at ingest, delete the LIKE ladders.
- [x] **A2. Overtime detection is a substring match on `notes`.** `str_detect(notes, "ot|...")`
  matches "F**oot**ball", "C**ot**ton", "P**ot**ato", "Sc**oot**er's": every CFP game is coded OT,
  and real OT games (notes are empty) are never detected. 2026 data has 0 OT wins and 1 "OT loss"
  (TCU–UNC, Aer Lingus *College Football* Classic). Fix: derive OT from line scores
  (`length(home_line_scores) > 4`) or CFBD game `periods`.
- [x] **A3. SB and Buchholz are sums over games**, so schedule credit scales with games played.
  USC is SOR #1 largely because it has 4 games vs 3 for everyone else (SB 5.83 vs ~4.2). Also
  Michigan is SOR #7 with Power #62. Superseded by C2, but wrong as-is.
- [x] **A4. AP poll cache never refreshes.** `fetch_external_polls(season)` caches to
  `polls_<season>_latest.rds` and returns it forever; same for `cbs134_latest_latest.rds`.
  Poll week is also not aligned to `max_week` (CFBD poll week *w+1* = poll after week *w* games).
- [x] **A5. Fabricated "CBS 134" benchmark.** When scraping fails, `align_and_benchmark_rankings()`
  blends AP + Colley and labels it "CBS Sports 134". Remove the fallback (drop CBS entirely; the
  scrape is brittle and untracked by targets). *Later reversed:* the real CBS Sports 138 is back as
  an archived, targets-tracked benchmark with no synthetic fallback (D8).
- [x] **A6. Historical seasons in the dashboard are synthetic.** `build_season_bundle()` invents
  θ/δ/ω/α from records plus `rnorm()` noise for every season except the current one, and there is
  a further fully-random fallback generator (`docs/dashboard.qmd:414-457`). Fit real models for
  every season (D1) and delete both generators.
- [x] **A7. January games of a finished season can be frozen out.** `is_completed_season <- s < curr_year`
  freezes season *s* on Jan 1 of *s+1*, before bowls/CFP finish. Freeze only after the national
  championship (or e.g. Feb 1).
- [x] **A8. Postseason week numbers collide with regular-season week 1.** Weekly summaries and
  `week <= max_week` filters group on `week` alone. Add `week_seq` at ingest
  (postseason = last regular week + 1) and use it everywhere.
- [x] **A9. Week-resolution rules disagree with their docs** (docstring says ≥95% complete; code
  uses ≥60% or ≥20 games, plus a ≥25-games-by-22:00 bypass). Pick one rule, document it, test it.
- [x] **A10. XPD ignores `filter_garbage_time`** (parameter accepted, never used).
- [x] **A11. Only FBS-involved games are ingested**, so FCS teams have 1–2 games and their ratings
  are essentially the prior; ~90 FCS/D-II teams are lumped in one "FCS" tier. Ingest FCS schedules
  (`division = "fcs"`), and put non-D1 opponents in a third tier (or one pooled node).
- [x] **A12. Warm-start inits are a hidden side effect.** `data/inits/*.rds` is written outside
  targets, so a fit depends on untracked disk state. Models are small: drop warm-starting.
- [x] **A13. Prior-sensitivity sweep is mostly VI noise.** Meanfield ADVI with one seed per cell;
  the grid column called `sigma_fcs` is actually the prior *sd of μ_FCS*. Only WLD is swept.
- [x] **A14. Diagnostics never gate anything** (`strict_halt = FALSE` everywhere).
- [x] **A15. Season label is wrong**: selector shows "2026-27"; CFB seasons are single-year.

## B. Methodology problems (why the current rankings aren't sound)

- **B1. Ad-hoc weighting.** SOR = 0.40·Φ(zθ) + 0.30·Φ(zSB) + 0.20·Φ(zδ) + 0.10·Φ(zBH) + H2H;
  Power = 0.40·zα + 0.25·zEPA + 0.20·zω + 0.15·zδ. No weight is estimated or validated.
- **B2. Schedule is counted 3–4 times in SOR.** θ is already opponent-adjusted; SB re-adds opponent
  quality; Buchholz rewards *playing* good teams regardless of result (a 0-12 SEC team gets SOS
  credit); H2H adds it again.
- **B3. XWLD is XPD discretized.** PGWE = logistic(0.15·net_xp) binned at 0.25/0.5/0.75 is a
  monotone function of XPD with thresholds at ±7.3; the "OT" tiers are mislabelled. It adds no
  information, only weight.
- **B4. "XPD/EPA" isn't EPA.** It's a hand-coded drive value (7 for a TD, a step function of
  min yards-to-goal otherwise) from possession changes inferred by clock ordering; TD drives score
  actual points, so it correlates 0.88 with margin. Real CFBD PPA is already stored and is better.
- **B5. The WLD ordered logit isn't symmetric.** Free cutpoints let c₂ ≠ 0, which acts as an
  intercept confounded with the arbitrary home/away labelling on neutral sites; with OT categories
  near-empty (A2) it is effectively a binary logit anyway. γ_home ~ N(0.2, 0.1²) is also tight
  and low (CFB home edge is ≈0.3–0.4 logits).
- **B6. `power_rating` is not a calibrated spread.** A re-standardized blend of correlated z-scores
  multiplied by sd(δ) has no reason to equal points; projections then add PD's home edge to it.
  Never checked against outcomes.
- **B7. Posterior uncertainty is thrown away.** Everything is computed from posterior medians; no
  rank intervals, no P(top-12).
- **B8. Luck metrics are rank differences.** Rank gaps are non-linear (huge in the dense middle,
  small at the top). Use wins/points.
- **B9. No preseason information and no time dynamics.** Week-1–4 power ratings rest on 1–4 games;
  a team's week-1 game counts the same as last week's in November.
- **B10. No out-of-sample validation at all** (no backtest, no Vegas/SP+/FPI comparison, no PPC,
  `log_lik` computed but unused).
- **B11. Garbage-time rule is crude** (2nd half, |margin| > 28 in *offense–defense* score).

## C. Target methodology

### C1. Power model (one Stan model, points scale) — replaces PD, XPD, XWLD, ridge EPA, Power blend
- Latent strength `r[i]` in points. Two likelihoods per game sharing it:
  - Scoreboard margin (regulation margin; OT games → 0): Student-t(ν, r_h − r_a + hfa·home, σ_m).
  - Efficiency margin: net PPA/play × expected plays (or per-play with its own scale `λ`):
    Normal(λ·(r_h − r_a + hfa·home), σ_e). The model learns how much to trust each via σ_m, σ_e —
    no hand-set weights.
  - Optional later: split into offense/defense ratings (gives projected scores directly).
- Hierarchy: `r[i] ~ N(μ_conf[i], σ_team)`, `μ_conf ~ N(μ_tier, σ_conf)`, tiers FBS/FCS/non-D1,
  FBS tier anchored at 0.
- Preseason prior: `r[i] ~ N(ρ·r_prev[i], σ_pre)` from last season's final posterior (ρ, σ_pre fit
  on history), new/transitioning teams fall back to conference mean. Recency via game-weight
  decay or a weekly random walk `r[i,w] = r[i,w−1] + N(0, σ_drift)` (phase 2).
- hfa: one parameter per season with a prior centred on the historical estimate (~2.5–3 pts);
  zero on neutral sites.
- Garbage time: standard thresholds (Q2 > 38, Q3 > 28, Q4 > 22) on true game score for the
  efficiency likelihood.
- Output per posterior draw: rating, neutral-field spread and win probability for any pair
  (σ_game → P(win) = Φ(spread/σ_game)).

### C2. Resume ranking — replaces WLD θ, SB, Buchholz, H2H, SOR blend
- **Wins Above Benchmark (WAB)**, per posterior draw *s*:
  `WAB_i = Σ_games (W_ik − P_s(benchmark team beats opp_k at that venue))`,
  benchmark = the *b*-th best FBS team in draw *s* (b = 12 ≈ playoff bubble; configurable).
  Additive, venue-aware, in units of wins, and opponent strength comes from the Power posterior.
- Also report **SOR probability** (ESPN-style): P(benchmark team goes ≥ this record on this
  schedule) via Poisson-binomial — useful as the secondary sort / tooltip.
- Resume is W/L only (OT win = win). Margin stays in Power, not Resume.
- Rank per draw → publish median rank, 80% interval, P(top 4), P(top 12).
- SOS shown as a display column only: expected losses of the benchmark team vs. this schedule.

### C3. Diagnostics & "luck" (all in interpretable units)
- Expected wins = Σ P(team wins | its own rating); **Win luck = actual − expected wins**.
- Close-game record (≤ 8 pts), **Resume vs. Power gap** = resume rank − power rank (display only).

### C4. Validation (drives every tuning choice)
- Rolling-origin backtest 2014–2025: fit through week *w*, predict week *w+1*.
  Metrics: margin MAE/RMSE, Brier/log-loss on W/L, calibration curve, ATS vs. closing lines
  (CFBD `lines`), compared with SP+/FPI/Elo from CFBD.
- Use the backtest to set ρ, σ_pre, recency, garbage thresholds, benchmark b.
- PPC (margin distribution, home win %, blowout frequency) and LOO for model comparison.
- Sensitivity: `priorsense` power-scaling on all priors (or NUTS re-fits), not ADVI.
- Gates: fail the run on R-hat/ESS/divergence breaches for the published fit.

### C5. Benchmarks
- AP and CFP (aligned to the modelled week), Colley (pure record baseline), SP+/FPI/Elo.
  Report rank correlation *and* the individual disagreements. No synthesized sources; the one
  scraped source, CBS Sports 138, is archived to disk and tracked by targets (D8).

## D. Pipeline / infrastructure

- [x] **D1. Fit every season** with targets dynamic branching (`pattern = map(season)`), and
  **every week** for backtest + rank trajectories. Published weeks: full NUTS; backtest weeks can
  use a fast path (Pathfinder/Laplace) if NUTS is too slow — measure first.
- [x] **D2. Ingest cleanup:** team IDs (A1), line scores/OT (A2), `week_seq` (A8), FCS games (A11),
  team metadata (logo URLs, colors, conference by season from `cfbd_team_info`), betting lines.
  Store PPA with game score so garbage time uses the real score.
- [x] **D3. One `canonical_team_name`** (currently duplicated in 01 and 02); ideally unnecessary
  once IDs are used.
- [x] **D4. Poll fetching as proper targets** keyed by (season, week), with `tar_cue` for the
  current week only (A4).
- [x] **D5. Remove dead weight:** warm-start inits (A12), rstan fallback, VI "fast_prototype",
  unused `version` args, `transform_wld_xpd` alias, `.Renviron` sanitising duplicated in 3 places,
  `plan.md` from the Quarto render list, CBS scraper (old CBS 134 scraper; a new one is in D8).
- [x] **D6. Tests (testthat):** week resolution, OT detection, WAB/SOR math on a toy schedule,
  Colley on a known example, ingest schema.
- [x] **D7. Dashboard reads only targets outputs** (no sourcing R files or DuckDB fallbacks from
  inside the .qmd). Export one tidy data file per season/week for the front end.
- [x] **D8. CBS Sports 138 benchmark** (`R/01_ingest_cbs.R`). It ranks every FBS team but has
  no API, so it is scraped.
  - Each `sync` fetches the live page and archives it as `data/raw/cbs138/<updated date>.html`
    (a no-op when that date is already saved). The page shows only the current ranking.
  - Missed weeks: `cbs_backfill_wayback()` (Wayback captures) and `cbs_fetch_articles()` (the
    weekly article, top 25 only, used only for weeks with no full-page capture).
  - All archived files are parsed into the `cbs_rankings` table on every sync. The ranking count
    per season is part of the `sync` fingerprint, so a new ranking reruns `data_<s>` downstream.
  - `cbs_as_polls()` aligns a ranking like the AP poll: it follows the latest week whose games
    were 90% played by its update date (poll week w+1 follows snapshot w).
  - Shown on the Rankings, Team and Polls pages; `check_build.R` WARNs when the latest week has
    only a top 25 and FAILs when it has none.
  - Coverage so far: 2026 only (full page 09-21; articles 05-13, 08-12, 09-09, 09-14). The
    Wayback backfill found no usable pre-2026 captures.
  - Operational: CBS updates Sunday night or Monday, so a run earlier on Sunday still gets
    last week's ranking. Rerun after it updates, or that week is only available later via
    Wayback or the article.

## E. Dashboard redesign

Current problems: 19-column tables full of θ/δ/ω/α/SB jargon; every season's widgets are
pre-rendered into one page and toggled with ~300 lines of hand-written JS plus DataTables resize
hacks; hard-coded hex colors ignore `_brand.yml`; red/green-only deltas; the quadrant chart plots
two correlated model internals with dramatized labels; half the "Over & Underrated" page is
`eval: false`.

Proposal: Quarto dashboard + **Observable JS** (built into Quarto, still a static HTML file) with
`Inputs.select` for season/week, `Inputs.table` or a small custom table, Observable Plot for
charts, data loaded from the exported JSON/Parquet (D7). Removes the year-panel JS and DT hacks.

Pages:
1. **Rankings** — one table: rank, ▲▼ movement vs. last week, logo + team + conf, record,
   Resume (WAB, with rank-interval bar), Power (pts) + rank, SOS rank, AP/CFP rank. Row click →
   team page. Toggle Resume ⇄ Power sort.
2. **Team** — header card (ranks, record, rating trend sparkline), schedule with pre-game win
   probability, result, efficiency margin, per-game resume contribution (ΔWAB), remaining games
   with win probability, projected final-record distribution.
3. **Matchup** — pick two teams + venue → spread, win %, projected score, rating distributions.
4. **Trends** — weekly rank trajectories (bump chart) for top 25; biggest movers.
5. **Resume vs. Power** — scatter (WAB vs. power rating) with plain-language annotations; win luck.
6. **Polls** — model vs. AP/CFP/Colley/SP+: agreement stats + largest disagreements.
7. **Methodology** — plain-language explanation; link to the diagnostics report.

Design system: tokens from `_brand.yml` only (no hex literals in .qmd/.js); Atkinson Hyperlegible
Next for UI + tabular numerals (Lora is a reading face, not for dense tables); diverging
blue↔orange from the brand palette with ▲▼ glyphs for direction; team colors as accents only;
dark mode; mobile layout; fixed-width number columns.

Reports: rebuild `season_retro.qmd` around the new outputs (resume/power, luck, SOS, polls) and
`model_diagnostics.qmd` around gates, PPC, calibration and backtest results.

## F. Order followed

1. **Data correctness:** A1, A2, A7, A8, A9, A11, D2, D3, D6 (tests alongside).
2. **Power model** (C1) + backtest harness (C4) on a few seasons; iterate until calibrated.
3. **Resume** (C2, C3) on top of the Power posterior; retire WLD/XWLD/XPD/SB/Buchholz/H2H.
4. **All seasons/weeks** via branching (D1), benchmarks (C5, A4, A5), gates (A14), cleanup (D5, A12, A13).
5. **Dashboard + reports** (E, D7, A6, A15).
6. README rewrite.

## Implementation notes

- CFBD REST API replaces cfbfastR play-by-play: `games` (both classifications, line scores),
  `stats/game/advanced` (garbage-time-filtered PPA; CFBD's own filter replaces B11's thresholds),
  `lines`, `rankings`, `teams`. All keyed by CFBD team IDs / names from the same source.
- Postseason is one snapshot (`week_seq` = last regular week + 1); week 0 is a prior-only
  preseason snapshot (from 2015), so week-1 games are backtested too.
- Offense/defense split and projected scores are not implemented (spread + win probability only).

- Dashboard additions (after the first full build):
  - a conference filter (group or single conference) beside every team picker;
  - Resume vs. Power colored by Power 5/4, Group of 5/6 and Independents, with a conference table;
  - the Polls page sorts by Resume, SOR, Power, AP, Coaches, CFP or Colley;
  - a new Games page showing model spread vs. Vegas;
  - the Team schedule shows the expected result, model and Vegas spreads (betting convention),
    and the expected / projected record.
- `check_build.R` runs the post-build checks.

## Follow-ups

Done (second pass, after the first full build):

- [x] **Convergence retry.** A retry now also doubles the post-warmup draws
      (`retry_sampling = 2000`). The first full build had two misses, 2025 weeks 7 and 16,
      both from low ESS rather than divergences.
- [~] **Close-game misfit** (partly addressed). Replicated margins are now rounded to whole
      points, so the check compares like with like. That raised the replicated share within 7
      (2025: 0.228–0.265 → 0.247–0.283), but the observed share (0.30–0.36) is still outside the
      interval in 13 of 13 seasons. So there is a real misfit: real margins bunch closer to one
      score than a Student-t around the rating gap allows. Likely causes are end-game behaviour
      (running clock, kneeling, onside/prevent defence), key numbers, and regulation ties (4%).
      The win/loss probabilities are unaffected (backtest calibration within 4 points in every
      bin), so the Resume and the win probabilities are sound. Only the predicted margin
      distribution's shape is off.
- [x] **Prior re-tuning** (first pass, `analysis/tune_priors.R`, since replaced by the
      `prior_refits` and `ablation` targets and deleted; the CSVs named below are in git
      history only; see the paper review status below):
  - **A. Structural priors.** Each season's final snapshot was refit and the posterior means
    were pooled over 2014–2025. The priors are now centred on the pooled mean with sd = 2× the
    between-season sd (`analysis/structural_pooled.csv`). The old priors were far off for the
    FCS offset (−18 vs −27), the non-D1 offset (−35 vs −54), σ_conf (9 vs 4.4) and b_prev
    (0.6 vs 0.75), which is what priorsense flagged.
  - Caveat: these hyperpriors use every season, so early-week backtest numbers for 2014–2024
    are slightly optimistic. They are structural constants, not team ratings.
  - **B. Recency half-life.** Rolling backtest over 2022–2024, 196 fits
    (`analysis/halflife_backtest.csv`). No down-weighting beats half-lives of 10, 6 and 4 weeks
    on every measure. For FBS, week 5+: MAE 12.51 vs 12.56 / 12.65 / 12.81, and Brier
    0.196 vs 0.197 / 0.199 / 0.202. `half_life_weeks` stays NULL.
  - **New vs. old priors** on the same games:
    - weeks 1–3, FBS vs FCS/lower: MAE 14.47 → 13.56, Brier 0.121 → 0.109;
    - weeks 1–3, FBS vs FBS: MAE 13.68 → 13.44;
    - week 4+: unchanged (12.44).

TODO:

- [x] **Offense/defense ratings** (2026-10-06). Each team also has a total tendency t; off =
      (r + t)/2, def = (r − t)/2, so the home score is base + (μ + T)/2 and the away score
      base + (T − μ)/2 with T = t_h + t_a. The total (final score; overtime games excluded) is
      conditioned on the summed offensive PPA exactly as the margin is on the net PPA. t has its
      own conference/tier hierarchy and carry-over. Published as Off / Def (points scored /
      allowed vs. an average FBS team, Off − Def = Power) with ranks; projected scores and totals
      in the predictions; backtest compares the total with the closing over/under.
- [x] **Margin distribution shape** (2026-10-06). `margin_family` in `power.stan`: normal, t,
      two-normal scale mixture, and a t whose scale grows with |μ| (scale-t). The
      `margin_comparison` target fits t / mixture / scale-t to every complete season's final week
      (vague priors, no carry-over, no total model), and `choose_margin_family()` publishes the
      family with the best pooled elpd if it beats the t by > 2 pooled SE (`margin_families`
      target has the table and the "within 7" PPCs). The backtest now scores the model's own
      predictive distribution (quantiles of posterior predictive draws) instead of a t
      approximation.
- [ ] **CBS Sports 138 history.** Only 2026 is covered. Look for another archive of past
      weekly rankings (articles for 2022–2025), or keep it as a current-season-only column.
- [ ] **Host the dashboard publicly.** Investigate options: add to personal website, self-host,
      Quarto Pub / Posit Connect, GitHub Pages, etc. (Observable JS needs HTTP, not `file://`.)
- [x] **Weekly random-walk ratings** (2026-10-06). r and t both walk: a state per (team, week
      with a game), each step τ·sqrt(weeks elapsed)·ε, starting from the hierarchical week-1
      level. The published rating is the state at the snapshot week (latest state + drift since);
      predictions add drift for the weeks ahead. Half-life weighting is gone; the ablation has a
      "Static ratings" arm instead (and arms for the other margin families).

### Follow-ups after the model changes (2026-10-06)

- [x] **Re-pool the priors.** Superseded by `plans/todo.md` (2026-10-11: new pooling rule, τ/ν/total
      priors pooled, preseason terms). Original note: the margin priors in `model_config()` were pooled under the static
      t model; τ, the total model and the family priors are provisional. After the rebuild,
      `check_build.R` prints the pooled values from `prior_refits` when they drift; copy them in
      and rebuild.
- [ ] **Paper.** Not yet updated for the changes; it may not render until it is. To do:
  - Model section: the random walk (states, τ, drift in predictions), the total model and
    off/def notation, the margin families. Remove the static-strength limitation (§ limitations,
    item 2) and the half-life sentences (@sec-hierarchy, @sec-ablation).
  - Ablation table: variants are now Published / Margin only / No carry-over / No conference
    level / Static ratings / one per other margin family (`ablation_order` lists the old ones).
  - New results: `margin_families` (pooled LOO, "within 7" PPC by family), τ by season, total
    backtest vs. over/under, off/def examples.
  - Code that assumes the t: the championship plug-in example uses `pt(., nu)`; `params$nu` is
    NA for the normal/mixture families; the ν prior-sensitivity discussion; supplement `par`
    now has the family's own parameters (`model_vars()`); SBC adds τ (and the family's
    parameters).
  - Induced priors now include τ; PPC adds "Share within 3 pts", "Mean total", "SD of total".

## Paper review (paper/cfb-paper.qmd, 2026-09-26)

**Status (2026-09-28): every item below is implemented, the new priors are set, the full
rebuild has run and the paper renders (see "Remaining").** Every number in the paper is
computed at render time, so the prose follows the targets; conditional wording is used where
the direction of a result was not known in advance, and those sentences still need a read.

What changed, by item group:

- **Pipeline.** Spring 2021 FCS games are weeks after the 2020 postseason (`add_week_seq`,
  applied again in `read_season`). Each FBS/FCS independent has its own conference effect.
  The carry-over integrates out last season's rating uncertainty
  (`sqrt(tau^2 + (beta s_i)^2)`, `prev_sd`). The ν prior is data (`nu_prior`, `nu_lower`).
  `power.stan` has ablation switches (`student_t`, `conf_level`; `use_eff`/`carry_over` in R)
  and stores `log_lik_margin` (the margin's marginal density) for model comparison.
  `LAST_FULL = last_complete_season()` drives both the supplement targets and the paper.
- **Priors.** Set from full-season refits under vague priors, pooled without 2014 (no
  carry-over) and without 2020 for η (`pool_priors()`, `priors_from_pool()`), rounded into
  `model_config()`. The one-off refits used to set them are in `analysis/vague/<season>.rds`
  (gitignored); the `prior_refits` target recomputes them on every build, and
  `check_build.R` warns if they drift more than 0.5 prior sd from the configured priors
  (largest drift after the rebuild: 0.01 prior sd).
- **New targets** (`R/07_analysis.R`): `prior_refits`, `loso_backtest` (weeks 0–3 under
  leave-one-season-out priors), `ablation` (8 variants incl. half-lives, 2022–2025),
  `baselines` (ridge least squares; CFBD pregame Elo), `external_ratings` (SP+/FPI, 24 API
  calls), `final_refits` (b = 4/12/25, per-draw SOR, MCSE, margin-only LOO), `margin_leakage`,
  `sbc` (100 seasons), `schedule_graph`, `resume_check` (CFP 2014–2025, retrodictive accuracy),
  `data_hashes`, `prior_sensitivity_2020`; `prior_sensitivity` now also has ν > 3 and
  one-at-a-time scenarios; `paper` is a `tar_quarto` target.
- **Backtest.** Scope filters on the predicted week; CRPS and 50/80/95% coverage from a t
  predictive; line-implied Brier; sharpness by week; ECE, Spiegelhalter z and calibration
  splits; cluster-bootstrap CIs, Diebold–Mariano-style comparison, ATS binomial CI.
- **Paper.** All of P1–P4 edited as listed; hard-coded claims are inline computations; five
  `.example` blocks, the 2025 playoff field, schedule-graph figure, η and β by season, new
  sections on the benchmark, margin leakage, ablations/baselines, LOSO priors, preseason
  ratings, tuned choices, committee validation, SBC; abstract ~150 words; notation ℓ (tiers),
  ξ (t location), B⁽ˢ⁾ (benchmark), R_i⁽ˢ⁾ (rank). The legacy composite is not in the committee
  comparison (its code was retired).
- **Reproducibility.** `renv.lock` added; seeds and data hashes in the appendix.
  `analysis/tune_priors.R` and its CSV/log outputs were deleted (superseded by the targets
  above); `analysis/vague/` keeps the one-off vague-prior refits used to set the priors.
- **Tests.** `tests/testthat/test-model.R` and the new ingest tests pass.
- **Rebuild (2026-09-28).** No errored targets; all 225 snapshots pass the gates (15 needed
  the retry); backtest FBS MAE 12.87 vs Vegas 12.29. Fixes made while rendering the paper:
  - `fin_last` filtered on `max(week)` across all seasons; since spring games moved after
    the postseason, that left the final 2025 table empty. It now filters the season first.
  - `loo_compare()` in loo ≥ 2.10 returns the model names in a column, not the row names,
    so the margin-only LOO comparison was unlabelled. Handled in `run_final_refits()`.
  - `check_build.R` only calls `quit()` under `Rscript`, so `source()` no longer kills an
    interactive session.

**Remaining:**

1. Read the rendered paper. Sentences with conditional wording (e.g. the committee
   comparison, margin-leakage sign, SBC, margin-only LOO) and the examples (bubble team,
   disagreement team, championship game) depend on the new results.
2. Decide whether the paper should discuss the remaining `check_build.R` warnings: priorsense
   flags ν as a prior–data conflict in 10 of 13 seasons, and the "share within 7 pts" PPC is
   extreme in 13 of 13 (see "Margin distribution shape" in the TODO list).
3. Commit.

How the rebuild was run, for reference: (1) vague-prior refits of each complete season's final
input with `run_prior_refits()`, saved per season to `analysis/vague/`; (2)
`priors_from_pool(model_config(), pool_priors(refits))`, rounded into `model_config()`;
(3) `targets::tar_make()` with the sandbox off (the SP+/FPI step makes 24 API calls; the
whole rebuild takes several hours), then `Rscript check_build.R`.

A critical read of the merged paper. Section labels (`@sec-…`, `@tbl-…`) are given
because line numbers drift.

### P1. Errors and inaccuracies (fix first)

- [x] **2020 FCS spring games leak into early 2020 posteriors (pipeline bug).** CFBD's
      `spring_regular` / `spring_postseason` games, about 255 of them played Feb–May 2021,
      carry `week_seq` 1–12. So the 2020 "through week w" fits contain games played months
      later. This contaminates the 2020 backtest (FBS–FCS predictions and tier offsets) and
      contradicts `@sec-data` ("fitted like any other"). There is also one
      `spring_postseason` game at week 1. Fix: move spring games after the fall postseason, or
      treat them as a separate block, then refit 2020+.
- [x] **`@tbl-backtest` mixes denominators.** `N` is all games (18,792), but the `MAE`
      column is `mae_model_lined` (13,070 games with a line), while RMSE, Brier, log loss and
      accuracy use all 18,792. Report N with a line separately, or split into two tables.
- [x] **Scope label mismatch.** The row "FBS vs FBS, week 5+" is built with `week >= 4` in
      `R/04_validation.R:154`. Decide which is meant and make the label and filter agree
      (also `bt_late`).
- [x] **Calibration caption says "deciles"; the bins are fixed-width 0.1 bins.**
- [x] **Calibration is overstated.** "Within 3.4 points" hides that two bins are about 3 SE
      off: (0.1, 0.2] is 0.155 predicted vs 0.123 observed with n = 1,055, and (0.6, 0.7] is
      0.650 vs 0.684 with n = 2,354. Say so. Add ECE and a reliability test (e.g.
      Spiegelhalter z). Also split calibration by FBS–FBS / cross-tier and early / late weeks.
- [x] **Intro contribution 4 overclaims.** It says all posteriors "pass … posterior
      predictive checks and prior-sensitivity diagnostics". The within-7 PPC fails in every
      season, and priorsense flags ν, m₂, m₃ and τ₃. Reword.
- [x] **The concluded-week rule is misstated.** `@sec-data` says a week needs 90% of its
      games final plus a 6-hour buffer. `_targets.R` uses `min_share = 0` for past seasons,
      so the 90% rule applies only to the current season. State both.
- [x] **`@tbl-priors` posterior column is mislabelled.** It says "posterior means at each
      complete season's final week", but it comes from `analysis/structural_by_season.csv`:
      refits under the *old* priors, not the published fits.
- [x] **The justification of the priors is circular.** `@sec-priors` says "every
      between-season range lies well inside its prior", but the priors were centred on those
      same seasons.
      - `@sec-supp-justify` says the refits used the old priors "whose influence at that
        sample size is small". Yet plan.md records that priorsense flagged those old priors,
        and the old σ_conf prior (mean 9) could have pulled the refit estimates.
      - Fix: redo the refits under vague priors, or use leave-one-season-out priors (or
        earlier-seasons-only priors), and report both.
- [x] **"Barely move between seasons" is false for ω.** σ_conf ranges 1.7–9.1. The 2014
      value (9.08) is structural: with no carry-over (d_i = 0) conference effects absorb
      team strength. 2020 is also atypical (η = 1.77 with empty stadiums, ν = 27.8).
      Discuss both, and consider excluding 2014 from the pooled priors.
- [x] **The between-season sd is mostly estimation noise.** For b_prev, hfa and ν,
      `between_sd ≈ typical_post_sd` in `analysis/structural_pooled.csv`, so "prior sd = 2 ×
      between-season sd" measures noise, not true season-to-season variation. Say so; it
      makes the priors conservative.
- [x] **ν reasoning in `@sec-sens` contradicts itself.** It explains a *prior–data
      conflict* flag with a *flat likelihood* (which would be a weak-likelihood flag). Check
      what priorsense actually reports for ν and rewrite.
- [x] **Hard-coded prose claims should be computed inline** (they will drift):
  - "η and σ_m show a mild conflict in about a third of seasons";
  - "no parameter is flagged for a weak likelihood in more than one season";
  - "p_loo close to, and below, the number of teams";
  - "resume ranks are sharper than power ranks at the top";
  - "by mid-season the intervals have roughly halved";
  - "the resume tracks Colley closely from the first weeks";
  - "the gap to the line is roughly constant"; "error flat from about week 4";
  - "the schedule graph has been connected in every season" (compute the number of
    components per snapshot, including 2020).
- [x] **WAB Properties (ii) needs a qualifier.** "Changing a loss to a win raises WAB by
      exactly one" holds per draw with the draws fixed. The published WAB changes by a
      different amount, because the result also moves the posterior (the opponent and the
      benchmark).
- [x] **Calibration is not "direct evidence" that π_g is on the right scale.** π_g is a
      hypothetical benchmark-vs-opponent fixture, fitted in-sample, while the backtest scores
      real fixtures out of sample. Soften the wording.
- [x] **Stale reference to `paper/supplement.qmd`** in the `R/06_supplement.R` header. The
      file has already been removed, so only the header needs fixing.

### P2. Missing analyses (gaps a referee will raise)

- [x] **Ablations and baselines.** None exist yet. Backtest on identical games:
  - margin-only (no efficiency likelihood) — this settles the Information Proposition
    empirically;
  - Normal vs Student-t;
  - no carry-over (β = 0) and no hierarchy;
  - a least-squares Massey and an Elo baseline;
  - the published SP+, FPI and Elo ratings from CFBD (plan C5 promised these).
- [x] **LOO cannot compare models yet.** `log_lik` is the joint p(e, y) for games with
      efficiency and p(y) otherwise, so its elpd is not comparable with a margin-only model.
      Add a margin-only log-lik (the marginal p(y)) for comparisons.
- [x] **Uncertainty on the headline comparisons.**
  - Model vs line MAE: paired bootstrap or Diebold–Mariano test.
  - ATS 49.4%: binomial CI, and note the −110 break-even is 52.4%.
  - Brier and log loss: CIs.
- [x] **Show the Brier-rise claim instead of asserting it.** Plot the mean |p − 0.5| by week,
      or the line-implied Brier by week, next to the model's.
- [x] **Predictive distribution of the margin in the backtest.** Report 50/80/95% interval
      coverage and CRPS, not only MAE. The PPC shape misfit suggests the intervals are too
      wide near zero.
- [x] **No validation of the resume.** It has no out-of-sample target, so the paper needs
      some external evidence:
  - agreement with CFP committee selections, 2014–2025 (top 4 / top 12 and the at-large
    bubble), against Colley, AP and the old composite;
  - retrodictive accuracy (the share of games whose winner the ranking orders correctly).
- [x] **Sensitivity of the benchmark #12.**
  - Rerun with b = 4, 12 and 25.
  - Discuss that 2014–2023 had a 4-team playoff.
  - Discuss that the 12-team format has auto-bids (5 conference champions), so "12th-best
    team" ≠ "12th selected". Plan C2/C4 said to set b by backtest, which is impossible for
    a retrodictive measure; say how b is chosen instead.
- [x] **Quantify how margin leaks into the resume.** A blowout win lowers the opponent's
      posterior strength and so lowers the winner's WAB credit. Measure it: refit with one
      game's margin replaced by ±1 and record the ΔWAB for both teams.
- [x] **SOR is inconsistent with the per-draw design.** It uses the posterior-mean π̄_g
      rather than per-draw π_g^{(s)}, and "at least as well" counts ties. Justify, or
      compute SOR per draw and average.
- [x] **Monte Carlo error of the published quantities.** Report the MCSE of Pr(top 12),
      rank intervals and WAB from the `summary_draws` thinned draws. Thinning also discards
      precision; justify it.
- [x] **Simulation-based calibration / fake-data recovery.** This is missing, and Kahle et al.
      and Schad et al. recommend it. Simulate seasons from the prior on a real schedule,
      refit, and check the coverage of r_i, WAB and ranks (e.g. with the SBC package).
- [x] **Uncertainty in d_i is ignored.** d_i uses the posterior *mean* of last season's
      rating, which is errors-in-variables, so β absorbs attenuation. Propagate draws, or
      add a measurement-error term (known sd from last season).
- [x] **Teams that change tier.** FCS→FBS transitions (e.g. Sam Houston, JSU, Kennesaw
      State, Delaware, Missouri State) get d_i relative to their *old* tier mean but α from
      the new tier. Check what the code does and document it; test on the transition seasons.
- [x] **Independents are pooled per tier.** Notre Dame, Army, UConn and UMass share one α_c.
      Consider giving each independent its own conference effect or pooling to the tier.
- [x] **ω is shared across tiers** while τ_k is tier-specific. Justify, or estimate ω_k.
- [x] **One home-field advantage for all games.** Discuss FCS venues, travel and altitude,
      and the 2020 no-fan season. Show η by season as a worked example.
- [x] **Asymmetric garbage time.** PPA excludes garbage time but the margin does not;
      discuss what that does to λ and κ.
- [x] **State the missing-efficiency assumption.** Efficiency is missing mostly by tier (an
      observed covariate), so MAR is plausible. Say so.
- [x] **Discreteness of the margin.** Integer margins, the spike at 0 from overtime games
      and key numbers (3, 7) are modelled with a continuous t. Mention and cite discrete
      alternatives: score models (Karlis & Ntzoufras 2003; Baio & Blangiardo 2010) or a
      rounded/interval likelihood.
- [x] **Gaps in the perturbation study.**
  - ν is not perturbed (its prior is fixed in the Stan program), although it is the most
    often flagged parameter: expose the ν prior in `stan_data` and add ν > 3.
  - Add one-at-a-time perturbations; everything is currently moved jointly.
  - Only 2025 is tested; add an older season (e.g. 2020 or 2014).
- [x] **Preseason quality.** Evaluate the week-0 ratings against the preseason AP poll /
      SP+ and discuss portal-era changes (β by season, 2014–2025: is carry-over falling
      since 2021?).
- [x] **Leakage from tuning.** The half-life and prior tuning used the same backtest that
      is reported. Hold out seasons, or at least list every tuned choice in one place.

### P3. Examples and exposition

- [x] **No `.example` blocks anywhere**, unlike the reference chapter. Add worked examples:
  - **Win probability:** one 2025 game, from r_i, r_j, η and σ̄ to the spread, p_ij and the
    line.
  - **WAB for one team:** the schedule table with π_g per game, W_g − π_g and the running
    WAB, plus SOR via the Poisson-binomial recursion. Pick a bubble team.
  - **Resume vs. power disagreement:** a team with high power but low WAB, explained game
    by game.
  - **Colley on a 4-team toy schedule,** with C, f and the solution.
  - **Information Proposition:** plug in 2025's values to show the numbers.
- [x] **The 2025 CFP field vs the model.** Which teams the committee took, where WAB and
      power disagree, and why. This is the example readers will look for first.
- [x] **Support the fig-scatter claims.** They are asserted ("lost close games", "few
      chances"); overlay win luck (actual − expected wins) or the close-game record.
- [x] **A schedule-graph figure** for one season (conferences as clusters, cross-conference
      and cross-tier edges) to motivate sparsity and connectivity.
- [x] **A related-work citation for wins above bubble** (NCAA NET / team-sheet practice) is
      missing.
  - Also add Elo/538, SP+, FPI and Massey's composite, Knorr-Held (2000) dynamic
    paired comparisons, Glickman (1999), and Annis & Craig (2005) for CFB.
- [x] **Identification is a triviality labelled "Theorem".** Downgrade it to a Remark, or
      strengthen it (e.g. the posterior of the centred r̃ under a flat prior on the
      connected graph).
- [x] **Abstract is long** (about 250 words). Trim to about 150.

### P4. Notation and presentation

- [x] **ρ is used twice:** for rank ρ_i^{(s)} (`@sec-sor`) and for Spearman ρ in the
      perturbation tables. Rename one (e.g. rank R_i^{(s)}).
- [x] **Collisions among k, κ, k̂ and K:** tier k, loading κ, Pareto k̂, top-K. Also m_k
      (tier mean) vs the t location m. Rename the t location, and use ℓ or t for tiers.
- [x] **β (carry-over) vs b^{(s)} (benchmark)** reads confusingly. Consider B^{(s)}.
- [x] **PPC replicate counts differ:** 400 replicate seasons in `@fig-ppc` vs 100 in the
      supplement. Unify, or state why.
- [x] **The DAG** still has tight label placement near λ, σ_e and the curved edge. Also
      σ_e enters y_g (through σ̄) when e_g is missing, and no edge shows this.

### P5. Reproducibility

- [x] `analysis/structural_*.csv` and the half-life results are produced by a manual script
      outside targets. Make them targets, or record their inputs' hashes.
- [x] **CFBD data can change retroactively.** Snapshot or hash the DuckDB tables used for
      the paper; note the build date.
- [x] **Pin the environment:** add an `renv` lockfile (check whether one exists), plus the
      CmdStan version and seeds.
- [x] **Add the paper as a `tar_quarto` target** so it cannot go stale.
- [x] **The supplement targets use `CURRENT_SEASON - 1`**, while the paper uses `last_full`.
      They disagree in the window after the postseason but before the season rolls over.
      Derive both from the same rule.

## References

- Fang, Han, Luo & Xu (2026). *Recent advances in the Bradley–Terry model.* arXiv:2601.14727.
- Bradley & Terry (1952). *Rank analysis of incomplete block designs I.* Biometrika 39.
- Glickman & Stern (1998). *A state-space model for National Football League scores.* JASA 93.
- Colley (2002). *Colley's bias-free college football ranking method.*
- NCAA NET / Wins Above Bubble methodology (men's basketball selection).
- Gelman et al. (2020). *Bayesian workflow.* arXiv:2011.01808.
- Vehtari et al. (2021). *Rank-normalization, folding, and localization: an improved R̂.* Bayesian Analysis 16.
- Kallioinen et al. (2024). *Detecting and diagnosing prior and likelihood sensitivity with power-scaling.* Statistics and Computing.
