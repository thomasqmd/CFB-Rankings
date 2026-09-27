# CFB Bayesian Rankings — Overhaul Plan

Status: review completed 2026-09-26 against commit `c374378`; overhaul implemented the same day.
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
  scrape is brittle and untracked by targets).
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
  Report rank correlation *and* the individual disagreements. No scraped or synthesized sources.

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
  `plan.md` from the Quarto render list, CBS scraper.
- [x] **D6. Tests (testthat):** week resolution, OT detection, WAB/SOR math on a toy schedule,
  Colley on a known example, ingest schema.
- [x] **D7. Dashboard reads only targets outputs** (no sourcing R files or DuckDB fallbacks from
  inside the .qmd). Export one tidy data file per season/week for the front end.

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
- [x] **Prior re-tuning** (`analysis/tune_priors.R`):
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

- [ ] **Offense/defense ratings** (projected scores and totals). This is needed for a predicted
      score from the model itself; until then the dashboard shows the model spread and win
      probability.
  - Sketch: points_home ~ f(off_home − def_away + hfa/2), and likewise for away.
  - Use the per-play efficiency split (offense PPA and defense PPA) as the second signal, as
    the margin model does now.
  - Validate the total against the Vegas over/under (already in the `lines` table).
- [ ] **Margin distribution shape.** Try a likelihood that bunches margins closer to one
      score than a Student-t does: for example a two-component mixture, or a margin-dependent
      scale. Compare them with LOO and the "within 7" check. This would sharpen spread and
      cover probabilities; it does not affect win probabilities much.
- [ ] **Weekly random-walk ratings** instead of ratings that are static within a season
      (C1 phase 2): r_t = r_{t−1} + τ·ε. This replaces the half-life weighting with an
      estimated τ.

## References

- Fang, Han, Luo & Xu (2026). *Recent advances in the Bradley–Terry model.* arXiv:2601.14727.
- Bradley & Terry (1952). *Rank analysis of incomplete block designs I.* Biometrika 39.
- Glickman & Stern (1998). *A state-space model for National Football League scores.* JASA 93.
- Colley (2002). *Colley's bias-free college football ranking method.*
- NCAA NET / Wins Above Bubble methodology (men's basketball selection).
- Gelman et al. (2020). *Bayesian workflow.* arXiv:2011.01808.
- Vehtari et al. (2021). *Rank-normalization, folding, and localization: an improved R̂.* Bayesian Analysis 16.
- Kallioinen et al. (2024). *Detecting and diagnosing prior and likelihood sensitivity with power-scaling.* Statistics and Computing.
