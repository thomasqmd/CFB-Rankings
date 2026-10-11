# Improving the Bayesian priors

Started 2026-10-10; implemented and screened 2026-10-11, followed by three full rebuilds (done).
Supersedes the "Re-pool the priors" follow-up in `plan.md` (2026-10-06).

Two kinds of prior, treated separately below:

- **Structural hyperpriors**: hfa, σ_m, λ, κ, ν, τ, and the total-model scales. They are set in
  `model_config()` (`R/02_model.R`) from vague-prior refits (`prior_refits` → `pool_priors()` →
  `priors_from_pool()`, `R/07_analysis.R`).
- **Preseason team prior**: `r0 = mu_conf + carry * prev_dev + c_ret * ret + b_tal * tal +
  b_prev2 * prev2_dev + s_team * z`, with `carry = b_prev + b_ret * ret` (`stan/power.stan`).

## Outcome

**Pooling rule.** `model_config()$pool` now uses recency weights with a half-life of 3 seasons
and the random-effects predictive sd for a new season. τ and ν are pooled, and so are the total
model and the preseason coefficients.

**Preseason terms.** Three terms were adopted: returning production (`use_ret`), the 247 talent
composite (`use_tal`) and the rating from two seasons back (`use_prev2`).

**Rejected.** A separate FCS carry-over (`b_prev_tier`) and a coaching-change spread
(`use_coach`). Their code switches stay in, off.

**The earlier τ prior was badly understated.** The old refits kept the published τ prior,
N⁺(0.6, 0.25). Under a wide N⁺(0.8, 0.8) the per-season τ_r estimates are 0.73–1.87, and the
pooled value is 1.25.

**Screening.** All fits used short chains. Priors were pooled without the predicted season,
except "Published priors", which was pooled in-sample. The test covered 2015–2025: weeks 0–3
(5,046 games) and weeks 4–12 (10,391 games). Bootstrap sds are in parentheses.

| Comparison | Weeks | ΔMAE | Δlog loss | ΔCRPS | 80% / 95% coverage |
| :--- | :--- | ---: | ---: | ---: | :--- |
| Predictive vs. 2× sd (equal weights) | 0–3 | −0.001 (0.016) | −0.0034 (0.0007) | −0.022 (0.011) | 80.8 / 95.5 vs. 82.2 / 96.7 |
| + returning production vs. base | 0–3 | −0.077 (0.024) | −0.0013 (0.0009) | −0.046 (0.015) | |
| + talent vs. base | 0–3 | −0.091 (0.023) | −0.0019 (0.0009) | −0.062 (0.015) | |
| + two seasons back vs. base | 0–3 | −0.031 (0.016) | −0.0006 (0.0006) | −0.030 (0.010) | |
| + coaching change vs. base | 0–3 | −0.009 (0.006) | 0.0000 (0.0003) | −0.004 (0.005) | rejected |
| + FCS carry-over vs. base | 0–3 | +0.004 (0.006) | +0.0002 (0.0002) | +0.002 (0.005) | rejected |
| All five terms vs. base | 0–3 | −0.212 (0.035) | −0.0038 (0.0013) | −0.133 (0.021) | week-0 MAE 14.07 → 13.67 |
| Adopted vs. published priors | 4–12 | −0.058 (0.015) | −0.0038 (0.0007) | −0.042 (0.010) | 79.9 / 95.2 vs. 79.2 / 95.2 |
| Adopted, old τ prior vs. published | 4–12 | −0.036 (0.014) | −0.0031 (0.0007) | −0.022 (0.009) | |

The pipeline now reproduces this comparison: target `prior_variants` → `prior_variant_scores`
→ `prior_variant_summary`. Its variants are "Before (2026-10-10 priors)", "Published", the
earlier pooling rule, and "Without" each adopted term. The paper reports it in `@sec-loso`.

**Side effect: the margin family changed from Student-t to scale-t.** `margin_comparison` fits each
family under `vague_config()`. With the wide τ prior, the scale-t now beats the t by Δelpd 229.5
(se 30.4) pooled over 12 seasons, so `choose_margin_family()` publishes it.

- The first rebuild ran scale-t with priors pooled under the t, so check_build §7 drifted
  (σ_m by 3.7 prior sd).
- The priors were re-pooled from that rebuild's scale-t refits: σ_m 6.61, γ 0.281, ν Gamma(56.3,
  1.83), τ_r 1.56. Then a second full rebuild ran.
- In the first rebuild's `prior_variants` (under scale-t), Published vs. "Before" on weeks 0–3:
  MAE −0.135 (0.039), log loss −0.0066 (0.0016), CRPS −0.102 (0.024), coverage 79.9 / 95.1
  vs. 83.1 / 96.4.
- Dropping each adopted term raises MAE: returning production +0.093 (0.024), talent +0.060
  (0.018), two seasons back +0.013 (0.016, marginal but in the right direction on every
  metric).
- `run_margin_comparison()` now uses the same longer chains as the refits (`long_chains()`):
  9 of 36 comparison fits had narrowly failed the gates (34 of 36 pass with them).

**Second rebuild (2026-10-11, 5h46m, no errored targets).** Scale-t was kept (Δelpd 229, se 30).
`check_build.R` found the following; each is fixed or explained. A third rebuild followed (below).

- §7 drift: b_prev 0.621 → 0.591 (0.89 prior sd) and b_tal 1.65 → 1.87 (0.59 sd).
  - The refits' carry-over d_i comes from the published snapshots. Those changed when τ_r went
    from 1.25 to 1.56, so b_prev followed: 0.663 → 0.621 → 0.591.
  - τ_r is now stable (1.56 → 1.57). The printed values were copied in.
- 2023 week 13 failed the gates (R-hat 1.59). The first attempt only just missed (R-hat 1.014 on
  τ_r). The retry reused seed 2026, and one chain's step-size adaptation collapsed (8e-5, every
  draw at max treedepth).
  - `fit_power()` now retries with `seed + 1`. With it, that snapshot passes: R-hat 1.002, ESS
    2520.
- CBS 138 FAIL for 2026 week 6. This is timing, not a bug: CBS had not yet published its week 6
  ranking (the live page still showed Oct 5). It clears on the first sync after CBS updates.
- The paper's Table 21 (`@tbl-tuned`) overflowed the page. It now uses fixed-width ragged
  columns.
- §11.7 still warned about the old Gamma(2, 0.1) ν prior. It is rewritten for the pooled Gamma,
  and the figure caption is now conditional.
- The perturbation refits (§11.11) never moved the newly informative ν and γ priors.
  - New `family` prior group: ν's Gamma is moved by its mean and sd and matched back to a Gamma.
  - The obsolete "ν > 3" scenario was dropped.
  - The paper's group labels now include the shape parameters and the preseason terms.

## Phase 0 — Evaluation harness

- [x] **0.1 Early-week variant runner.** `run_prior_variants()`, `score_variant_predictions()`
      and `compare_variants()` (`R/07_analysis.R`).
  - Refits are shared by variants with the same model switches.
  - `fixed_priors` keeps a variant's priors as configured.
  - `after_pool` overrides pooled priors.
- [x] **0.2 Record the baseline.** The "Before (2026-10-10 priors)" variant
      (`priors_2026_10_10()`).
- [x] **0.3 Acceptance rule.** Applied as planned: early-week log loss or CRPS must improve and
      weeks 4–12 must not get worse. The paired bootstrap is in `compare_variants()`. Its first
      version computed the log-loss and CRPS sds after `summarise()` had overwritten those
      columns; that was fixed before any decision.

## Phase 1 — Structural pooling

- [x] **1.1 τ without circularity.** Neither option (a) nor (b) as written. `vague_config()` now
      gives τ_r and τ_t wide priors centred away from 0, N⁺(0.8, 0.8) and N⁺(1, 0.8), and they are
      pooled like everything else.
  - The vague refits mix more slowly in the τ tails, so `run_prior_refits()` uses longer chains
    (1000/2000, retry 1500/4000). With the shorter chains, 6 of 12 failed the ESS gate.
  - Stage 3 above checks that the larger τ helps later weeks.
- [x] **1.2 Recency-aware pooling.** `pool$half_life` sets the decay. The weights are centred on
      the target season: the season left out under LOSO, otherwise the next season.
  - Half-life 3 was neutral for margins and improved totals: 2023+ total MAE 12.88 vs. 12.90 and
    bias +0.06 vs. +0.36.
- [x] **1.3 Pool ν.** The vague prior is Gamma(2, 0.05); the pooled mean and sd are moment-matched
      to a Gamma (now Gamma(13.3, 0.8), mean 16.7).
- [x] **1.4 Principled prior sd.** `sd_rule = "predictive"`:
      `sqrt(max(between² − post², 0) + between² / n_eff)`.
  - It beat 2× sd, 1× sd and a 0.5× posterior-sd floor on log loss and CRPS, and brought
    coverage to nominal.
- [x] **1.5 Re-pool and copy in.** Values were pooled from full-config refits under the adopted
      model and copied into `model_config()`.
  - New `pooled_priors` and `prior_loso_check` targets.
  - `check_build.R` §7 now covers ν and the preseason coefficients.
- [x] **1.6 Weakly identified non-D1 parameters.** No special handling needed: the predictive sd
      narrows the σ_team[3] prior from 9.35 to 2.83.

## Phase 2 — Preseason team prior

- [x] **2.1 Calibrate the existing spread.** Weeks 0–1 over-covered because the predictive sd at
      week 0 was ~13% too large (RMSE / predictive sd 0.89).
  - The predictive-sd pooling fixes it: σ_conf's prior sd went from 2.53 to 0.32 and b_prev's
    from 0.087 to 0.030.
  - Week-0 coverage in the screening is now 79.4% / 95.4% with the adopted terms.
- [x] **2.2 Returning production.** Ingest: `parse_returning()` and `sync_preseason()`, stored in
      the DuckDB `preseason` table and read by `read_season()$preseason`.
  - Term: `use_ret`. The level effect is c_ret ≈ 7.8 pts per unit share. The carry-over
    interaction b_ret ≈ 0.16 is small.
- [x] **2.3 Talent composite.** `parse_talent()` (2015+). Term: `use_tal`, b_tal ≈ 2.0 pts per
      FBS sd. Recruiting rankings were not added; talent already summarises them.
- [x] **2.4 Two-season carry-over.** `prepare_week_input(prior2 = ...)`, wired in `_targets.R`
      (`prior2`, `prior_list_call()`) and in `run_ablation(priors2 = ...)`.
  - Term: `use_prev2`, b_prev2 ≈ 0.11.
- [x] **2.5 Uneven preseason spread.** A coaching-change switch was built (`parse_coaches()`,
      `coach_changes()`, `use_coach`) and rejected (δ ≈ 0.06 ± 0.48).
  - Portal turnover was not built. Returning production already measures departures and talent
    measures the roster, and portal data only exist from 2021.
- [x] **2.6 Tier-specific carry-over.** Built (`b_prev_tier`) and rejected: b_prev_low ≈ b_prev.
- [x] **2.7 Re-pool after the model changes.** Done in 1.5. `model_vars()`, `par_names`,
      `extract_draws()`, `prior_fields`, `prior_groups`, `draw_prior_params()` (induced priors
      and SBC) and snapshot params all know the new terms.

## Phase 3 — Write-up

- [x] **3.1 Paper.** Updated:
  - abstract and introduction;
  - Data: a new "Preseason information" paragraph;
  - `@eq-hier`;
  - `@sec-priors` with the new `@eq-pool`, and `@tbl-priors-other`, which now lists every pooled
    parameter including the preseason terms;
  - `@sec-loso`, rewritten around `@tbl-prior-variants`;
  - `@tbl-tuned`, the discussion, and the appendix justification, whose LOSO check now uses
    `prior_loso_check`.
- [x] **3.2 Induced priors and SBC.** Rerun and checked. SBC 80% coverage is 72–85% across
      parameters, including β_q, γ_q, γ_a and β_2. The induced-prior text is rewritten for the
      pooled ν prior.
- [x] **3.3 Method card on the dashboard.** "How it works" now describes the preseason inputs.

## After the rebuild

- [x] Run `Rscript check_build.R` after the second rebuild. Findings and fixes are above.
- [x] Read the rendered paper: `@sec-priors`, `@sec-sens`, `@sec-loso` and the supplement. Fixes
      are above.
- [x] Third rebuild (2026-10-11, 4h30m, no errored targets). `check_build.R` results:
  - 227 of 227 snapshots pass the gates; 10 needed the retry.
  - §7 PASS: the largest drift is 0.15 prior sd (σ_e), so the priors have converged.
  - Margin comparison: 36 of 36 fits pass. Scale-t is kept.
  - Backtest: MAE 12.79 (Vegas 12.29), 80% coverage 78.6%.
  - The only FAIL is CBS 138 for 2026 week 6. It is still unpublished (see above).
  - Published vs. "Before" on weeks 0–3: MAE −0.156 (0.041), log loss −0.0071 (0.0016), CRPS
    −0.116 (0.025), coverage 80 / 95 vs. 83 / 96.
- [x] Paper layout: the new `@eq-hier` is split over two lines, and Tables 6 and 20 wrap their
      text columns. The SBC figure now labels the preseason coefficients and was made taller.
  - The remaining wide tables and long Stan lines (the blowouts, baseline, sampler and
    perturbation tables) were already wide in the committed PDF.
