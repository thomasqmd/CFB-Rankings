# College Football Retrospective Bayesian Ranking Engine: Architecture & Technical Specification

## 1. Executive Summary & Core Philosophy

This project implements a comprehensive 4-pipeline Bayesian ranking system for NCAA College Football (CFB) spanning all FBS and connected FCS programs.

Unlike predictive power ratings (e.g., ESPN SP+, ESPN FPI, Sagarin, standard KenPom) that attempt to forecast future performance or hypothetical neutral-field point spreads, this engine operates strictly as a **retrospective evaluative ranking (Strength of Record / Quality of Past Achievement)**. It answers the fundamental question:

> *Given what occurred on the field across an entire season, what level of accomplishment did each team legitimately earn, when evaluated against the aggregate capability of its opponents?*

The framework resolves schedule asymmetry by back-propagating schedule credit using end-of-season opponent performance, fusing tournament tie-breaking mathematics (Sonneborn–Berger and Buchholz/Solkoff metrics) with Bayesian hierarchical regression to account for structural disparities between FBS and FCS competition.

---

## 2. Theoretical Architecture: The 2×2 Orthogonal Performance Matrix

Every football matchup produces information along two distinct axes:
- **Domain (Actual Realized vs. Expected Dominance):** Did the team win on the scoreboard, or did it dominate the down-to-down plays (net play Expected Points Added, EPA)?
- **Granularity (Discrete Resume vs. Continuous Margin):** Did the team secure a discrete victory/defeat, or did it build a continuous scoring/efficiency differential?

Mapping these axes produces our **4-Pipeline Performance Decomposition**:

| Granularity \ Domain | **Actual Realized (Scoreboard)** | **Expected Dominance (Net EPA / Play)** |
| :--- | :--- | :--- |
| **Discrete (Game Resumes)** | **Pipeline 1: WLD ($\theta$)**<br>Ordered logit on $\{L, OTL, OTW, W\}$ | **Pipeline 3: XWLD ($\omega$)**<br>Ordered logit on Post-Game Win Expectancy |
| **Continuous (Margins)** | **Pipeline 2: PD ($\delta$)**<br>Robust Student-$t$ on score differentials | **Pipeline 4: XPD ($\alpha$)**<br>Robust Student-$t$ on cumulative net play EPA |

Cross-pipeline comparisons directly isolate variance and game control:
- **$\theta_i - \omega_i$ (Outcome Luck Factor):** Measures whether a team won more games than its per-play performance dictated (e.g., turnover luck, one-score game variance).
- **$\delta_i - \alpha_i$ (Finishing / Margin Luck Factor):** Measures red-zone efficiency variance versus overall field dominance.

---

## 3. Mathematical Formulations

### 3.1 Pipeline 1: WLD (Discrete Outcome Resume Engine)
Models discrete game outcomes $Y_k \in \{1: \text{Regulation Loss}, 2: \text{OT Loss}, 3: \text{OT Win}, 4: \text{Regulation Win}\}$ via a cumulative ordered logit:

$$\begin{aligned}
Y_k &\sim \text{OrderedLogit}(\eta_k^{\text{wld}}, \mathbf{c}) \\
\eta_k^{\text{wld}} &= \theta_{\text{home}[k]} - \theta_{\text{away}[k]} + \gamma_{\text{home}} \cdot (1 - \text{neutral}_k)
\end{aligned}$$

- $\theta_i$: Latent actual resume strength of team $i$.
- $\mathbf{c} = (c_1, c_2, c_3)$ with $c_1 < c_2 < c_3$: Internal cutpoints separating outcome tiers.
- $\gamma_{\text{home}} \sim \mathcal{N}(0.2, 0.1^2)$: Structural home-field advantage log-odds.

### 3.2 Pipeline 2: PD (Scoreboard Point Differential Engine)
Models scoreboard point margin $\Delta \text{Pts}_k = \text{Score}_{\text{home}} - \text{Score}_{\text{away}}$ using a robust Student-$t$ likelihood to temper extreme blowouts:

$$\Delta \text{Pts}_k \sim \text{Student-}t\left(\nu_{\text{pd}}, \, \delta_{\text{home}[k]} - \delta_{\text{away}[k]} + \beta_{\text{home, pd}} \cdot (1 - \text{neutral}_k), \, \tau_{\text{pd}}^2\right)$$

- $\delta_i$: Latent actual point differential rating.
- $\nu_{\text{pd}} \sim \text{Gamma}(2, 0.1)$: Heavy-tailed degree of freedom ($\nu \approx 4\text{--}8$).
- $\beta_{\text{home, pd}} \sim \mathcal{N}(2.8, 1.0^2)$: Realized home advantage in points.

### 3.3 Pipeline 3: XWLD (Expected Discrete Outcome Engine)
Converts net play EPA into Post-Game Win Expectancy (PGWE) mapped to expected discrete outcome tiers $Y_k^{\text{exp}} \in \{1, 2, 3, 4\}$:

$$\begin{aligned}
Y_k^{\text{exp}} &\sim \text{OrderedLogit}(\eta_k^{\text{xwld}}, \mathbf{c}_{\text{xwld}}) \\
\eta_k^{\text{xwld}} &= \omega_{\text{home}[k]} - \omega_{\text{away}[k]} + \gamma_{\text{home, xwld}} \cdot (1 - \text{neutral}_k)
\end{aligned}$$

- $\omega_i$: Latent expected outcome strength (deserved wins).

### 3.4 Pipeline 4: XPD (Expected Point Differential Engine)
Computes game-level expected point differential ($\Delta \text{XP}_k$) directly from cumulative play-by-play Net EPA:

$$\Delta \text{XP}_k = \sum_{p \in \text{Plays}_k} \text{EPA}_{p, \text{home}} - \sum_{p \in \text{Plays}_k} \text{EPA}_{p, \text{away}}$$

$$\Delta \text{XP}_k \sim \text{Student-}t\left(\nu_{\text{xpd}}, \, \alpha_{\text{home}[k]} - \alpha_{\text{away}[k]} + \beta_{\text{home, xpd}} \cdot (1 - \text{neutral}_k), \, \tau_{\text{xpd}}^2\right)$$

- $\alpha_i$: Latent per-play efficiency rating (expected point differential).

---

## 4. Sparse Graph Regularization & Sub-Division Hierarchical Shrinkage

Matchups between FBS and FCS teams form a sparse, asymmetric bipartite subgraph (~110 games annually, ~98% hosted by FBS teams). Unregularized flat priors permit transitive inflation loops where elite FCS teams score higher than mid-tier FBS teams purely from internal conference connectivity.

Following recent theoretical advances on high-dimensional Bradley–Terry models (*Fang et al., 2026, arXiv:2601.14727*), we apply non-centered hierarchical Gaussian shrinkage:

$$\begin{aligned}
\theta_i &\sim \mathcal{N}\left(\mu_{\text{sub}[i]}, \, \sigma^2_{\text{sub}[i]}\right), \quad \text{sub}[i] \in \{\text{FBS}, \text{FCS}\} \\
\mu_{\text{FBS}} &= 0 \quad (\text{Identifiability Anchor}) \\
\mu_{\text{FCS}} &\sim \mathcal{N}(-1.5, 0.5^2) \quad [\text{or } \xi_{\text{FCS}} \sim \mathcal{N}(-21.0, 3.0^2) \text{ in XPD}] \\
\sigma_{\text{FBS}}, \sigma_{\text{FCS}} &\sim \text{Exponential}(1)
\end{aligned}$$

- **Elite FCS Programs (e.g., North Dakota State, South Dakota State):** Shrink toward the upper tail of the FCS prior distribution, giving appropriate credit to FBS teams that beat them.
- **Low-Tier FCS Programs:** Heavily shrunk toward the mean, yielding negligible resume credit in $\theta$ and automatically bounding runaway point margin inflations.

---

## 5. Sonneborn–Berger & Buchholz Tie-Break Derivation

Latent team abilities back-propagate into classical tournament equity metrics:

- **Sonneborn–Berger (SB) Score:**
$$\text{SB}_i = \sum_{k \in \text{Games}(i)} w_{k, i} \cdot \text{logit}^{-1}(\theta_{\text{opp}[k]})$$
where $w_{k, i} \in \{3: \text{W}, 2: \text{OTW}, 1: \text{OTL}, 0: \text{L}\}$.

- **Buchholz (Strength of Schedule) Score:**
$$\text{BH}_i = \sum_{k \in \text{Games}(i)} \text{logit}^{-1}(\theta_{\text{opp}[k]})$$

- **4-Pipeline Composite Index:**
$$\text{Index}_i = w_1 \Phi(Z_{\theta_i}) + w_2 \Phi(Z_{\delta_i}) + w_3 \Phi(Z_{\omega_i}) + w_4 \Phi(Z_{\alpha_i})$$
Default weights: $w_1 = 0.35$ (WLD Resume), $w_2 = 0.25$ (PD Margin), $w_3 = 0.15$ (XWLD Exp W/L), $w_4 = 0.25$ (XPD Net EPA).

---

## 6. Multi-Season Data Store & DuckDB Architecture

The data pipeline connects `cfbfastR` to a local analytical `DuckDB` database (`data/cfb.duckdb`).

### 6.1 Multi-Season Storage Schema
- **`schedules`**: Stores game metadata, conference affiliation, division indicators, scores, and overtime flags across multiple seasons (2014–present).
- **`pbp`**: Stores play-level Expected Points Added (EPA), Win Probability Added (WPA), period, clock, and down/distance for every game.
- **`sync_metadata`**: Tracks season-by-season sync timestamps, games count, and play counts, enabling granular incremental updates.

### 6.2 Multi-Season Ingestion
The ingestion engine (`R/01_ingest_duckdb.R`) supports syncing the full modern era (`seasons = 2013:current_season`), ensuring that:
1. Multi-season data is preserved in DuckDB across both the BCS finale (2013 schedules/scores) and the entire CFP play-by-play EPA era (2014–present).
2. Season-over-season trajectory (e.g. conference realignment in 2024 with Oregon/Washington in the Big Ten and Texas/Oklahoma in the SEC) is tracked.
3. Inits for warm-starting Stan MCMC chains are cleanly scoped by season (`data/inits/<season>_<engine>_inits.rds`).

### 6.3 API-Guarding, Smart Caching & Active Week Determination
To protect against rate-limiting, redundant external calls, and downstream target invalidation:
1. **Local Analytical Cache First**: [`get_latest_cfb_week()`](file:///home/thomas/CFB/R/01_ingest_duckdb.R#L160-L245) queries `data/cfb.duckdb` directly before making any network requests. If game records exist locally, it resolves the active week in milliseconds with zero network overhead.
2. **AP Top 25 Poll Release & Concluded Week Threshold**: College Football weekend slates finish Saturday night (with occasional Sunday/Monday games), and the AP Top 25 Poll is officially released on **Sunday afternoons at approximately 2:00 PM US Eastern Time (14:00 ET)**. Under [`resolve_completed_ap_week()`](file:///home/thomas/CFB/R/01_ingest_duckdb.R#L80-L159), a week is counted as the active evaluation week only once:
   - All scheduled games for that week are finished ($\ge 95\%$ to allow for rare cancellations).
   - Current time is at or after the AP poll release time (Sunday 14:00 ET for Thursday/Friday/Saturday slates, Monday 14:00 ET for Sunday slates, Tuesday 14:00 ET for Monday night games like Week 1 Labor Day).
   - If a week is currently underway (e.g. Thursday night, Friday, Saturday all day/evening, or Sunday morning before 14:00 ET), the pipeline steps back to the most recent fully concluded week (e.g. Week 2 when Week 3 is underway).
3. **Pipeline Dataset Scoping**: [`transform_all_pipelines()`](file:///home/thomas/CFB/R/02_transform.R#L280-L330) accepts `max_week = current_season_info$week`, ensuring models and external AP poll comparisons evaluate the exact same concluded game slate.
4. **Historical Season Freezing**: Completed past seasons (e.g. 2013–2025) are permanent records. [`pull_and_sync_duckdb()`](file:///home/thomas/CFB/R/01_ingest_duckdb.R#L320-L370) checks `sync_metadata`: if a historical season already contains game records (and PBP for 2014+), it is frozen and never re-fetched unless explicit `full_refresh = TRUE` is passed.
5. **External Poll Disk Caching**: [`fetch_external_polls()`](file:///home/thomas/CFB/R/06_benchmark.R#L73-L115) persists poll results to `data/external_polls/polls_<season>_<week>.rds`. Subsequent targets runs load directly from disk without consuming CFBD API quotas.
6. **Target Pipeline Stability**: By returning consistent objects from local stores, `targets` hashes remain identical across invocations, allowing `targets::tar_make()` to cleanly skip already-computed steps.

---

## 7. Pipeline Orchestration & Quality Control (`targets`)

```
[ cfbfastR Data Feeds ]
          │
          ▼
   [ data/cfb.duckdb ]  ◄── Incremental Multi-Season Sync (2013–Present)
          │
    targets Workflow Graph
    ├── 1. current_season_info (active season & week tracker)
    ├── 2. active_seasons (historical sync vector: 2013:current_season)
    ├── 3. duckdb_sync (high-throughput multi-season ingest)
    ├── 4. modeling_data (WLD, PD, XWLD, XPD transformations)
    ├── 5. fit_wld, fit_pd, fit_xwld, fit_xpd (CmdStan MCMC with warm-start)
    ├── 6. diagnostics_wld, pd, xwld, xpd (R-hat <= 1.01, ESS >= 400, Div = 0)
    ├── 7. prior_sensitivity_wld (FCS hyperparameter perturbation grid)
    ├── 8. sb_ratings & composite_rankings (Sonneborn-Berger & 4-way fusion)
    ├── 9. colley_rankings & benchmark_comparison (AP, Coaches, CFP, Colley)
    ├── 10. render_dashboard (docs/dashboard.qmd -> 100% rankings-focused interactive explorer)
    ├── 11. render_weekly_briefing (docs/reports/weekly_briefing.qmd -> week-scoped rankings briefing)
    ├── 12. render_season_retro (docs/reports/season_retro.qmd -> full season retrospective rankings audit)
    └── 13. render_model_diagnostics (docs/reports/model_diagnostics.qmd -> dedicated MCMC convergence & diagnostics audit)
```

### 7.1 Diagnostic Quality Gates
Pipeline execution fails fast if posterior draws fail any diagnostic threshold:
- $\widehat{R} \le 1.01$ across all latent ability parameters.
- $\text{ESS}_{\text{bulk}} \ge 400$ and $\text{ESS}_{\text{tail}} \ge 400$.
- Post-warmup divergent transitions $= 0$.
- Energy Bayesian Fraction of Missing Information (E-BFMI) $> 0.20$.

### 7.2 Prior Sensitivity Diagnostic
A 9-point grid perturbation sweep on the FCS prior $(\mu_{\text{FCS}} \in \{-1.0, -1.5, -2.0\}, \sigma_{\text{FCS}} \in \{0.3, 0.5, 0.8\})$ verifies that Top-25 FBS rankings remain invariant ($\rho \ge 0.985$, Jaccard $\ge 0.95$).

---

## 8. Presentation, Reporting, & Dashboard Architecture

The delivery layer is powered by Quarto and themed with `_brand.yml`. A strict separation of concerns is maintained: **all public/consumer viewing documents are dedicated 100% to rankings, accomplishments, and schedule evaluations**, while **all statistical model diagnostics, MCMC audits, and prior sensitivity sweeps are housed in a dedicated technical document**.

### 8.1 Viewable Interactive Dashboard (`docs/dashboard.qmd`)
- **Temporal Scope:** Multi-season single-year engine spanning 2013 to 2026 (strictly single-year at a time).
- **Format:** Quarto Dashboard (`format: dashboard`), compiling to a self-contained HTML document (`docs/dashboard.html`) viewable directly in any web browser without an active R server.
- **Global Year Selector (`# {.sidebar}`):**
  - Features a persistent, prominent Season Selector dropdown in the left sidebar (and synchronized page controls) allowing the user to select any season from 2026 down to 2013.
  - Quick Era buttons allow one-click jumping between structural CFB eras (2026 Active, 2024–2025 12-Team CFP, 2014–2023 4-Team CFP, 2013 BCS Championship).
  - Changing the selector immediately updates the entire dashboard — updating all KPI banners, side-by-side matrices, 2×2 quadrants, poll divergence audits, and full standings to the chosen single year.
- **Features (Dedicated Purely to Rankings & Single-Year Alignment):**
  - **Side-by-Side Unified Rankings Matrix:** Side-by-side comparison across Composite Engine (`rank_composite`, `composite_index`), the 4 Component Pipelines (WLD resume $\theta$, PD margin $\delta$, XWLD deserved W/L $\omega$, XPD net EPA $\alpha$), External Benchmarks (Colley Matrix, AP Top 25, AP $\Delta$), and Luck Factors. Includes an interactive multi-system parallel rank trajectory chart (bump plot) and 4-pipeline component value decomposition.
  - **2×2 Orthogonal Quadrants:** Active single-year Resume Quality ($\theta$) vs. EPA Play Efficiency ($\alpha$), with 4 quadrant zones (Dominant & Deserving, Unlucky Powerhouses, Resume Contenders, Vulnerable Seeds) and outcome luck factors.
  - **Poll Divergence Benchmark:** Active single-year consensus comparison against AP Top 25 and Colley Matrix, isolating most underrated and overrated programs.
  - **Single-Year Standings Archive:** Complete 134-team FBS standings table (Rank 1-134, records, win %, margins, PPG, Opp PPG) strictly one single year at a time for every season from 2013 to 2026.

### 8.2 Executive Weekly Briefing (`docs/reports/weekly_briefing.qmd`)
- **Temporal Scope:** In-season weekly tracking (Week $W$, Season $S$).
- **Role:** Pure rankings and playoff bubble audit for a specific in-season week. Features executive summary callouts, side-by-side unified performance matrix with `gt` spanners, orthogonal ability quadrant scatter, and AP poll divergence breakdown.

### 8.3 Season Retrospective Audit (`docs/reports/season_retro.qmd`)
- **Temporal Scope:** Full-season retrospective evaluation (Season $S$).
- **Role:** Pure retrospective accomplishment audit. Features full 4-pipeline performance matrix, outcome luck variance analysis, and schedule difficulty & opponent quality back-propagation (Sonneborn-Berger and Buchholz ratings).

### 8.4 Dedicated Model Diagnostics & Convergence Certification (`docs/reports/model_diagnostics.qmd`)
- **Temporal Scope:** Technical modeling and sampling audit.
- **Role:** The isolated technical specification document for statistical modeling and computation. Features individual convergence gate audits for all 4 pipelines (WLD, PD, XWLD, XPD), combined health certification matrix ($\widehat{R}$, bulk/tail ESS, divergences, E-BFMI), and the 9-point FCS prior hyperparameter sensitivity grid sweep.

## 9. Theoretical & Methodological References

- **Bradley–Terry Modern Theory & Sparse Graph Asymptotics:**
  - Fang, S., Han, R., Luo, Y., & Xu, Y. (2026). *Recent advances in the Bradley–Terry Model: theory, algorithms, and applications*. arXiv preprint arXiv:2601.14727 [stat.ME]. https://arxiv.org/abs/2601.14727
- **Paired Comparisons & Ordinal Regression:**
  - Bradley, R. A., & Terry, M. E. (1952). *Rank analysis of incomplete block designs: I. The method of paired comparisons*. Biometrika, 39(3/4), 324–345.
  - Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.). Wiley.
  - Cattelan, M. (2012). *Models for paired comparison data: A review with emphasis on dependent data*. Statistical Science, 27(3), 412–433.
- **College Football Ratings & Resume Evaluation:**
  - Connelly, B. (2017+). *SP+ Methodology and Efficiency Diagnostics*. ESPN Analytics.
  - Colley, W. N. (2002). *Colley's Bias Free College Football Ranking Method: The Colley Matrix Explained*. Princeton University.
- **Bayesian Computation & Workflow:**
  - Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Bürkner, P. C., Yanchenko, A. K., & Modrák, M. (2020). *Bayesian workflow*. arXiv preprint arXiv:2011.01808.
  - Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C. (2021). *Rank-normalization, folding, and localization: An improved $\widehat{R}$ for assessing convergence of MCMC*. Bayesian Analysis, 16(2), 667–718.