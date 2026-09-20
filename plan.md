CFB Project Poll/Rankings

• All FBS/FCS teams

• Rank Teams(past performance as the results of most recent aggregate result - rank week i matchup by teams record at end of season)

• Sonneborn-Berger or Buchholz (Solkoff) like approach - weight(ratio/multiple) based off opponent performance

◦ Performance

▸ W(3)/L(0)/OT(2-1)

▸ XPD(expected point differential - expected goal(XG) but for football)

• Compare w other rankings (AP, kem pom for CFB?, CBS)

• Historical data then current



Evaluate this for potential ranking(not power rating - predict future performance) of teams past performances

An evaluation of your proposed retro-evaluative ranking system:

System Architecture & Methodology

    Retroactive Opponent Valuation: Using end-of-season records to back-calculate strength of schedule resolves early-season variance and small-sample bias. This mimics the PageRank/eigenvector centrality approach over directed bipartite-like multi-graphs.

    Tie-Break Scoring Adaptations:

        Buchholz/Solkoff: Direct summation of opponent metric mass. Penalizes playing non-FBS/lower FCS opponents with severely depressed baselines.

        Sonneborn-Berger: Weighs game outcome directly by opponent quality:

Code snippet

S_i = \sum_{j \in \text{Opp}(i)} w_{ij} \cdot R_j

where $w_{ij} \in \{0, 1, 2, 3\}$ and $R_j$ is opponent end-of-season rating. This rewards beating elite teams while heavily punishing bad losses.

    Metric Fusion (Outcome vs. Efficiency):

        Awarding points purely on binary outcomes (3/2/1/0) preserves resume integrity.

        Integrating an expected point differential (XPD, derived from EPA/play or post-game win expectancy like Bill Connelly's SP+ equivalents) prevents fluke results (e.g., turnover variance, kick-return luck) from distorting the true signal of past dominance.

Key Challenges & Adjustments

    Graph Disconnection & FBS/FCS Silos: FCS vs. FBS interleague connectivity is sparse. Without a damping factor or an anchored Bayesian prior on tier differentials, an undefeated top-tier FCS team can out-inflate its cluster score through internal transitive cycles.

    Collinearity in XPD and Record: Blending pure wins (W=3) and efficiency (XPD) requires scaling to prevent double-counting. Standardize both metrics before linear combination:

Code snippet

M_{ij} = \alpha \cdot \tilde{W}_{ij} + (1 - \alpha) \cdot \widetilde{\text{XPD}}_{ij}

Comparison Targets for CFB

    Direct Resume Anchors (Past Performance): SOR (Strength of Record), Resume SP+, Colley Matrix (purely non-predictive, mathematical record adjusting).

    Predictive / Efficiency Baselines (To Benchmark Divergence): FEI (drive efficiency), SP+ (Bill Connelly, the standard KenPom equivalent for CFB), FPI (ESPN).

    Media Anchors: AP Poll, Coaches Poll, CFP Committee Rankings.

Implementation Strategy (R Pipeline)

    cfbfastR exposes complete PBP for both FBS and FCS, allowing direct derivation of EPA/play into XPD.

    Compute seasonal ratings via sparse matrix eigenvector decomposition rather than brute iteration.

Four pipelines - 1 WLD, 2 PD, 3 XWLD, 4 XPD

Focus on FBS but must account for occasional FCS opponents, bayesian hierarchical approach

A Bayesian framework cleanly separates the four evaluative objectives along the 2×2 Orthogonal Performance Matrix (Actual vs. Expected × Discrete vs. Continuous) while resolving the FCS–FBS connectivity deficit:

    Pipeline 1 (WLD): Actual discrete strength of record via an ordinal Bradley–Terry or cumulative logit model.

    Pipeline 2 (PD): Actual point differential via a robust Student-t model on scoreboard point margins (downweights extreme blowouts).

    Pipeline 3 (XWLD): Expected discrete outcomes via an ordered logit model on Post-Game Win Expectancy (PGWE) derived from play-level net EPA.

    Pipeline 4 (XPD): Pure latent performance efficiency via a heavy-tailed Student-t model on cumulative EPA differentials.

All pipelines handle FCS opponents through hierarchical shrinkage rather than arbitrary downweighting factors.

1. Pipeline 1: WLD (Bayesian Win-Loss-OT Actual Resume)

To model discrete game outcomes (W=3, OT Win=2, OT Loss=1, L=0), cast each game outcome Yk​∈{1,2,3,4} as an ordered categorical variable using an ordered logit formulation:

Y_k \sim \text{OrderedLogit}(\eta_k, \mathbf{c})

\eta_k = \theta_{\text{home}[k]} - \theta_{\text{away}[k]} + \gamma_{\text{home}} \cdot (1 - \text{neutral}_k)

    θi​: Latent retro-performance / actual resume strength of team i.
    γhome​: Structural home-field advantage log-odds.
    c=(c1​,c2​,c3​): Cutpoints separating regulation loss, OT loss, OT win, and regulation win.

To extract end-of-season Sonneborn–Berger characteristics, compute team i's rating as the expected points secured against its specific schedule under posterior team qualities:

\text{SB}_i = \sum_{j \in \text{Opp}(i)} w_{ij} \cdot \text{logit}^{-1}(\theta_j)

2. Pipeline 2: PD (Actual Scoreboard Point Differential)

Model actual score margin (ΔPts_k = Score_home[k] - Score_away[k]) using a Student-t likelihood to prevent 70-point blowouts from exerting unbounded leverage:

\Delta \text{Pts}_k \sim \text{Student-}t\left(\nu_{\text{pd}}, \, \delta_{\text{home}[k]} - \delta_{\text{away}[k]} + \beta_{\text{home, pd}} \cdot (1 - \text{neutral}_k), \, \tau_{\text{pd}}^2\right)

    δi​: Latent actual point differential rating of team i.
    νpd​: Degrees of freedom (ν≈4–8) for heavy-tailed robustness.
    βhome, pd​: Home-field advantage in actual points (~2.8 pts).

3. Pipeline 3: XWLD (Expected Discrete Outcomes / Post-Game Win Expectancy)

Convert play-by-play net EPA into Post-Game Win Expectancy (PGWE) and map into 4 discrete expected tiers (Y_exp ∈ {1, 2, 3, 4}):

Y_k^{\text{exp}} \sim \text{OrderedLogit}(\eta_k^{\text{xwld}}, \mathbf{c}_{\text{xwld}})

\eta_k^{\text{xwld}} = \omega_{\text{home}[k]} - \omega_{\text{away}[k]} + \gamma_{\text{home, xwld}} \cdot (1 - \text{neutral}_k)

    ωi​: Latent expected outcome strength of team i.
    θi - ωi: Resume Luck / Game Control variance.

4. Pipeline 4: XPD (Expected Point Differential Efficiency)

Compute game-level expected point differential (ΔXPk​) directly from cumulative play-level EPA using cfbfastR::load_cfb_pbp():

\Delta \text{XP}_k = \sum_{p \in \text{Plays}_k} \text{EPA}_{p, \text{home}} - \sum_{p \in \text{Plays}_k} \text{EPA}_{p, \text{away}}

Model ΔXPk​ as a continuous response using a Student-t likelihood:

\Delta \text{XP}_k \sim \text{Student-}t\left(\nu_{\text{xpd}}, \, \alpha_{\text{home}[k]} - \alpha_{\text{away}[k]} + \beta_{\text{home, xpd}} \cdot (1 - \text{neutral}_k), \, \tau_{\text{xpd}}^2\right)

    αi​: Latent net efficiency (XPD rating) of team i.
    νxpd​: Degrees of freedom (ν≈4–7) for heavy-tailed robustness.
    δi - αi: Finishing / Red-Zone margin luck.

3. Hierarchical Handling of FCS Opponents

Because FCS–FBS matchups are sparse and one-sided, treating FCS teams with flat uninformative priors produces high posterior variance or graph disconnectedness.

Group FCS teams into an overarching subdivision hierarchy:
Code snippet

\theta_i \sim \mathcal{N}\left(\mu_{\text{sub}[i]}, \, \sigma^2_{\text{sub}[i]}\right), \quad \text{sub}[i] \in \{\text{FBS}, \text{FCS}\}

Code snippet

\mu_{\text{FBS}} = 0 \quad (\text{identifiability constraint})

Code snippet

\mu_{\text{FCS}} \sim \mathcal{N}(-1.5, 0.5^2) \quad \text{[or } \mu_{\text{FCS, XPD}} \sim \mathcal{N}(-21, 3^2)\text{]}

    An FBS team that plays an elite FCS program (e.g., North Dakota State, Montana State) is judged against an FCS team shrunk toward the upper tail of N(μFCS​,σFCS2​).

    Beating a bottom-tier FCS team yields negligible resume credit in θ and caps XPD gains automatically.

R Implementation Outline (cfbfastR + brms)
R

library(cfbfastR)
library(tidyverse)
library(brms)
library(future)
library(furrr)

plan(multisession)

pbp <- load_cfb_pbp(2025)

# Pipeline 2: Game-level XPD aggregation
game_xpd <- pbp |>
  filter(!is.na(epa)) |>
  group_by(game_id, home_team, away_team, home_division, away_division) |>
  summarize(
    home_epa_diff = sum(epa[posteam == home_team], na.rm = TRUE) - 
                    sum(epa[posteam == away_team], na.rm = TRUE),
    .groups = "drop"
  )

# Stan formulation via brms for latent team parameters
fit_xpd <- brm(
  home_epa_diff ~ 0 + home_team - away_team + (1 | home_division) + (1 | away_division),
  data = game_xpd,
  family = student(),
  prior = c(
    prior(normal(0, 7), class = "b"),
    prior(gamma(2, 0.1), class = "nu")
  ),
  chains = 4,
  cores = 4
)

How up to date is CFB fast? And how many years back? For bayes will also need prior sensitivity analysis and convergence checks. Will also maybe want to use targets and maybe something in addition or instead of to be able to update with new data

cfbfastR Coverage & Latency

    Historical Reach:

        load_cfb_pbp() (the default fast loader with native EPA/WPA) covers FBS back to 2014.   
        SportsDataverse

        load_espn_cfb_pbp() reaches back to 2004.   
        SportsDataverse

        load_ncaa_mfb_pbp() reaches back to 2013 and is particularly useful for lower divisions and standalone FCS games.   
        SportsDataverse

        High-level game outcomes, schedules, and scores via load_cfb_schedules() / CFBD API endpoints reach back to 2002+ (with basic box/records back to 1869 via the API).   
        SportsDataverse

    Update Cadence:

        During active seasons, static GitHub data releases update weekly (overnight Sunday/Monday) following games.

        For intraday or early-Sunday updates, live endpoints (espn_cfb_*() or cfbd_*() via a free CFBD key) fetch data on demand.

MCMC Convergence & Prior Sensitivity Pipeline

For automated rating systems, formalize diagnostic assertions inside the pipeline to flag ill-conditioned fits before ratings propagate downstream.

    Convergence Diagnostics:

        R≤1.01 across all latent team abilities θi​ and cutpoints c or variance components σ.

        Bulk and Tail Effective Sample Sizes (ESSbulk​,ESStail​>400).

        Zero divergent transitions post-warmup (strict requirement for hierarchical shrinkage with sparse connectivity).

    Prior Sensitivity Analysis:

        Priors under inspection: σFBS​, σFCS​, and the gap prior μFCS​∼N(μ0​,τ02​).

        Evaluation metric: Prior-data conflict and shrinkage ratio:

Code snippet

S_{\text{shrink}} = 1 - \frac{\text{Var}(\theta_i \mid y)}{\text{Var}(\theta_i)}

    Measure rank correlation sensitivity (Spearman ρ and top-25 Jaccard similarity) under perturbed priors: μ0​∈{−1.0,−1.5,−2.0} and τ0​∈{0.25,0.5,1.0}.

Orchestration Architecture: targets + DuckDB

Re-fitting full seasonal MCMC weekly from scratch is computationally wasteful. Combine targets with dynamic branching, a local analytical store (DuckDB), and bridge sampling or Laplace/SVI warm starts.

    Storage Layer (DuckDB via duckdb / arrow):

        Store parsed PBP and derived game-level aggregations in a local games.duckdb file.

        Append only completed games weekly; avoid re-parsing multi-gigabyte historical PBP files.

    Pipeline Graph (targets):

        Target 1 (raw_pbp): Track max game_id or season calendar week to trigger invalidation.

        Target 2 (game_summaries): Transform play-by-play into WLD and XPD summary tables (cue = tar_cue(mode = "thorough")).

        Target 3 (model_fit): Pass previous week's posterior parameter medians as the initial values (init = ...) to Stan/brms to cut warmup iterations from 1,000 down to 250–300.

        Target 4 (diagnostics_check): Explicit target executing posterior::summarise_draws() and rstan::check_divergences(), throwing an explicit stop/warning if R>1.01.

        Target 5 (rankings): Generate expected Sonneborn-Berger and latent ability tables comparing against external benchmarks.

R

library(targets)

tar_option_set(
  packages = c("cfbfastR", "tidyverse", "brms", "posterior", "duckdb"),
  format = "qs"
)

list(
  tar_target(
    name = current_week,
    command = get_latest_cfb_week()
  ),
  tar_target(
    name = game_data,
    command = pull_and_sync_duckdb(current_week)
  ),
  tar_target(
    name = fit_wld,
    command = fit_bayesian_wld(game_data),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    name = diagnostics_wld,
    command = evaluate_mcmc_health(fit_wld)
  ),
  tar_target(
    name = rankings_wld,
    command = extract_sb_rankings(fit_wld, diagnostics_wld)
  )
)

For rapid weekly iteration without running heavy MCMC chains during initial prototyping, variational inference via brms(..., algorithm = "meanfield") or cmdstanr pathfinder can serve as a fast sanity check before triggering full NUTS sampling.

Write a large readme or plan.md that compiles all the necessary(basically totality of information) information to give to a similar agent in the future . Include pipeline, file structure, packages, data(variables) used, etc. will also want dashboards & reports, ask for dashboard and report and presentation theme at end of plan

where wk,i​ is the outcome weight and M(θ) maps latent posterior opponent strength into game equity.
Code snippet

\\text{BH}_i = \\sum_{k \\in \\text{Games}(i)} \\mathcal{M}(\\theta_{\\text{opp}[k]})

2.2 Pipeline 1: WLD (Discrete Outcome Resume Engine)

Pipeline 1 models win-loss-overtime discrete outcomes without score margin inflations.

    Outcome Space:

        Regulation Win (W): 3 points (Yk​=4)

        Overtime Win (OTW​): 2 points (Yk​=3)

        Overtime Loss (OTL​): 1 point (Yk​=2)

        Regulation Loss (L): 0 points (Yk​=1)

    Likelihood: Cumulative Ordered Logit

Code snippet

Y_k \\sim \\text{OrderedLogit}(\\eta_k, \\mathbf{c})

Code snippet

\\eta_k = \\theta_{\\text{home}[k]} - \\theta_{\\text{away}[k]} + \\gamma_{\\text{home}}

where c=(c1​,c2​,c3​) are internal cutpoints strictly constrained such that c1​<c2​<c3​, and γhome​ is home-field advantage.

    Priors & Sub-Division Hierarchical Shrinkage:
    To prevent FCS transitivities from corrupting FBS ratings due to sparse cross-tier scheduling:

Code snippet

\\theta_i \\sim \\mathcal{N}\\left(\\mu_{\\text{sub}[i]}, \\, \\sigma^2_{\\text{sub}[i]}\\right), \\quad \\text{sub}[i] \\in \\{\\text{FBS}, \\text{FCS}\\}

Code snippet

\\mu_{\\text{FBS}} = 0 \\quad (\\text{identification anchor})

Code snippet

\\mu_{\\text{FCS}} \\sim \\mathcal{N}(-1.5, 0.5^2)

Code snippet

\\sigma_{\\text{FBS}}, \\sigma_{\\text{FCS}} \\sim \\text{Exponential}(1)

Code snippet

\\gamma_{\\text{home}} \\sim \\mathcal{N}(0.2, 0.1^2)

2.3 Pipeline 2: XPD (Expected Point Differential Engine)

Pipeline 2 isolates structural per-play dominance by computing game-level Expected Point Differential (XPD) via play-by-play Expected Points Added (EPA).

    Play-Level Aggregation:

Code snippet

\\text{XPD}_k = \\sum_{p \\in \\text{Plays}(k)} \\text{EPA}_{p, \\text{home}} - \\sum_{p \\in \\text{Plays}(k)} \\text{EPA}_{p, \\text{away}}

    Likelihood: Student-t robust regression to temper non-representative garbage-time plays and 70-point blowouts:

Code snippet

\\text{XPD}_k \\sim \\text{Student-}t\\left(\\nu, \\, \\alpha_{\\text{home}[k]} - \\alpha_{\\text{away}[k]} + \\beta_{\\text{home}}, \\, \\tau^2\\right)

where:

    αi​ is latent efficiency for team i.

    ν∼Gamma(2,0.1) represents degrees of freedom for outlier downweighting (ν∈[4,7]).

    βhome​∼N(2.5,1.02) represents expected point boost for home field.

    Sub-division hierarchy:

Code snippet

\\alpha_i \\sim \\mathcal{N}\\left(\\xi_{\\text{sub}[i]}, \\, \\omega^2_{\\text{sub}[i]}\\right)

Code snippet

\\xi_{\\text{FBS}} = 0, \\quad \\xi_{\\text{FCS}} \\sim \\mathcal{N}(-21.0, 3.0^2)

2.4 Hybrid Sonneborn-Berger Integration

The dual pipelines can be reported independently or combined into a composite resume score:
Code snippet

\\text{Index}_i = \\lambda \\cdot \\Phi\\left(\\frac{\\theta_i - \\bar{\\theta}}{\\text{SD}(\\theta)}\\right) + (1 - \\lambda) \\cdot \\Phi\\left(\\frac{\\alpha_i - \\bar{\\alpha}}{\\text{SD}(\\alpha)}\\right)

where λ=0.65 prioritizes the primary objective of winning games while Φ(⋅) represents the standard normal CDF standardizing scales to [0,1].
3. Data Sources & Variable Definitions
3.1 Raw Ingestion (cfbfastR)

    Historical EPA/WPA Play-by-Play: load_cfb_pbp(season) (covers FBS 2014–present).

    Historical lower-tier / FCS matchups: load_ncaa_mfb_pbp(season) (covers 2013–present).

    Schedules, venues, post-season flags: load_cfb_schedules(season).

    Benchmark Polls: cfbd_rankings(season, week) (AP Top 25, Coaches Poll, CFP Committee Rankings).

3.2 Ingested & Derived Variable Schema

    Game Metadata:

        game_id (Integer / Character): Unique CFB identifier.

        season (Integer): e.g., 2024, 2025.

        week (Integer): 1 to 15 + Postseason.

        neutral_site (Logical): Overrides home-field indicators.

        home_team, away_team (Character): Canonical team identifiers.

        home_division, away_division (Character): 'fbs' or 'fcs'.

    WLD Features:

        home_points, away_points (Integer): Final scores.

        overtime_flag (Logical): Indicates match went into overtime.

        home_outcome_cat (Factor: 1=RegLoss, 2=OTLoss, 3=OTWin, 4=RegWin).

    XPD Play-by-Play Features:

        epa (Numeric): Expected Points Added per play.

        posteam (Character): Possessing team.

        defteam (Character): Defending team.

        garbage_time (Logical): Garbage time filter indicator (∣margin∣>28 in 2nd half).

        home_net_epa (Numeric): Total home EPA minus total away EPA.

4. Pipeline Architecture & Orchestration

The pipeline is orchestrated via targets backed by a high-throughput analytical store (DuckDB) to enable weekly state caching and fast invalidations.

[ cfbfastR / CFBD API ]
          │
          ▼
   [ data/cfb.duckdb ]  ◄── Incremental Sync (Weekly)
          │
    targets Graph
    ├── 1. raw_schedules_pbp
    ├── 2. transform_wld_xpd
    ├── 3. fit_wld_stan (brms / cmdstanr)
    ├── 4. fit_xpd_stan (brms / cmdstanr)
    ├── 5. convergence_diagnostics (R-hat, ESS, Divergences)
    ├── 6. prior_sensitivity_sweep
    ├── 7. benchmark_comparison (AP, CFP, SP+, Colley)
    └── 8. render_reports_and_dashboards

4.1 Convergence Checks & Diagnostic Gates

A target execution stops pipeline execution if MCMC diagnostics fail:

    R≤1.01 for all latent team parameters (θi​,αi​).

    ESSbulk​≥400 and ESStail​≥400.

    Number of divergent transitions =0.

    E-BFMI >0.2 for all chains.

4.2 Prior Sensitivity Analysis

The system runs perturbation checks on the FCS prior penalty μFCS​ and scale σFCS​:

    Perturbation grid: μFCS​∈
    −1.0,−1.5,−2.0, σFCS​∈
    0.3,0.5,0.8.

    Diagnostic Metric: Top-25 Jaccard Similarity and Spearman Rank Correlation ρ. Sensitivity alert triggers if ρ<0.985.

5. Repository File Structure

cfb-bayes-rank/
├── _targets.R
├── cfb-bayes-rank.Rproj
├── .gitignore
├── README.md
├── PLAN.md
├── data/
│   ├── cfb.duckdb
│   └── external_polls/
├── R/
│   ├── 01_ingest_duckdb.R
│   ├── 02_transform.R
│   ├── 03_models_stan.R
│   ├── 04_diagnostics.R
│   ├── 05_sonneborn_berger.R
│   └── 06_benchmark.R
├── stan/
│   ├── wld_ordered_logit.stan
│   └── xpd_student_t.stan
├── docs/
│   ├── reports/
│   │   ├── weekly_briefing.qmd
│   │   └── season_retro.qmd
│   ├── presentation/
│   │   └── methodology.qmd
│   └── styles/
│       ├── custom.scss
│       └── tol_palette.scss
└── dashboard/
    ├── app.R
    └── modules/

6. Target Packages

    Data Wrangling & Core: cfbfastR, tidyverse, duckdb, DBI, arrow, qs

    Bayesian Inference & Diagnostics: cmdstanr, brms, posterior, bayesplot

    Workflow & Parallel Processing: targets, crew, future, furrr

    Presentation & Reporting: quarto, ggplot2, gt, gtExtras, patchwork, plotly
    """

with open("PLAN.md", "w") as f:
f.write(plan_content)

print("File written successfully.")


```text?code_stdout&code_event_index=1
File written successfully.

Your Markdown file is ready
MD icon
PLAN
 MD 

PLAN.md has been compiled to document the complete theoretical, statistical, computational, and architectural design of the retrospective Bayesian CFB ranking engine.
Clarification on Deliverables

Before building out the Quarto reporting and dashboard layers, what visual and layout themes do you prefer for:

    Dashboard: Preferred UI framework (e.g., Quarto Dashboard with bslib / shiny, or a standalone interactive HTML layout), color schema, and density (compact technical vs. wide analytical)?

    Weekly / Retrospective Reports: Document format (e.g., HTML report or Typst / LaTeX PDF), typography, and accent palette?

    Methodology Presentation: Revealjs theme base, slide aspect ratio, and CSS/SCSS styling preferences?

Https://arxiv.org/html/2601.14727v1 add this at the end of the document along with the theme/etc questions. Maybe be useful is Bradley terry stuff is used. Will want to have sources/references

Your Markdown file is ready
MD icon
PLAN
 MD 

PLAN.md has been updated with:

    The foundational and modern literature references, including the recent survey on high-dimensional Bradley–Terry models (Fang et al., 2026, arXiv:2601.14727) for sparse comparison graph asymptotics and regularization under divergent item sets.

    Foundational works in paired comparisons, sports analytics, and modern Bayesian computational workflows.

    A specification checklist covering delivery preferences across the Quarto Dashboard, weekly/seasonal reports, and Revealjs methodology slides.


Gemini is AI and can make mistakes.

# CFB Retrospective Bayesian Ranking Engine: Architecture & Technical Specification

## 1. Executive Summary & Philosophy

This project implements a dual-pipeline Bayesian ranking system for NCAA College Football (CFB) spanning all FBS and full-connectivity FCS teams. 

Unlike predictive power ratings (e.g., SP+, FPI, Sagarin, standard KenPom) that predict future performance spread on neutral fields, this system operates strictly as a **retrospective evaluative ranking (Strength of Record / Quality of Past Achievement)**. It measures *what teams have earned given what happened*, using end-of-season opponent performance as the back-propagating standard of proof.

The mathematical core blends Swiss-tournament tie-breaking concepts (Sonneborn–Berger and Buchholz/Solkoff metrics) with Bayesian hierarchical regression to account for structural disparities between FBS and FCS competition.

---

## 2. Mathematical Framework

### 2.1 Graph Architecture & Latent Schedule Valuation
Let $\mathcal{G} = (\mathcal{V}, \mathcal{E})$ be the directed multi-graph of completed matchups in a given season, where vertices $i \in \mathcal{V}$ represent teams and edges $k \in \mathcal{E}$ represent games between $i = \text{home}[k]$ and $j = \text{away}[k]$.

To eradicate early-season scheduling noise and small-sample bias, game performance is evaluated against the opponent's posterior aggregate capability across the entire schedule:
```latex
\text{SB}_i = \sum_{k \in \text{Games}(i)} w_{k, i} \cdot \mathcal{M}(\theta_{\text{opp}[k]})
```
where $w_{k, i}$ is the outcome weight and $\mathcal{M}(\theta)$ maps latent posterior opponent strength into game equity.

```latex
\text{BH}_i = \sum_{k \in \text{Games}(i)} \mathcal{M}(\theta_{\text{opp}[k]})
```

---

### 2.2 Pipeline 1: WLD (Discrete Outcome Resume Engine)
Pipeline 1 models win-loss-overtime discrete outcomes without score margin inflations.

* **Outcome Space:**
  * Regulation Win ($W$): 3 points ($Y_k = 4$)
  * Overtime Win ($	ext{OT}_W$): 2 points ($Y_k = 3$)
  * Overtime Loss ($	ext{OT}_L$): 1 point ($Y_k = 2$)
  * Regulation Loss ($L$): 0 points ($Y_k = 1$)

* **Likelihood:** Cumulative Ordered Logit
```latex
Y_k \sim \text{OrderedLogit}(\eta_k, \mathbf{c})
```
```latex
\eta_k = \theta_{\text{home}[k]} - \theta_{\text{away}[k]} + \gamma_{\text{home}}
```
where $\mathbf{c} = (c_1, c_2, c_3)$ are internal cutpoints strictly constrained such that $c_1 < c_2 < c_3$, and $\gamma_{\text{home}}$ is home-field advantage.

* **Priors & Sub-Division Hierarchical Shrinkage:**
To prevent FCS transitivities from corrupting FBS ratings due to sparse cross-tier scheduling:
```latex
\theta_i \sim \mathcal{N}\left(\mu_{\text{sub}[i]}, \, \sigma^2_{\text{sub}[i]}\right), \quad \text{sub}[i] \in \{\text{FBS}, \text{FCS}\}
```
```latex
\mu_{\text{FBS}} = 0 \quad (\text{identification anchor})
```
```latex
\mu_{\text{FCS}} \sim \mathcal{N}(-1.5, 0.5^2)
```
```latex
\sigma_{\text{FBS}}, \sigma_{\text{FCS}} \sim \text{Exponential}(1)
```
```latex
\gamma_{\text{home}} \sim \mathcal{N}(0.2, 0.1^2)
```

---

### 2.3 Pipeline 2: XPD (Expected Point Differential Engine)
Pipeline 2 isolates structural per-play dominance by computing game-level Expected Point Differential (XPD) via play-by-play Expected Points Added (EPA).

* **Play-Level Aggregation:**
```latex
\text{XPD}_k = \sum_{p \in \text{Plays}(k)} \text{EPA}_{p, \text{home}} - \sum_{p \in \text{Plays}(k)} \text{EPA}_{p, \text{away}}
```

* **Likelihood:** Student-$t$ robust regression to temper non-representative garbage-time plays and 70-point blowouts:
```latex
\text{XPD}_k \sim \text{Student-}t\left(\nu, \, \alpha_{\text{home}[k]} - \alpha_{\text{away}[k]} + \beta_{\text{home}}, \, \tau^2\right)
```
where:
* $\alpha_i$ is latent efficiency for team $i$.
* $\nu \sim \text{Gamma}(2, 0.1)$ represents degrees of freedom for outlier downweighting ($
u \in [4, 7]$).
* $\beta_{\text{home}} \sim \mathcal{N}(2.5, 1.0^2)$ represents expected point boost for home field.
* Sub-division hierarchy:
```latex
\alpha_i \sim \mathcal{N}\left(\xi_{\text{sub}[i]}, \, \omega^2_{\text{sub}[i]}\right)
```
```latex
\xi_{\text{FBS}} = 0, \quad \xi_{\text{FCS}} \sim \mathcal{N}(-21.0, 3.0^2)
```

---

### 2.4 Hybrid Sonneborn-Berger Integration
The dual pipelines can be reported independently or combined into a composite resume score:
```latex
\text{Index}_i = \lambda \cdot \Phi\left(\frac{\theta_i - \bar{\theta}}{\text{SD}(\theta)}\right) + (1 - \lambda) \cdot \Phi\left(\frac{\alpha_i - \bar{\alpha}}{\text{SD}(\alpha)}\right)
```
where $\lambda = 0.65$ prioritizes the primary objective of winning games while $\Phi(\cdot)$ represents the standard normal CDF standardizing scales to $[0, 1]$.

---

## 3. Data Sources & Variable Definitions

### 3.1 Raw Ingestion (`cfbfastR`)
* Historical EPA/WPA Play-by-Play: `load_cfb_pbp(season)` (covers FBS 2014–present).
* Historical lower-tier / FCS matchups: `load_ncaa_mfb_pbp(season)` (covers 2013–present).
* Schedules, venues, post-season flags: `load_cfb_schedules(season)`.
* Benchmark Polls: `cfbd_rankings(season, week)` (AP Top 25, Coaches Poll, CFP Committee Rankings).

### 3.2 Ingested & Derived Variable Schema
* **Game Metadata:**
  * `game_id` (Integer / Character): Unique CFB identifier.
  * `season` (Integer): e.g., 2024, 2025.
  * `week` (Integer): 1 to 15 + Postseason.
  * `neutral_site` (Logical): Overrides home-field indicators.
  * `home_team`, `away_team` (Character): Canonical team identifiers.
  * `home_division`, `away_division` (Character): `'fbs'` or `'fcs'`.
* **WLD Features:**
  * `home_points`, `away_points` (Integer): Final scores.
  * `overtime_flag` (Logical): Indicates match went into overtime.
  * `home_outcome_cat` (Factor: 1=RegLoss, 2=OTLoss, 3=OTWin, 4=RegWin).
* **XPD Play-by-Play Features:**
  * `epa` (Numeric): Expected Points Added per play.
  * `posteam` (Character): Possessing team.
  * `defteam` (Character): Defending team.
  * `garbage_time` (Logical): Garbage time filter indicator ($|\text{margin}| > 28$ in 2nd half).
  * `home_net_epa` (Numeric): Total home EPA minus total away EPA.

---

## 4. Pipeline Architecture & Orchestration

The pipeline is orchestrated via `targets` backed by a high-throughput analytical store (`DuckDB`) to enable weekly state caching and fast invalidations.

```
[ cfbfastR / CFBD API ]
          │
          ▼
   [ data/cfb.duckdb ]  ◄── Incremental Sync (Weekly)
          │
    targets Graph
    ├── 1. raw_schedules_pbp
    ├── 2. transform_wld_xpd
    ├── 3. fit_wld_stan (brms / cmdstanr)
    ├── 4. fit_xpd_stan (brms / cmdstanr)
    ├── 5. convergence_diagnostics (R-hat, ESS, Divergences)
    ├── 6. prior_sensitivity_sweep
    ├── 7. benchmark_comparison (AP, CFP, SP+, Colley)
    └── 8. render_reports_and_dashboards
```

### 4.1 Convergence Checks & Diagnostic Gates
A target execution stops pipeline execution if MCMC diagnostics fail:
* $\widehat{R} \le 1.01$ for all latent team parameters ($\theta_i, \alpha_i$).
* $\text{ESS}_{\text{bulk}} \ge 400$ and $\text{ESS}_{\text{tail}} \ge 400$.
* Number of divergent transitions $= 0$.
* E-BFMI $> 0.2$ for all chains.

### 4.2 Prior Sensitivity Analysis
The system runs perturbation checks on the FCS prior penalty $\mu_{\text{FCS}}$ and scale $\sigma_{\text{FCS}}$:
* Perturbation grid: $\mu_{\text{FCS}} \in \{-1.0, -1.5, -2.0\}$, $\sigma_{\text{FCS}} \in \{0.3, 0.5, 0.8\}$.
* Diagnostic Metric: Top-25 Jaccard Similarity and Spearman Rank Correlation $\rho$. Sensitivity alert triggers if $\rho < 0.985$.

---

## 5. Repository File Structure

```
cfb-bayes-rank/
├── _targets.R
├── cfb-bayes-rank.Rproj
├── .gitignore
├── README.md
├── PLAN.md
├── data/
│   ├── cfb.duckdb
│   └── external_polls/
├── R/
│   ├── 01_ingest_duckdb.R
│   ├── 02_transform.R
│   ├── 03_models_stan.R
│   ├── 04_diagnostics.R
│   ├── 05_sonneborn_berger.R
│   └── 06_benchmark.R
├── stan/
│   ├── wld_ordered_logit.stan
│   └── xpd_student_t.stan
├── docs/
│   ├── reports/
│   │   ├── weekly_briefing.qmd
│   │   └── season_retro.qmd
│   ├── presentation/
│   │   └── methodology.qmd
│   └── styles/
│       ├── custom.scss
│       └── tol_palette.scss
└── dashboard/
    ├── app.R
    └── modules/
```

---

## 6. Target Packages

* **Data Wrangling & Core:** `cfbfastR`, `tidyverse`, `duckdb`, `DBI`, `arrow`, `qs`
* **Bayesian Inference & Diagnostics:** `cmdstanr`, `brms`, `posterior`, `bayesplot`
* **Workflow & Parallel Processing:** `targets`, `crew`, `future`, `furrr`
* **Presentation & Reporting:** `quarto`, `ggplot2`, `gt`, `gtExtras`, `patchwork`, `plotly`


---

## 7. Theoretical & Methodological References

* **Bradley-Terry Modern Theory & Large-Scale Asymptotics:**
  * Fang, S., Han, R., Luo, Y., & Xu, Y. (2026). *Recent advances in the Bradley–Terry Model: theory, algorithms, and applications*. arXiv preprint arXiv:2601.14727 [stat.ME]. https://arxiv.org/abs/2601.14727
    * *Core utility:* Formal asymptotic properties under sparse comparison graphs ($p \ll n$), regularized estimation, dynamic ranges, and algorithmic convergence properties for massive pairwise systems.
* **Paired Comparisons & Ordinal Formulations:**
  * Bradley, R. A., & Terry, M. E. (1952). *Rank analysis of incomplete block designs: I. The method of paired comparisons*. Biometrika, 39(3/4), 324–345.
  * Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.). Wiley.
  * Cattelan, M. (2012). *Models for paired comparison data: A review with emphasis on dependent data*. Statistical Science, 27(3), 412–433.
* **College Football Ratings & Resume Evaluation:**
  * Connelly, B. (2017+). *SP+ Methodology and Efficiency Diagnostics*. ESPN Analytics.
  * Colley, W. N. (2002). *Colley's Bias Free College Football Ranking Method: The Colley Matrix Explained*. Princeton University.
* **Bayesian Computation & Workflow:**
  * Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Bürkner, P. C., Yanchenko, A. K., & Modrák, M. (2020). *Bayesian workflow*. arXiv preprint arXiv:2011.01808.
  * Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C. (2021). *Rank-normalization, folding, and localization: An improved $\widehat{R}$ for assessing convergence of MCMC*. Bayesian Analysis, 16(2), 667–718.

---

## 8. Dashboard, Reporting, & Presentation Architecture: Design Questions

To finalize the delivery interfaces across Quarto and interactive layers, specify preferences for the following styling parameters:

* **Quarto Dashboard (`dashboard/`):**
  * **Framework:** Interactive Shiny-for-R runtime vs. static Quarto Dashboard (`format: dashboard`) with embedded client-side `plotly` and `gt` tables?
  * **Theme & Palette:** Dark theme (e.g., `cosmo`, `darkly`) vs. light technical layout (`flatly`, `zephyr`), integrated with Paul Tol color palettes (`light`, `muted`, or `high-contrast`) for categorical division markers?
  * **View Layout:** Tabular rank matrix on the left with dynamic head-to-head resume cards on the right, or single-page cascading metric panels?

* **Automated Weekly & Retrospective Reports (`docs/reports/`):**
  * **Rendering Target:** Modern HTML reports (`format: html` with floating TOC, code-folding, light/dark switch) vs. Typst / LaTeX PDF documents (`format: typst`) formatted for formal print distribution?
  * **Typography:** System fonts (e.g., San Francisco / Inter) vs. serif academic typography (e.g., Linux Libertine, Computer Modern)?

* **Methodology Presentation (`docs/presentation/`):**
  * **Format:** Revealjs (`format: revealjs`) slide deck?
  * **Slide Aspect Ratio & Layout:** 16:9 widescreen layout with standalone SCSS styling?
  * **Math Engine:** KaTeX vs. MathJax for typesetting LaTeX equations?


# Theme reference: github.com/thomasqmd/QMDThemes