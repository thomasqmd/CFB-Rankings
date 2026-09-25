# CFB Retrospective Bayesian Ranking Engine

> A high-throughput, Bayesian retrospective ranking engine for NCAA Division I College Football (FBS & FCS), powered by a **4-pipeline performance matrix**, hierarchical FCS shrinkage, Sonneborn–Berger schedule equity, and DuckDB analytical storage.

---

## 1. System Overview & Methodology

The engine publishes two orderings:

* **Ranking (SOR):** past accomplishment (**retrospective resume**). By constructing complete season directed multi-graphs $\mathcal{G} = (\mathcal{V}, \mathcal{E})$, it values wins, losses, and margins strictly through end-of-season opponent performance.
* **Power Ranking:** predicted neutral-field strength going forward, built from expected-performance signals (XPD, opponent-adjusted EPA/play, XWLD) plus realized margin. `power_rating` is in points; `power_rating[A] - power_rating[B]` is the expected neutral-field spread.

### 1.1 The 2×2 Orthogonal Performance Matrix

To avoid conflating luck, finishing ability, and per-play efficiency, the engine evaluates games across four orthogonal pipelines:

```
                                  [ cfbfastR / CFBD API ]
                                             │
                                             ▼
                              ┌─────────────────────────────────────┐
                              │ DuckDB Analytical Store (PBP/Sched) │
                              └──────────────────┬──────────────────┘
                                                 │
                  ┌──────────────────────────────┴──────────────────────────────┐
                  ▼                                                             ▼
     ┌───────────────────────────┐                                 ┌───────────────────────────┐
     │  Pipeline 1: WLD Engine   │                                 │  Pipeline 2: PD Engine    │
     │   (Actual Discrete SOR)   │                                 │   (Actual Margin / Blowout)│
     ├───────────────────────────┤                                 ├───────────────────────────┤
     │ Cumulative Ordered Logit  │                                 │ Heavy-Tailed Student-t    │
     │ Outcomes: W=3, OTW=2,     │                                 │ Scoreboard Point Margin   │
     │           OTL=1, L=0      │                                 │ Residual Scale: τ_pd      │
     │ Latent Resume: θ_i        │                                 │ Latent Margin: δ_i        │
     └────────────┬──────────────┘                                 └────────────┬──────────────┘
                  │                                                             │
                  ├──────────────────────────────┬──────────────────────────────┘
                  │                              │
                  ▼                              ▼
     ┌───────────────────────────┐ ┌───────────────────────────┐
     │  Pipeline 3: XWLD Engine  │ │  Pipeline 4: XPD Engine   │
     │  (Expected Discrete Wins) │ │ (Expected Continuous EPA) │
     ├───────────────────────────┤ ├───────────────────────────┤
     │ Ordered Logit on PGWE     │ │ Heavy-Tailed Student-t    │
     │ Post-Game Win Expectancy  │ │ Net EPA Aggregation       │
     │ Deserved Wins: ω_i        │ │ Latent Efficiency: α_i    │
     └────────────┬──────────────┘ └────────────┬──────────────┘
                  │                              │
                  └──────────────┬───────────────┘
                                 ▼
               ┌─────────────────────────────────────┐
               │  Ranking: Strength of Resume (SOR)  │
               │  (past performance)                 │
               │   WLD θ 40% + SB 30% + PD δ 20%     │
               │   + Buchholz SOS 10%                │
               │                                     │
               │  Power Ranking (neutral field)      │
               │   XPD α 40% + Adj. EPA/play 25%     │
               │   + XWLD ω 20% + PD δ 15%           │
               │                                     │
               │  Cross-Pipeline Luck & Divergence:  │
               │  - Power Gap: rank(Pow)-rank(SOR)   │
               │  - Resume Luck: rank(xSOR)-rank(SOR)│
               │  - Outcome Luck: rank(ω) - rank(θ)  │
               │  - Margin Luck:  rank(α) - rank(δ)  │
               └─────────────────────────────────────┘
```

| Pipeline | Domain | Outcome Type | Likelihood Model | Latent Parameter |
| :--- | :--- | :--- | :--- | :--- |
| **1. WLD** | **Actual** | Discrete ($Y_k \in \{1, 2, 3, 4\}$) | Cumulative Ordered Logit | $\theta_i$ (Actual Resume) |
| **2. PD** | **Actual** | Continuous ($\Delta \text{Pts} = Score_H - Score_A$) | Heavy-Tailed Student-$t$ | $\delta_i$ (Actual Point Margin) |
| **3. XWLD** | **Expected** | Discrete ($Y^{\text{exp}}_k \in \{1, 2, 3, 4\}$ via PGWE) | Cumulative Ordered Logit | $\omega_i$ (Expected Outcomes) |
| **4. XPD** | **Expected** | Continuous ($\Delta \text{XP} = \sum EPA_H - \sum EPA_A$) | Heavy-Tailed Student-$t$ | $\alpha_i$ (Net EPA Efficiency) |

### 1.2 Mathematical Specifications

* **Pipeline 1: WLD (Actual Discrete):**
  $$Y_k \sim \text{OrderedLogit}(\theta_{\text{home}[k]} - \theta_{\text{away}[k]} + \gamma_{\text{home}} \cdot (1 - \text{neutral}_k), \, \mathbf{c}_{\text{wld}})$$

* **Pipeline 2: PD (Actual Continuous Margin):**
  $$\Delta \text{Pts}_k \sim \text{Student-}t\left(\nu_{\text{pd}}, \, \delta_{\text{home}[k]} - \delta_{\text{away}[k]} + \beta_{\text{home, pd}} \cdot (1 - \text{neutral}_k), \, \tau_{\text{pd}}^2\right)$$

* **Pipeline 3: XWLD (Expected Discrete Outcomes):**
  $$Y^{\text{exp}}_k \sim \text{OrderedLogit}(\omega_{\text{home}[k]} - \omega_{\text{away}[k]} + \gamma_{\text{home, xwld}} \cdot (1 - \text{neutral}_k), \, \mathbf{c}_{\text{xwld}})$$

* **Pipeline 4: XPD (Expected Continuous Net EPA):**
  $$\Delta \text{XP}_k \sim \text{Student-}t\left(\nu_{\text{xpd}}, \, \alpha_{\text{home}[k]} - \alpha_{\text{away}[k]} + \beta_{\text{home, xpd}} \cdot (1 - \text{neutral}_k), \, \tau_{\text{xpd}}^2\right)$$

* **Sonneborn-Berger Schedule Equity:**
  $$\text{SB}_i = \sum_{k \in \text{Games}(i)} w_{k, i} \cdot \text{logit}^{-1}(\theta_{\text{opp}[k]})$$

* **Buchholz Schedule Strength:**
  $$\text{BH}_i = \sum_{k \in \text{Games}(i)} \text{logit}^{-1}(\theta_{\text{opp}[k]})$$

* **Opponent-Adjusted EPA/Play (Power input):** ridge-regularized paired comparison on game-level net EPA/play
  $$\text{EPA}^{\text{net}}_k = r_{\text{home}[k]} - r_{\text{away}[k]} + b_{\text{FCS}}(\text{fcs}_{\text{home}} - \text{fcs}_{\text{away}}) + h \cdot (1 - \text{neutral}_k) + \varepsilon_k$$

* **Hierarchical FCS Shrinkage:**
  $$\theta_i \sim \mathcal{N}\left(\mu_{\text{sub}[i]}, \, \sigma^2_{\text{sub}[i]}\right), \quad \mu_{\text{FBS}} = 0 \quad (\text{Anchor}), \quad \mu_{\text{FCS}} \sim \mathcal{N}(-1.5, 0.5^2)$$

---

## 2. Repository File Structure

```
CFB/
├── cfb-bayes-rank.Rproj        # RStudio project configuration
├── .gitignore                  # Git ignore rules for DuckDB, targets, and cache
├── README.md                   # System documentation and execution guide
├── plan.md                     # Architecture blueprint and theoretical notes
├── install_packages.R          # Dependency installer and CmdStan verification
├── _targets.R                  # Complete targets pipeline orchestration definition
├── data/                       # Local analytical storage
│   ├── cfb.duckdb              # High-throughput analytical DuckDB store
│   └── external_polls/         # Cached AP/Coaches/CFP poll benchmarks
├── R/                          # Modular analytical engines
│   ├── 01_ingest_duckdb.R      # Data ingestion & DuckDB incremental sync
│   ├── 02_transform.R          # 4-pipeline feature engineering and Stan formatting
│   ├── 03_models_stan.R        # CmdStan / Stan compilation and 4-model MCMC fitting
│   ├── 04_diagnostics.R        # Convergence assertions and prior sensitivity sweep
│   ├── 05_sonneborn_berger.R   # SB, Buchholz SOS, and 4-way Composite Index calculation
│   └── 06_benchmark.R          # Colley Matrix baseline and AP Poll divergence
├── stan/                       # Pure Stan probabilistic models
│   ├── wld_ordered_logit.stan  # Pipeline 1: Cumulative ordered logit model
│   ├── pd_student_t.stan       # Pipeline 2: Robust Student-t point differential
│   ├── xwld_ordered_logit.stan # Pipeline 3: Ordered logit expected outcomes
│   └── xpd_student_t.stan      # Pipeline 4: Robust Student-t net EPA model
├── docs/                       # Publishing, reporting, and presentations
│   ├── styles/
│   │   ├── tol_palette.scss    # Paul Tol colorblind-safe SCSS variables
│   │   └── custom.scss         # Quarto styles, cards, and typography
│   ├── reports/
│   │   └── season_retro.qmd    # Comprehensive season audit (in-season and end-of-season)
│   └── presentation/
│       └── methodology.qmd     # Revealjs slide deck on mathematical foundations
└── dashboard/                  # Interactive Shiny / bslib Web Application
    ├── app.R                   # Multi-tab dashboard entrypoint
    └── modules/
        ├── mod_rankings.R      # Filterable rankings table with 2x2 matrix weighting
        ├── mod_team_card.R     # Deep-dive 4-pipeline resume and luck inspection
        ├── mod_benchmarks.R    # AP Poll and Colley Matrix quadrant scatter
        └── mod_diagnostics.R   # 4-pipeline MCMC health gates and sensitivity sweep
```

---

## 3. Installation & Setup

### 3.1 Install R Dependencies & CmdStan
Run the dependency installer script in an R terminal:

```r
source("install_packages.R")
```

If CmdStan C++ backend is not yet installed:
```r
cmdstanr::check_cmdstan_toolchain()
cmdstanr::install_cmdstan(cores = parallel::detectCores())
```

---

## 4. Pipeline Execution & Orchestration

The pipeline is managed via [`targets`](https://books.ropensci.org/targets/).

### 4.1 Inspect Pipeline Graph
```r
library(targets)
tar_glimpse()  # or tar_visnetwork()
```

### 4.2 Execute Pipeline
```r
tar_make()
```

For multi-core parallel execution across targets:
```r
tar_make_future(workers = 4)
```

### 4.3 Read Pipeline Outputs in R
```r
library(targets)
rankings <- tar_read(composite_rankings)
benchmarks <- tar_read(benchmark_comparison)
diagnostics <- tar_read(diagnostics_wld)
```

---

## 5. Interactive Dashboard & Quarto Reports

### 5.1 Launch Interactive Shiny Dashboard
```r
shiny::runApp("dashboard")
```
* **Rankings Table:** Filterable Top 134 FBS (and FCS) rankings with 2×2 matrix controls (Actual vs. Expected and Discrete vs. Margin).
* **Team Resume Card:** Select any team to view its 4 performance pillars, luck metrics, and opponent quality scores.
* **Benchmark Divergence:** Interactive scatter plot contrasting Bayes Retro against AP Top 25 consensus.
* **MCMC Diagnostics:** 2×2 convergence audit grid ($\widehat{R}$, ESS, divergences) across all 4 pipelines.

### 5.2 Render Quarto Reports
```bash
# Render season retrospective report
quarto render docs/reports/season_retro.qmd

# Render Revealjs methodology slide deck
quarto render docs/presentation/methodology.qmd
```

---

## 6. MCMC Convergence Gates & Quality Control

Target pipeline assertions strictly enforce across all 4 pipelines:
* $\widehat{R} \le 1.01$ across all latent parameters ($\theta_i$, $\delta_i$, $\omega_i$, $\alpha_i$, $\mathbf{c}$, $\sigma_{\text{sub}}$).
* $\text{ESS}_{\text{bulk}} \ge 400$ and $\text{ESS}_{\text{tail}} \ge 400$.
* Exactly **$0$ post-warmup divergent transitions**.
* Minimum Energy Bayesian Fraction of Missing Information ($\text{E-BFMI} > 0.20$).
* Prior sensitivity sweep Spearman $\rho \ge 0.985$ under $\mu_{\text{FCS}} \in \{-1.0, -1.5, -2.0\}$.

---

## 7. Theoretical References

* **Bradley-Terry Modern Theory & Graph Asymptotics:**
  * Fang, S., Han, R., Luo, Y., & Xu, Y. (2026). *Recent advances in the Bradley–Terry Model: theory, algorithms, and applications*. arXiv:2601.14727.
* **Paired Comparisons & Ordinal Regression:**
  * Bradley, R. A., & Terry, M. E. (1952). *Rank analysis of incomplete block designs: I. The method of paired comparisons*. Biometrika, 39(3/4), 324–345.
  * Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.). Wiley.
* **College Football Ratings & Schedule Strength:**
  * Colley, W. N. (2002). *Colley's Bias Free College Football Ranking Method: The Colley Matrix Explained*. Princeton University.
  * Connelly, B. (2017+). *SP+ Methodology and Efficiency Diagnostics*. ESPN Analytics.
* **Bayesian Workflow:**
  * Gelman, A. et al. (2020). *Bayesian workflow*. arXiv:2011.01808.
  * Vehtari, A. et al. (2021). *Rank-normalization, folding, and localization: An improved $\widehat{R}$ for assessing convergence of MCMC*. Bayesian Analysis, 16(2), 667–718.
