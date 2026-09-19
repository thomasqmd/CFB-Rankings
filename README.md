# CFB Retrospective Bayesian Ranking Engine

[![Targets Pipeline](https://img.shields.io/badge/Workflow-targets-blue.svg)](https://books.ropensci.org/targets/)
[![Bayesian Backend](https://img.shields.io/badge/Bayesian-Stan%20%2F%20CmdStan-red.svg)](https://mc-stan.org/cmdstanr/)
[![Analytical DB](https://img.shields.io/badge/Store-DuckDB-yellow.svg)](https://duckdb.org/)
[![Interactive Dashboard](https://img.shields.io/badge/UI-Shiny%20%2F%20bslib-green.svg)](https://shiny.posit.co/)

A dual-pipeline Bayesian ranking system for NCAA College Football (CFB) evaluating all FBS programs and cross-tier FCS competition.

Unlike predictive systems (such as SP+, ESPN FPI, or standard KenPom) that estimate theoretical point spreads on neutral fields, this system operates strictly as a **retrospective evaluative ranking (Strength of Record / Quality of Past Achievement)**. It answers: *What did teams mathematically earn given what transpired?*, using full-schedule opponent performance as the back-propagating standard of proof.

---

## 1. System Architecture & Mathematical Formulation

The engine couples two Bayesian hierarchical pipelines to separate pure outcome achievement from per-play dominance, resolving FCS–FBS graph disconnection through hierarchical shrinkage.

```
                  ┌────────────────────────────────────────┐
                  │    DuckDB Analytical Store (PBP/Sched) │
                  └──────────────────┬─────────────────────┘
                                     │
                 ┌───────────────────┴───────────────────┐
                 ▼                                       ▼
    ┌───────────────────────────┐           ┌───────────────────────────┐
    │  Pipeline 1: WLD Engine   │           │  Pipeline 2: XPD Engine   │
    │   (Outcome Resume / SOR)  │           │   (Latent Play Dominance) │
    ├───────────────────────────┤           ├───────────────────────────┤
    │ Cumulative Ordered Logit  │           │ Heavy-Tailed Student-t    │
    │ Outcomes: W=3, OTW=2,     │           │ Net EPA Aggregation       │
    │           OTL=1, L=0      │           │ Garbage-Time Filtering    │
    │ Latent Ability: θ_i       │           │ Latent Efficiency: α_i    │
    └────────────┬──────────────┘           └────────────┬──────────────┘
                 │                                       │
                 ▼                                       ▼
    ┌───────────────────────────┐           ┌───────────────────────────┐
    │ Sonneborn-Berger & Buchholz│          │ Standardized EPA Residuals│
    │ SB_i = Σ w_ki * plogis(θ) │           │ Φ((α_i - μ) / σ)          │
    └────────────┬──────────────┘           └────────────┬──────────────┘
                 │                                       │
                 └───────────────────┬───────────────────┘
                                     ▼
                  ┌─────────────────────────────────────┐
                  │    Hybrid Composite Index Score     │
                  │   λ * Φ(Z_θ) + (1 - λ) * Φ(Z_α)     │
                  │       (Default: λ = 0.65)           │
                  └─────────────────────────────────────┘
```

### 1.1 Pipeline 1: WLD (Discrete Outcome Resume Engine)
* **Outcome Mapping:** Discrete categories $Y_k \in \{1: \text{L}, 2: \text{OTL}, 3: \text{OTW}, 4: \text{W}\}$
* **Likelihood:** Ordered Logit
  $$Y_k \sim \text{OrderedLogit}(\theta_{\text{home}[k]} - \theta_{\text{away}[k]} + \gamma_{\text{home}} \cdot (1 - \text{neutral}_k), \, \mathbf{c})$$
* **Sonneborn-Berger Tie-Break:**
  $$\text{SB}_i = \sum_{k \in \text{Games}(i)} w_{k, i} \cdot \text{logit}^{-1}(\theta_{\text{opp}[k]})$$
* **Buchholz Schedule Strength:**
  $$\text{BH}_i = \sum_{k \in \text{Games}(i)} \text{logit}^{-1}(\theta_{\text{opp}[k]})$$

### 1.2 Pipeline 2: XPD (Expected Point Differential Efficiency)
* **Play-Level EPA Differential:**
  $$\text{XPD}_k = \sum_{p \in \text{Plays}(k)} \text{EPA}_{p, \text{home}} - \sum_{p \in \text{Plays}(k)} \text{EPA}_{p, \text{away}}$$
* **Robust Student-t Likelihood:** Protects against blowout non-linearities and garbage-time variance:
  $$\text{XPD}_k \sim \text{Student-}t\left(\nu, \, \alpha_{\text{home}[k]} - \alpha_{\text{away}[k]} + \beta_{\text{home}} \cdot (1 - \text{neutral}_k), \, \tau^2\right)$$

### 1.3 Hierarchical FCS Shrinkage
To prevent disconnected transitive cycles between subdivisions:
$$\theta_i \sim \mathcal{N}\left(\mu_{\text{sub}[i]}, \, \sigma^2_{\text{sub}[i]}\right)$$
$$\mu_{\text{FBS}} = 0 \quad (\text{Identifiability Anchor}), \quad \mu_{\text{FCS}} \sim \mathcal{N}(-1.5, 0.5^2)$$

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
│   ├── 02_transform.R          # WLD & XPD feature engineering and Stan formatting
│   ├── 03_models_stan.R        # CmdStan / Stan compilation and MCMC fitting
│   ├── 04_diagnostics.R        # Convergence assertions and prior sensitivity sweep
│   ├── 05_sonneborn_berger.R   # SB, Buchholz SOS, and Composite Index calculation
│   └── 06_benchmark.R          # Colley Matrix baseline and AP Poll divergence
├── stan/                       # Pure Stan probabilistic models
│   ├── wld_ordered_logit.stan  # Cumulative ordered logit model
│   └── xpd_student_t.stan      # Robust Student-t net EPA model
├── docs/                       # Publishing, reporting, and presentations
│   ├── styles/
│   │   ├── tol_palette.scss    # Paul Tol colorblind-safe SCSS variables
│   │   └── custom.scss         # Quarto styles, cards, and typography
│   ├── reports/
│   │   ├── weekly_briefing.qmd # Weekly rankings release briefing
│   │   └── season_retro.qmd    # Comprehensive end-of-season audit
│   └── presentation/
│       └── methodology.qmd     # Revealjs slide deck on mathematical foundations
└── dashboard/                  # Interactive Shiny / bslib Web Application
    ├── app.R                   # Multi-tab dashboard entrypoint
    └── modules/
        ├── mod_rankings.R      # Filterable rankings table with dynamic λ slider
        ├── mod_team_card.R     # Deep-dive schedule resume inspection
        ├── mod_benchmarks.R    # AP Poll and Colley Matrix quadrant scatter
        └── mod_diagnostics.R   # MCMC health gates and prior sensitivity sweep
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
* **Rankings Table:** Searchable, filterable Top 134 FBS (and FCS) rankings with a dynamic $\lambda$ slider.
* **Team Resume Card:** Select any team to view its full schedule resume, opponent quality scores, and points earned.
* **Benchmark Divergence:** Interactive scatter plot contrasting Bayes Retro against AP Top 25 consensus.
* **MCMC Diagnostics:** Visual audit of $\widehat{R}$, ESS, divergences, and the prior perturbation sweep.

### 5.2 Render Quarto Reports
```bash
# Render weekly briefing HTML report
quarto render docs/reports/weekly_briefing.qmd

# Render season retrospective report
quarto render docs/reports/season_retro.qmd

# Render Revealjs methodology slide deck
quarto render docs/presentation/methodology.qmd
```

---

## 6. MCMC Convergence Gates & Quality Control

Target pipeline assertions strictly enforce:
* $\widehat{R} \le 1.01$ across all latent parameters ($\theta_i$, $\alpha_i$, $\mathbf{c}$, $\sigma_{\text{sub}}$).
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
