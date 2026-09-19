# ==============================================================================
# Application: dashboard/app.R
# Purpose: Interactive College Football Bayesian Retrospective Analytics Dashboard
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)

# Source dashboard modules
source("modules/mod_rankings.R")
source("modules/mod_team_card.R")
source("modules/mod_benchmarks.R")
source("modules/mod_diagnostics.R")

# Check for targets data availability or load mock demonstration state
load_dashboard_data <- function() {
  has_targets <- tryCatch({
    targets::tar_read(composite_rankings)
    TRUE
  }, error = function(e) FALSE)
  
  if (has_targets) {
    message("[Dashboard] Successfully loaded live targets pipeline artifacts.")
    list(
      composite = targets::tar_read(composite_rankings),
      wld_df = targets::tar_read(modeling_data)$wld_df,
      benchmark = targets::tar_read(benchmark_comparison),
      diag_wld = targets::tar_read(diagnostics_wld),
      diag_xpd = targets::tar_read(diagnostics_xpd),
      sensitivity = targets::tar_read(prior_sensitivity_wld)
    )
  } else {
    message("[Dashboard] Targets store not yet computed. Loading demonstration dataset.")
    # Demonstration fallback data
    demo_teams <- c(
      "Oregon", "Georgia", "Texas", "Penn State", "Notre Dame", 
      "Ohio State", "Tennessee", "Indiana", "Boise State", "SMU",
      "Arizona State", "Clemson", "Alabama", "Miami", "Ole Miss",
      "South Carolina", "BYU", "Iowa State", "Colorado", "Army"
    )
    n <- length(demo_teams)
    
    comp <- tibble::tibble(
      team = demo_teams,
      conference = c("Big Ten", "SEC", "SEC", "Big Ten", "Independent",
                     "Big Ten", "SEC", "Big Ten", "Mountain West", "ACC",
                     "Big 12", "ACC", "SEC", "ACC", "SEC",
                     "SEC", "Big 12", "Big 12", "Big 12", "AAC"),
      subdivision_id = 1L,
      theta_median = seq(2.4, 0.8, length.out = n),
      alpha_median = seq(16.5, 4.2, length.out = n),
      sb_score = seq(18.5, 9.2, length.out = n),
      buchholz_sos = seq(8.4, 5.1, length.out = n),
      phi_theta = stats::pnorm(scale(seq(2.4, 0.8, length.out = n))),
      phi_alpha = stats::pnorm(scale(seq(16.5, 4.2, length.out = n))),
      composite_index = 0.65 * phi_theta + 0.35 * phi_alpha,
      rank_composite = 1:n
    )
    
    games <- tibble::tibble(
      game_id = paste0("g_", 1:(n*5)),
      week = rep(1:5, each = n),
      home_team = rep(demo_teams, 5),
      away_team = rep(rev(demo_teams), 5),
      home_points = sample(21:45, n*5, replace = TRUE),
      away_points = sample(10:35, n*5, replace = TRUE),
      outcome = sample(c(1L, 3L, 4L), n*5, replace = TRUE, prob = c(0.3, 0.1, 0.6))
    )
    
    bench <- list(
      comparison_table = comp |>
        dplyr::mutate(
          rank_colley = sample(1:n),
          rank_ap = c(1:15, rep(NA, n-15)),
          ap_delta = ifelse(is.na(rank_ap), 0, rank_ap - rank_composite)
        ),
      benchmark_metrics = tibble::tibble(
        Benchmark = c("Colley Matrix", "AP Top 25"),
        Spearman_Rho = c(0.962, 0.941),
        Kendall_Tau = c(0.881, 0.842),
        Top25_Jaccard = c(0.920, 0.880)
      ),
      most_underrated = tibble::tibble(
        team = c("Penn State", "Notre Dame", "SMU"),
        rank_composite = c(4, 5, 10),
        rank_ap = c(6, 8, 14),
        ap_delta = c(2, 3, 4)
      ),
      most_overrated = tibble::tibble(
        team = c("Miami", "Alabama", "Clemson"),
        rank_composite = c(14, 13, 12),
        rank_ap = c(9, 11, 8),
        ap_delta = c(-5, -2, -4)
      )
    )
    
    diag_w <- list(
      passed = TRUE,
      gate_report = tibble::tibble(
        Metric = c("Max R-hat", "Min ESS Bulk", "Min ESS Tail", "Post-warmup Divergences", "Min E-BFMI"),
        Value = c(1.002, 1450, 1120, 0, 0.88),
        Threshold = c("<= 1.01", ">= 400", ">= 400", "= 0", "> 0.20"),
        Status = rep("PASS", 5)
      )
    )
    
    diag_x <- list(
      passed = TRUE,
      gate_report = tibble::tibble(
        Metric = c("Max R-hat", "Min ESS Bulk", "Min ESS Tail", "Post-warmup Divergences", "Min E-BFMI"),
        Value = c(1.004, 1620, 1340, 0, 0.82),
        Threshold = c("<= 1.01", ">= 400", ">= 400", "= 0", "> 0.20"),
        Status = rep("PASS", 5)
      )
    )
    
    sens <- tibble::tibble(
      mu_fcs = rep(c(-1.0, -1.5, -2.0), each = 3),
      sigma_fcs = rep(c(0.3, 0.5, 0.8), 3),
      spearman_rho = c(0.998, 0.997, 0.995, 1.000, 0.999, 0.996, 0.996, 0.994, 0.991),
      jaccard_top25 = c(1.0, 1.0, 0.96, 1.0, 1.0, 0.96, 0.96, 0.96, 0.92),
      alert_flag = rep(FALSE, 9)
    )
    
    list(
      composite = comp,
      wld_df = games,
      benchmark = bench,
      diag_wld = diag_w,
      diag_xpd = diag_x,
      sensitivity = sens
    )
  }
}

app_data <- load_dashboard_data()

# User Interface
ui <- page_navbar(
  title = "CFB Bayesian Retrospective Analytics",
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#4477AA",
    success = "#228833",
    danger = "#EE6677"
  ),
  nav_panel(
    title = "Rankings Table",
    icon = bsicons::bs_icon("list-ol"),
    mod_rankings_ui("rankings")
  ),
  nav_panel(
    title = "Team Resume Card",
    icon = bsicons::bs_icon("person-vcard"),
    mod_team_card_ui("team_card")
  ),
  nav_panel(
    title = "Benchmark Comparisons",
    icon = bsicons::bs_icon("diagram-3"),
    mod_benchmarks_ui("benchmarks")
  ),
  nav_panel(
    title = "MCMC Diagnostics & Sensitivity",
    icon = bsicons::bs_icon("check2-circle"),
    mod_diagnostics_ui("diagnostics")
  ),
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode")
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive state containers
  composite_data <- reactiveVal(app_data$composite)
  games_data <- reactiveVal(app_data$wld_df)
  benchmark_data <- reactiveVal(app_data$benchmark)
  diag_wld_data <- reactiveVal(app_data$diag_wld)
  diag_xpd_data <- reactiveVal(app_data$diag_xpd)
  sensitivity_data <- reactiveVal(app_data$sensitivity)
  
  selected_team <- reactiveVal(NULL)
  
  # Initialize Rankings Module
  mod_rankings_server(
    id = "rankings",
    composite_data = composite_data,
    on_select_team = function(team_name) {
      selected_team(team_name)
      # Switch navigation to Team Resume Card panel
      nav_select("team_card")
    }
  )
  
  # Initialize Team Card Module
  mod_team_card_server(
    id = "team_card",
    composite_data = composite_data,
    games_data = games_data,
    selected_team_reactive = selected_team
  )
  
  # Initialize Benchmark Module
  mod_benchmarks_server(
    id = "benchmarks",
    benchmark_data = benchmark_data
  )
  
  # Initialize Diagnostics Module
  mod_diagnostics_server(
    id = "diagnostics",
    diag_wld_data = diag_wld_data,
    diag_xpd_data = diag_xpd_data,
    sensitivity_data = sensitivity_data
  )
}

shinyApp(ui = ui, server = server)
