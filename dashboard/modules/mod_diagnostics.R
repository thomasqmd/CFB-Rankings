# ==============================================================================
# Shiny Module: mod_diagnostics.R
# Purpose: Interactive MCMC convergence monitoring and prior sensitivity sweep review
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)

mod_diagnostics_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Pipeline 1: WLD Convergence Diagnostics"),
        card_body(
          DTOutput(ns("wld_gate_table"))
        )
      ),
      card(
        card_header("Pipeline 2: XPD Convergence Diagnostics"),
        card_body(
          DTOutput(ns("xpd_gate_table"))
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Prior Sensitivity Sweep (FCS Latent Penalty)"),
        card_body(
          plotlyOutput(ns("sensitivity_plot"), height = "350px")
        )
      ),
      card(
        card_header("FCS Hyperparameter Perturbation Grid"),
        card_body(
          DTOutput(ns("sensitivity_table"))
        )
      )
    )
  )
}

mod_diagnostics_server <- function(id, diag_wld_data, diag_xpd_data, sensitivity_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$wld_gate_table <- renderDT({
      req(diag_wld_data())
      datatable(
        diag_wld_data()$gate_report,
        options = list(dom = "t"),
        rownames = FALSE
      ) |>
        formatRound(columns = "Value", digits = 4)
    })
    
    output$xpd_gate_table <- renderDT({
      req(diag_xpd_data())
      datatable(
        diag_xpd_data()$gate_report,
        options = list(dom = "t"),
        rownames = FALSE
      ) |>
        formatRound(columns = "Value", digits = 4)
    })
    
    output$sensitivity_table <- renderDT({
      req(sensitivity_data())
      datatable(
        sensitivity_data(),
        options = list(dom = "t", pageLength = 10),
        rownames = FALSE
      ) |>
        formatRound(columns = c("spearman_rho", "jaccard_top25"), digits = 4)
    })
    
    output$sensitivity_plot <- renderPlotly({
      req(sensitivity_data())
      df <- sensitivity_data()
      
      p <- ggplot(df, aes(x = factor(mu_fcs), y = spearman_rho, fill = factor(sigma_fcs))) +
        geom_col(position = "dodge", width = 0.6) +
        geom_hline(yintercept = 0.985, linetype = "dashed", color = "#EE6677") +
        scale_fill_manual(values = c("0.3" = "#4477AA", "0.5" = "#66CCEE", "0.8" = "#228833")) +
        coord_cartesian(ylim = c(0.95, 1.0)) +
        labs(
          x = "FCS Latent Mean (μ_FCS)",
          y = "Spearman Rank Correlation (ρ)",
          fill = "FCS Scale (σ_FCS)"
        ) +
        theme_minimal()
      
      ggplotly(p)
    })
  })
}
