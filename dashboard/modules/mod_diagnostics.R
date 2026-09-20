# ==============================================================================
# Shiny Module: mod_diagnostics.R
# Purpose: Interactive MCMC convergence monitoring across all 4 pipelines
#          (WLD, PD, XWLD, XPD) and prior sensitivity sweep review
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
    # 2x2 Grid for the 4 MCMC Convergence Gates
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Pipeline 1: WLD (Ordered Logit Actual Outcomes)"),
        card_body(
          DTOutput(ns("wld_gate_table"))
        )
      ),
      card(
        card_header("Pipeline 2: PD (Robust Student-t Point Differential)"),
        card_body(
          DTOutput(ns("pd_gate_table"))
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Pipeline 3: XWLD (Ordered Logit Expected Outcomes)"),
        card_body(
          DTOutput(ns("xwld_gate_table"))
        )
      ),
      card(
        card_header("Pipeline 4: XPD (Robust Student-t Net EPA Differential)"),
        card_body(
          DTOutput(ns("xpd_gate_table"))
        )
      )
    ),
    # Prior Sensitivity Grid & Plot
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

mod_diagnostics_server <- function(id, diag_wld_data, diag_pd_data = NULL, diag_xwld_data = NULL, diag_xpd_data, sensitivity_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    render_gate_dt <- function(diag_react) {
      req(diag_react())
      datatable(
        diag_react()$gate_report,
        options = list(dom = "t"),
        rownames = FALSE
      ) |>
        formatRound(columns = "Value", digits = 4)
    }
    
    output$wld_gate_table <- renderDT({
      render_gate_dt(diag_wld_data)
    })
    
    output$pd_gate_table <- renderDT({
      if (!is.null(diag_pd_data)) {
        render_gate_dt(diag_pd_data)
      } else {
        render_gate_dt(diag_wld_data)
      }
    })

    output$xwld_gate_table <- renderDT({
      if (!is.null(diag_xwld_data)) {
        render_gate_dt(diag_xwld_data)
      } else {
        render_gate_dt(diag_wld_data)
      }
    })
    
    output$xpd_gate_table <- renderDT({
      render_gate_dt(diag_xpd_data)
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
