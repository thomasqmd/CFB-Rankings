# ==============================================================================
# Shiny Module: mod_rankings.R
# Purpose: Interactive, filterable rankings table with 4-pipeline dynamic weights
#          (WLD, PD, XWLD, XPD) and cross-pipeline divergence metrics
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(DT)

mod_rankings_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "4-Pipeline Controls",
        width = 320,
        selectInput(
          ns("subdivision_filter"),
          "Subdivision:",
          choices = c("FBS Only" = "fbs", "FCS Only" = "fcs", "All Programs" = "all"),
          selected = "fbs"
        ),
        uiOutput(ns("conference_filter_ui")),
        hr(),
        h6("2×2 Matrix Weighting"),
        sliderInput(
          ns("actual_weight"),
          "Actual vs. Expected Weight:",
          min = 0.0,
          max = 1.0,
          value = 0.60,
          step = 0.05
        ),
        helpText("1.0: Pure Scoreboard (WLD/PD)", br(), "0.0: Pure Play EPA (XWLD/XPD)"),
        sliderInput(
          ns("discrete_weight"),
          "Discrete vs. Margin Weight:",
          min = 0.0,
          max = 1.0,
          value = 0.50,
          step = 0.05
        ),
        helpText("1.0: Pure W/L Record (WLD/XWLD)", br(), "0.0: Pure Point Margin (PD/XPD)"),
        hr(),
        uiOutput(ns("derived_weights_summary"))
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "College Football 4-Pipeline Retrospective Rankings Table",
          span(class = "badge bg-primary", "Click a row to open Team Resume Card")
        ),
        card_body(
          DTOutput(ns("rankings_table"))
        )
      )
    )
  )
}

mod_rankings_server <- function(id, composite_data, on_select_team = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Dynamic conference filter
    output$conference_filter_ui <- renderUI({
      req(composite_data())
      confs <- sort(unique(na.omit(composite_data()$conference)))
      selectInput(
        ns("conf_filter"),
        "Conference:",
        choices = c("All Conferences" = "all", confs),
        selected = "all"
      )
    })

    # Summary of derived weights
    output$derived_weights_summary <- renderUI({
      w_act <- input$actual_weight
      w_exp <- 1 - w_act
      w_disc <- input$discrete_weight
      w_cont <- 1 - w_disc
      
      w1 <- round(w_act * w_disc * 100, 1) # WLD
      w2 <- round(w_act * w_cont * 100, 1) # PD
      w3 <- round(w_exp * w_disc * 100, 1) # XWLD
      w4 <- round(w_exp * w_cont * 100, 1) # XPD
      
      tagList(
        p(class = "text-muted small mb-1", strong("Current Pipeline Weights:")),
        tags$ul(class = "small text-muted ps-3 mb-0",
          tags$li(sprintf("Pipeline 1 (WLD): %s%%", w1)),
          tags$li(sprintf("Pipeline 2 (PD): %s%%", w2)),
          tags$li(sprintf("Pipeline 3 (XWLD): %s%%", w3)),
          tags$li(sprintf("Pipeline 4 (XPD): %s%%", w4))
        )
      )
    })
    
    # Recalculate composite dynamically based on user 2x2 matrix sliders
    recalculated_data <- reactive({
      req(composite_data())
      df <- composite_data()
      
      w_act <- input$actual_weight
      w_exp <- 1 - w_act
      w_disc <- input$discrete_weight
      w_cont <- 1 - w_disc
      
      w1 <- w_act * w_disc # WLD
      w2 <- w_act * w_cont # PD
      w3 <- w_exp * w_disc # XWLD
      w4 <- w_exp * w_cont # XPD
      
      # Ensure presence of transformed variables with fallbacks
      p_theta <- if ("phi_theta" %in% names(df)) df$phi_theta else stats::pnorm(scale(df$theta_median))
      p_delta <- if ("phi_delta" %in% names(df)) df$phi_delta else p_theta
      p_omega <- if ("phi_omega" %in% names(df)) df$phi_omega else p_theta
      p_alpha <- if ("phi_alpha" %in% names(df)) df$phi_alpha else stats::pnorm(scale(df$alpha_median))
      
      df |>
        dplyr::mutate(
          composite_dyn = (w1 * p_theta) + (w2 * p_delta) + (w3 * p_omega) + (w4 * p_alpha)
        ) |>
        dplyr::group_by(subdivision_id) |>
        dplyr::mutate(
          rank_display = dplyr::min_rank(desc(composite_dyn))
        ) |>
        dplyr::ungroup() |>
        dplyr::arrange(subdivision_id, rank_display)
    })
    
    # Filter by subdivision and conference
    filtered_data <- reactive({
      df <- recalculated_data()
      
      if (input$subdivision_filter == "fbs") {
        df <- df |> dplyr::filter(subdivision_id == 1L)
      } else if (input$subdivision_filter == "fcs") {
        df <- df |> dplyr::filter(subdivision_id == 2L)
      }
      
      if (!is.null(input$conf_filter) && input$conf_filter != "all") {
        df <- df |> dplyr::filter(conference == input$conf_filter)
      }
      
      df
    })
    
    output$rankings_table <- renderDT({
      df <- filtered_data()
      
      # Select display columns
      table_df <- df |>
        dplyr::transmute(
          Rank = rank_display,
          Team = team,
          Conference = conference,
          `Composite Index` = composite_dyn,
          `WLD (θ)` = rank_wld,
          `PD (δ)` = rank_pd,
          `XWLD (ω)` = rank_xwld,
          `XPD (α)` = rank_xpd,
          `Resume Luck (Δ)` = outcome_luck,
          `SB Score` = sb_score,
          `Buchholz SOS` = buchholz_sos
        )
      
      datatable(
        table_df,
        selection = "single",
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          dom = "ftipr"
        ),
        rownames = FALSE
      ) |>
        formatRound(columns = c("Composite Index"), digits = 3) |>
        formatRound(columns = c("SB Score", "Buchholz SOS"), digits = 2)
    })
    
    # Notify parent on row selection
    observeEvent(input$rankings_table_rows_selected, {
      selected_row <- input$rankings_table_rows_selected
      if (length(selected_row) > 0) {
        selected_team <- filtered_data()$team[selected_row]
        if (!is.null(on_select_team)) {
          on_select_team(selected_team)
        }
      }
    })
  })
}
