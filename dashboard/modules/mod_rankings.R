# ==============================================================================
# Shiny Module: mod_rankings.R
# Purpose: Interactive, filterable rankings table with dynamic lambda slider
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
        title = "Ranking Controls",
        width = 300,
        selectInput(
          ns("subdivision_filter"),
          "Subdivision:",
          choices = c("FBS Only" = "fbs", "FCS Only" = "fcs", "All Programs" = "all"),
          selected = "fbs"
        ),
        uiOutput(ns("conference_filter_ui")),
        sliderInput(
          ns("lambda_weight"),
          "Composite Outcome Weight (λ):",
          min = 0.0,
          max = 1.0,
          value = 0.65,
          step = 0.05
        ),
        helpText("λ = 1.0: Pure WLD Resume", br(), "λ = 0.0: Pure EPA Efficiency", br(), "Default: 0.65")
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "College Football Retrospective Rankings Table",
          span(class = "badge bg-primary", "Select a team to view detailed resume card")
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
    
    # Recalculate composite dynamically based on user lambda slider
    recalculated_data <- reactive({
      req(composite_data())
      df <- composite_data()
      lam <- input$lambda_weight
      
      df |>
        dplyr::mutate(
          composite_dyn = lam * phi_theta + (1 - lam) * phi_alpha
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
      df <- filtered_data() |>
        dplyr::select(
          Rank = rank_display,
          Team = team,
          Conference = conference,
          `Composite Index` = composite_dyn,
          `SB Score` = sb_score,
          `Buchholz SOS` = buchholz_sos,
          `Resume (θ)` = theta_median,
          `Net EPA (α)` = alpha_median
        )
      
      datatable(
        df,
        selection = "single",
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          dom = "ftipr"
        ),
        rownames = FALSE
      ) |>
        formatRound(columns = c("Composite Index"), digits = 3) |>
        formatRound(columns = c("SB Score", "Buchholz SOS", "Resume (θ)", "Net EPA (α)"), digits = 2)
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
