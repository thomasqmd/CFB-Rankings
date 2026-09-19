# ==============================================================================
# Shiny Module: mod_benchmarks.R
# Purpose: Benchmark comparisons against AP Top 25 and Colley Matrix with quadrant divergence
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

mod_benchmarks_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Benchmark Settings",
        width = 280,
        selectInput(
          ns("benchmark_target"),
          "Compare Bayes Retro Against:",
          choices = c("AP Top 25" = "ap", "Colley Matrix" = "colley"),
          selected = "ap"
        ),
        checkboxInput(ns("highlight_top25"), "Restrict to Top 25 Teams", value = TRUE),
        helpText("Points above the line indicate teams where our Bayesian model ranks the team higher than the consensus benchmark.")
      ),
      tagList(
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Rank Divergence Scatter Plot"),
            card_body(
              plotlyOutput(ns("scatter_plot"), height = "450px")
            )
          ),
          card(
            card_header("Benchmark Correlation & Overlap Summary"),
            card_body(
              DTOutput(ns("metrics_table")),
              hr(),
              h5("Top 5 Consensus Disagreements"),
              DTOutput(ns("outliers_table"))
            )
          )
        )
      )
    )
  )
}

mod_benchmarks_server <- function(id, benchmark_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    comp_df <- reactive({
      req(benchmark_data())
      benchmark_data()$comparison_table
    })
    
    output$scatter_plot <- renderPlotly({
      req(comp_df())
      df <- comp_df()
      is_ap <- input$benchmark_target == "ap"
      
      if (input$highlight_top25) {
        df <- if (is_ap) {
          df |> dplyr::filter(rank_composite <= 25 | (!is.na(rank_ap) & rank_ap <= 25))
        } else {
          df |> dplyr::filter(rank_composite <= 25 | rank_colley <= 25)
        }
      }
      
      y_col <- if (is_ap) "rank_ap" else "rank_colley"
      y_label <- if (is_ap) "AP Top 25 Rank" else "Colley Matrix Rank"
      
      df$y_val <- df[[y_col]]
      df <- df |> dplyr::filter(!is.na(y_val))
      
      max_limit <- max(c(df$rank_composite, df$y_val), na.rm = TRUE) + 2
      
      p <- ggplot(df, aes(x = rank_composite, y = y_val,
                          text = paste0("Team: ", team, 
                                        "\nBayes Rank: #", rank_composite, 
                                        "\n", y_label, ": #", y_val,
                                        "\nNet EPA: ", round(alpha_median, 1)))) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#BBBBBB") +
        geom_point(aes(color = conference), size = 3, alpha = 0.85) +
        scale_x_reverse(limits = c(max_limit, 1)) +
        scale_y_reverse(limits = c(max_limit, 1)) +
        labs(
          x = "Bayesian Retrospective Rank",
          y = y_label,
          color = "Conference"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    output$metrics_table <- renderDT({
      req(benchmark_data())
      datatable(
        benchmark_data()$benchmark_metrics,
        options = list(dom = "t"),
        rownames = FALSE
      ) |>
        formatRound(columns = c("Spearman_Rho", "Kendall_Tau", "Top25_Jaccard"), digits = 3)
    })
    
    output$outliers_table <- renderDT({
      req(benchmark_data())
      outliers <- if (input$benchmark_target == "ap") {
        benchmark_data()$most_underrated |>
          dplyr::select(Team = team, `Bayes Rank` = rank_composite, `AP Rank` = rank_ap, `Delta (+ spots)` = ap_delta)
      } else {
        benchmark_data()$comparison_table |>
          dplyr::mutate(colley_delta = rank_colley - rank_composite) |>
          dplyr::arrange(desc(abs(colley_delta))) |>
          dplyr::slice(1:5) |>
          dplyr::select(Team = team, `Bayes Rank` = rank_composite, `Colley Rank` = rank_colley, `Delta` = colley_delta)
      }
      
      datatable(
        outliers,
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
  })
}
