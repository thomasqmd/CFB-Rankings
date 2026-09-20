# ==============================================================================
# Shiny Module: mod_team_card.R
# Purpose: Deep-dive resume card showing 4-pipeline breakdown, luck metrics, and schedule
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

mod_team_card_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Team Selector",
        width = 280,
        uiOutput(ns("team_selector_ui"))
      ),
      tagList(
        # Headline value cards: Composite & Schedule Strength
        layout_column_wrap(
          width = 1/4,
          value_box(
            title = "Composite Rank",
            value = textOutput(ns("team_rank")),
            showcase = bsicons::bs_icon("trophy-fill"),
            theme = "primary"
          ),
          value_box(
            title = "Sonneborn-Berger Score",
            value = textOutput(ns("team_sb")),
            showcase = bsicons::bs_icon("graph-up"),
            theme = "success"
          ),
          value_box(
            title = "Schedule Strength (Buchholz)",
            value = textOutput(ns("team_buchholz")),
            showcase = bsicons::bs_icon("shield-shaded"),
            theme = "info"
          ),
          value_box(
            title = "Resume Luck (Δ)",
            value = textOutput(ns("team_luck")),
            showcase = bsicons::bs_icon("dice-5"),
            theme = "secondary"
          )
        ),

        # 4-Pipeline Performance Pillars
        layout_column_wrap(
          width = 1/4,
          value_box(
            title = "Pipeline 1: WLD (θ)",
            value = textOutput(ns("team_wld")),
            showcase = bsicons::bs_icon("award"),
            theme = "light"
          ),
          value_box(
            title = "Pipeline 2: PD (δ)",
            value = textOutput(ns("team_pd")),
            showcase = bsicons::bs_icon("calculator"),
            theme = "light"
          ),
          value_box(
            title = "Pipeline 3: XWLD (ω)",
            value = textOutput(ns("team_xwld")),
            showcase = bsicons::bs_icon("magic"),
            theme = "light"
          ),
          value_box(
            title = "Pipeline 4: XPD (α)",
            value = textOutput(ns("team_xpd")),
            showcase = bsicons::bs_icon("speedometer2"),
            theme = "light"
          )
        ),
        
        # Schedule equity plot and game log
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Schedule Opponent Quality & Outcome Points"),
            card_body(
              plotlyOutput(ns("schedule_plot"), height = "350px")
            )
          ),
          card(
            card_header("Game-by-Game Resume Breakdown"),
            card_body(
              DTOutput(ns("game_log_table"))
            )
          )
        )
      )
    )
  )
}

mod_team_card_server <- function(id, composite_data, games_data, selected_team_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Team dropdown selector
    output$team_selector_ui <- renderUI({
      req(composite_data())
      teams <- sort(unique(composite_data()$team))
      curr_sel <- if (!is.null(selected_team_reactive()) && selected_team_reactive() %in% teams) {
        selected_team_reactive()
      } else {
        teams[1]
      }
      selectInput(ns("chosen_team"), "Select Team:", choices = teams, selected = curr_sel)
    })
    
    # Observe external team selection updates
    observeEvent(selected_team_reactive(), {
      req(selected_team_reactive())
      updateSelectInput(session, "chosen_team", selected = selected_team_reactive())
    })
    
    # Target team summary record
    target_summary <- reactive({
      req(composite_data(), input$chosen_team)
      composite_data() |> dplyr::filter(team == input$chosen_team)
    })
    
    output$team_rank <- renderText({
      req(target_summary())
      paste0("#", target_summary()$rank_composite[1])
    })
    
    output$team_sb <- renderText({
      req(target_summary())
      sprintf("%.2f", target_summary()$sb_score[1])
    })
    
    output$team_buchholz <- renderText({
      req(target_summary())
      sprintf("%.2f", target_summary()$buchholz_sos[1])
    })

    output$team_luck <- renderText({
      req(target_summary())
      luck <- target_summary()$outcome_luck[1]
      if (is.null(luck) || is.na(luck)) "0" else sprintf("%+d ranks", luck)
    })

    output$team_wld <- renderText({
      req(target_summary())
      r <- target_summary()$rank_wld[1]
      paste0("#", r, " (θ = ", round(target_summary()$theta_median[1], 2), ")")
    })

    output$team_pd <- renderText({
      req(target_summary())
      r <- target_summary()$rank_pd[1]
      paste0("#", r, " (δ = ", round(target_summary()$delta_median[1], 1), " pts)")
    })

    output$team_xwld <- renderText({
      req(target_summary())
      r <- target_summary()$rank_xwld[1]
      paste0("#", r, " (ω = ", round(target_summary()$omega_median[1], 2), ")")
    })

    output$team_xpd <- renderText({
      req(target_summary())
      r <- target_summary()$rank_xpd[1]
      paste0("#", r, " (α = ", round(target_summary()$alpha_median[1], 1), " pts)")
    })
    
    # Team games
    team_games <- reactive({
      req(games_data(), input$chosen_team, composite_data())
      g <- games_data()
      t_name <- input$chosen_team
      
      # Filter games involving team
      t_games <- g |>
        dplyr::filter(home_team == t_name | away_team == t_name) |>
        dplyr::mutate(
          is_home = (home_team == t_name),
          opponent = ifelse(is_home, away_team, home_team),
          team_pts = ifelse(is_home, home_points, away_points),
          opp_pts = ifelse(is_home, away_points, home_points),
          pts_earned = dplyr::case_when(
            outcome == 4L & is_home  ~ 3,
            outcome == 3L & is_home  ~ 2,
            outcome == 2L & is_home  ~ 1,
            outcome == 1L & is_home  ~ 0,
            outcome == 1L & !is_home ~ 3,
            outcome == 2L & !is_home ~ 2,
            outcome == 3L & !is_home ~ 1,
            outcome == 4L & !is_home ~ 0,
            TRUE ~ 0
          ),
          result_label = dplyr::case_when(
            pts_earned == 3 ~ "W",
            pts_earned == 2 ~ "OTW",
            pts_earned == 1 ~ "OTL",
            pts_earned == 0 ~ "L"
          )
        ) |>
        dplyr::left_join(
          composite_data() |> dplyr::select(opponent = team, opp_theta = theta_median),
          by = "opponent"
        ) |>
        dplyr::mutate(
          opp_quality = stats::plogis(coalesce(opp_theta, 0))
        )
      
      t_games
    })
    
    output$schedule_plot <- renderPlotly({
      req(team_games())
      tg <- team_games()
      
      p <- ggplot(tg, aes(x = factor(week), y = opp_quality, fill = result_label,
                          text = paste0("Opponent: ", opponent, "\nResult: ", result_label, " (", pts_earned, " pts)\nScore: ", team_pts, "-", opp_pts))) +
        geom_col(width = 0.6) +
        scale_fill_manual(
          values = c("W" = "#228833", "OTW" = "#66CCEE", "OTL" = "#CCBB44", "L" = "#EE6677")
        ) +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        labs(
          x = "Week",
          y = "Opponent Quality (M(θ))",
          fill = "Outcome"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
    })
    
    output$game_log_table <- renderDT({
      req(team_games())
      df <- team_games() |>
        dplyr::select(
          Week = week,
          Opponent = opponent,
          Venue = is_home,
          Score = team_pts,
          OppScore = opp_pts,
          Result = result_label,
          Points = pts_earned
        ) |>
        dplyr::mutate(
          Venue = ifelse(Venue, "Home", "Away"),
          ScoreLine = paste0(Score, " - ", OppScore)
        ) |>
        dplyr::select(Week, Opponent, Venue, Score = ScoreLine, Result, `Points Earned` = Points)
      
      datatable(
        df,
        options = list(pageLength = 15, dom = "t"),
        rownames = FALSE
      )
    })
  })
}
