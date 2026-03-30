server <- function(input, output, session) {
  
  teams_data <- reactive({ get_teams() })
  team_names <- reactive({ sort(teams_data()$TeamName) })
  
  output$team_picker_a <- renderUI({
    team_selector("team_a", "Team A", team_names())
  })
  output$team_picker_b <- renderUI({
    req(input$team_a)
    team_selector("team_b", "Team B", setdiff(team_names(), input$team_a))
  })
  
  # ── Prediction ─────────────────────────────────────────────────────────────
  prediction <- eventReactive(input$predict_btn, {
    req(input$team_a, input$team_b)
    withProgress(message = "Running GNN prediction...", {
      predict_game(input$team_a, input$team_b)
    })
  })
  
  output$prediction_results <- renderUI({
    p      <- prediction()
    g      <- p$gnn
    team_a <- p$team_a
    team_b <- p$team_b
    winner <- if (g$win_prob_a > 0.5) team_a else team_b
    conf   <- max(g$win_prob_a, g$win_prob_b)
    
    tagList(
      fluidRow(
        column(6, result_card("Predicted Winner", winner, color = "#4CAF50")),
        column(6, result_card("Confidence", scales::percent(conf, accuracy = 1),
                              subtitle = "GNN win probability", color = "#FF9800"))
      ),
      fluidRow(
        column(6, result_card(glue("{team_a} Score"),
                              round(g$pred_score_a, 1), color = "#2196F3")),
        column(6, result_card(glue("{team_b} Score"),
                              round(g$pred_score_b, 1), color = "#9C27B0"))
      ),
      if (!is.null(p$kenpom)) {
        km <- p$kenpom
        div(class = "alert alert-info", style = "margin-top: 12px;",
            icon("chart-line"), strong(" KenPom reference: "),
            glue("{km$Home} {round(km$HomePred, 1)} – ",
                 "{km$Visitor} {round(km$VisitorPred, 1)} ",
                 "(KenPom HomeWP: {scales::percent(km$HomeWP, accuracy=1)})")
        )
      }
    )
  })
  
  output$wp_gauge <- renderPlotly({
    p <- prediction()
    g <- p$gnn
    plot_ly(
      type  = "indicator",
      mode  = "gauge+number",
      value = round(g$win_prob_a * 100, 1),
      title = list(text = glue("{p$team_a} Win Probability (%)")),
      gauge = list(
        axis  = list(range = list(0, 100)),
        bar   = list(color = "#4CAF50"),
        steps = list(
          list(range = c(0,  40), color = "#ffcdd2"),
          list(range = c(40, 60), color = "#fff9c4"),
          list(range = c(60,100), color = "#c8e6c9")
        ),
        threshold = list(line = list(color = "red", width = 4), value = 50)
      )
    )
  })
  
  output$stat_comparison <- renderPlotly({
    req(input$team_a, input$team_b)
    ratings <- get_ratings() |>
      filter(TeamName %in% c(input$team_a, input$team_b)) |>
      select(TeamName, AdjOE, AdjDE, AdjTempo, Luck, AdjEM)
    
    plot_ly(
      ratings |> tidyr::pivot_longer(-TeamName, names_to = "Stat"),
      x = ~Stat, y = ~value, color = ~TeamName,
      type = "bar", barmode = "group"
    ) |>
      layout(yaxis = list(title = "Value"), xaxis = list(title = ""))
  })
  
  # ── Training ───────────────────────────────────────────────────────────────
  output$train_log <- renderText("Click 'Train Model' to begin.")
  
  observeEvent(input$train_btn, {
    seasons   <- as.integer(input$train_seasons)
    half_life <- as.numeric(input$half_life)
    
    if (length(seasons) == 0) {
      output$train_log <- renderText("Select at least one season.")
      return()
    }
    
    output$train_log <- renderText(
      glue("Training on seasons: {paste(seasons, collapse=', ')} | ",
           "half-life: {half_life} days\nCheck R console for epoch-by-epoch progress...")
    )
    
    tryCatch({
      run_training(seasons = seasons, half_life = half_life)
      output$train_log <- renderText(
        glue("✅ Training complete.\n",
             "Seasons: {paste(seasons, collapse=', ')}\n",
             "Half-life: {half_life} days\n",
             "Model saved to data/models/")
      )
    }, error = function(e) {
      output$train_log <- renderText(paste("❌ Error:", e$message))
    })
  })
  
  # ── Stats table ────────────────────────────────────────────────────────────
  output$ratings_table <- renderDT({
    get_ratings() |>
      select(TeamName, AdjEM, RankAdjEM, AdjOE, AdjDE,
             AdjTempo, Luck, SOS, Pythag) |>
      arrange(RankAdjEM) |>
      datatable(options = list(pageLength = 25, scrollX = TRUE),
                rownames = FALSE)
  })
}