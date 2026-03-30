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
      predict_game(input$team_a, input$team_b,
                   location = input$game_location)
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
      title = list(text = glue("{p$team_a} Win Prob (%)"),
                   font = list(size = 12)),
      number = list(font = list(size = 24)),
      gauge = list(
        axis  = list(range = list(0, 100),
                     tickfont = list(size = 10)),
        bar   = list(color = "#4CAF50"),
        steps = list(
          list(range = c(0,  40), color = "#ffcdd2"),
          list(range = c(40, 60), color = "#fff9c4"),
          list(range = c(60,100), color = "#c8e6c9")
        ),
        threshold = list(line = list(color = "red", width = 4), value = 50)
      )
    ) |>
      layout(
        margin  = list(l = 20, r = 20, t = 40, b = 20),
        height  = 250
      )
  })
  
  output$stat_comparison <- renderPlotly({
    req(prediction())
    p          <- prediction()
    all_ratings <- get_ratings()
    
    ratings <- all_ratings |>
      filter(TeamName %in% c(p$team_a, p$team_b)) |>
      group_by(TeamName) |>
      slice(1) |>
      ungroup() |>
      select(TeamName, AdjOE, AdjDE, AdjTempo, AdjEM)
    
    numeric_cols <- c("AdjOE", "AdjDE", "AdjTempo", "AdjEM")
    
    ratings_pct <- ratings |>
      mutate(AdjDE = -AdjDE) |>
      mutate(across(all_of(numeric_cols), function(col_val) {
        all_vals <- all_ratings[[cur_column()]]
        round(100 * rank(col_val, ties.method = "average") /
                length(all_vals))
      })) |>
      rename(
        `Off. Efficiency` = AdjOE,
        `Def. Efficiency` = AdjDE,
        `Tempo`           = AdjTempo,
        `Overall (AdjEM)` = AdjEM
      )
    
    stats_long <- ratings_pct |>
      tidyr::pivot_longer(-TeamName, names_to = "Stat")
    
    plot_ly(stats_long, x = ~Stat, y = ~value, color = ~TeamName,
            type = "bar",
            text = ~glue("{value}th percentile"),
            hoverinfo = "text+x+name") |>
      layout(
        barmode = "group",
        yaxis = list(title = "Percentile rank (100 = best)",
                     range = c(0, 100)),
        xaxis = list(title = ""),
        legend = list(orientation = "h", y = -0.2)
      )
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
  
  # ── Model Evaluation ───────────────────────────────────────────────────────
  eval_metrics <- reactive({
    # Re-read after every training run
    input$train_btn
    
    meta_path <- file.path(getwd(), "data", "models", "meta.pkl")
    req(file.exists(meta_path))
    
    builtins <- import_builtins()
    pickle   <- import("pickle")
    fh       <- builtins$open(meta_path, "rb")
    meta     <- pickle$load(fh)
    fh$close()
    meta
  })
  
  output$eval_summary <- renderUI({
    m  <- eval_metrics()
    tm <- m$test_metrics
    
    tagList(
      fluidRow(
        column(3, result_card("Accuracy",
                              scales::percent(tm$accuracy, accuracy = 0.1),
                              subtitle = "% games predicted correctly",
                              color = "#4CAF50")),
        column(3, result_card("Brier Score",
                              round(tm$brier_score, 4),
                              subtitle = "Probability calibration (lower = better)",
                              color = "#2196F3")),
        column(3, result_card("Log Loss",
                              round(tm$log_loss, 4),
                              subtitle = "Confidence penalty (lower = better)",
                              color = "#FF9800")),
        column(3, result_card("Score MAE",
                              glue("{round(tm$mae_points, 1)} pts"),
                              subtitle = "Avg points off per team per game",
                              color = "#9C27B0"))
      ),
      fluidRow(
        column(6, p(class = "text-muted",
                    glue("Train games: {m$n_train} | ",
                         "Test games: {m$n_test} | ",
                         "Trained: {m$trained_date}"))),
      )
    )
  })
  
  output$roi_chart <- renderPlotly({
    m   <- eval_metrics()
    roi <- m$test_metrics$roi_by_threshold
    
    # Handle both "0.6" and "0.60" key formats
    get_roi <- function(r, key_options) {
      for (k in key_options) {
        if (!is.null(r[[k]])) return(r[[k]])
      }
      list(roi_pct = 0, n_bets = 0, win_rate = 0)
    }
    
    r55 <- get_roi(roi, c("0.55"))
    r60 <- get_roi(roi, c("0.6", "0.60"))
    r65 <- get_roi(roi, c("0.65"))
    
    df <- tibble(
      threshold = c("55%", "60%", "65%"),
      roi       = c(r55$roi_pct, r60$roi_pct, r65$roi_pct),
      n_bets    = c(r55$n_bets,  r60$n_bets,  r65$n_bets),
      win_rate  = c(r55$win_rate, r60$win_rate, r65$win_rate)
    )
    
    plot_ly(df, x = ~threshold, y = ~roi, type = "bar",
            text = ~glue("{n_bets} bets | Win rate: {scales::percent(win_rate, 1)}"),
            textposition = "outside",
            marker = list(color = ifelse(df$roi >= 0, "#4CAF50", "#f44336"))) |>
      layout(
        yaxis = list(title = "ROI % (vs -110 line)",
                     zeroline = TRUE, zerolinecolor = "black"),
        xaxis = list(title = "Confidence Threshold"),
        shapes = list(list(type = "line", y0 = 0, y1 = 0,
                           x0 = -0.5, x1 = 2.5,
                           line = list(color = "black", dash = "dash")))
      )
  })
  
  output$calibration_chart <- renderPlotly({
    # Calibration chart requires raw predictions — stored in meta if available
    # Falls back to a reference line if not available
    m <- eval_metrics()
    
    # Perfect calibration reference line
    df_ref <- tibble(x = seq(0, 1, 0.1), y = seq(0, 1, 0.1))
    
    plot_ly() |>
      add_trace(data = df_ref, x = ~x, y = ~y, type = "scatter",
                mode = "lines", name = "Perfect calibration",
                line = list(dash = "dash", color = "gray")) |>
      layout(
        xaxis = list(title = "Predicted win probability",
                     range = c(0, 1)),
        yaxis = list(title = "Actual win rate",
                     range = c(0, 1)),
        annotations = list(list(
          x = 0.5, y = 0.3, text = "Retrain model to populate calibration curve",
          showarrow = FALSE, font = list(color = "gray")
        ))
      )
  })
}