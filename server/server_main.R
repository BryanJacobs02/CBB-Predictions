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
    winner <- if (g$pred_score_a > g$pred_score_b) team_a else team_b
    margin <- abs(g$pred_score_a - g$pred_score_b)
    wp     <- if (winner == team_a) g$win_prob_a else g$win_prob_b
    
    # Get team aesthetics
    aesthetics <- get_team_aesthetics()
    logo_a  <- get_team_logo(team_a, aesthetics)
    logo_b  <- get_team_logo(team_b, aesthetics)
    color_a <- get_team_color(team_a, aesthetics, primary = TRUE)
    color_b <- get_team_color(team_b, aesthetics, primary = TRUE)
    
    tagList(
      # Matchup header with logos
      matchup_header(team_a, team_b, logo_a, logo_b, color_a, color_b),
      
      # Score prediction
      fluidRow(
        column(4, result_card(
          glue("{team_a} Score"),
          round(g$pred_score_a, 1),
          color = color_a
        )),
        column(4, result_card(
          "Predicted Margin",
          glue("+{round(margin, 1)}"),
          subtitle = glue("in favor of {winner}"),
          color = "#58a6ff"
        )),
        column(4, result_card(
          glue("{team_b} Score"),
          round(g$pred_score_b, 1),
          color = color_b
        ))
      ),
      
      # Winner and implied probability
      fluidRow(
        column(6, result_card(
          "Predicted Winner",
          winner,
          subtitle = glue("{scales::percent(wp, accuracy=1)} implied win probability"),
          color = "#3fb950"
        )),
        column(6, result_card(
          "Predicted Total",
          round(g$pred_score_a + g$pred_score_b, 1),
          subtitle = "combined projected points",
          color = "#d29922"
        ))
      ),
      
      # KenPom reference
      if (!is.null(p$kenpom)) {
        km <- p$kenpom
        div(
          class = "alert alert-secondary",
          style = "margin-top: 8px;",
          icon("chart-line"),
          strong(" KenPom reference: "),
          glue("{km$Home} {round(km$HomePred, 1)} – ",
               "{km$Visitor} {round(km$VisitorPred, 1)}")
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
      title = list(text = glue("{p$team_a} Win Prob (%) — derived from margin"),
                   font = list(size = 11)),
      number = list(font = list(size = 24)),
      gauge = list(
        axis  = list(range = list(0, 100), tickfont = list(size = 10)),
        bar   = list(color = "#4CAF50"),
        steps = list(
          list(range = c(0,  40), color = "#ffcdd2"),
          list(range = c(40, 60), color = "#fff9c4"),
          list(range = c(60,100), color = "#c8e6c9")
        ),
        threshold = list(line = list(color = "red", width = 4), value = 50)
      )
    ) |>
      layout(margin = list(l = 20, r = 20, t = 50, b = 20), height = 250)
  })
  
  output$stat_comparison <- renderPlotly({
    req(prediction())
    p <- prediction()
    
    all_ratings <- get_ratings() |>
      group_by(TeamName) |>
      slice(1) |>
      ungroup()
    
    ratings <- all_ratings |>
      filter(TeamName %in% c(p$team_a, p$team_b))
    
    if (nrow(ratings) < 2) return(plot_ly() |>
                                    layout(title = "Team data not found"))
    
    # Compute percentiles manually — no cur_column() confusion
    pct <- function(team_val, all_vals, higher_is_better = TRUE) {
      if (!higher_is_better) {
        team_val <- -team_val
        all_vals <- -all_vals
      }
      round(100 * sum(all_vals <= team_val, na.rm = TRUE) / length(all_vals))
    }
    
    rows <- list()
    for (tn in c(p$team_a, p$team_b)) {
      r <- ratings |> filter(TeamName == tn)
      rows[[tn]] <- tibble(
        TeamName = tn,
        Stat     = c("Off. Efficiency", "Def. Efficiency", "Tempo", "Overall (AdjEM)"),
        value    = c(
          pct(r$AdjOE,  all_ratings$AdjOE,  TRUE),
          pct(r$AdjDE,  all_ratings$AdjDE,  FALSE),  # lower AdjDE = better defense
          pct(r$AdjTempo, all_ratings$AdjTempo, TRUE),
          pct(r$AdjEM,  all_ratings$AdjEM,  TRUE)
        )
      )
    }
    
    stats_long <- bind_rows(rows)
    
    plot_ly(stats_long, x = ~Stat, y = ~value, color = ~TeamName,
            type = "bar",
            text = ~glue("{value}th percentile"),
            hoverinfo = "text+x+name") |>
      layout(
        barmode = "group",
        title   = glue("{p$team_a} vs {p$team_b}"),
        yaxis   = list(title = "Percentile rank (100 = best)",
                       range = c(0, 100)),
        xaxis   = list(title = ""),
        legend  = list(orientation = "h", y = -0.2)
      )
  })
  
  # ── Update bet team choices after prediction ──────────────────────────────
  observeEvent(prediction(), {
    p <- prediction()
    updateSelectInput(session, "bet_team",
                      choices = c(p$team_a, p$team_b),
                      selected = if (p$gnn$pred_score_a > p$gnn$pred_score_b) p$team_a else p$team_b
    )
  })
  
  # ── Betting analysis ───────────────────────────────────────────────────────
  observeEvent(input$analyze_bet_btn, {
    req(prediction(), input$bet_team, input$bet_spread, input$bet_moneyline)
    p          <- prediction()
    g          <- p$gnn
    bet_team   <- input$bet_team
    spread     <- input$bet_spread
    moneyline  <- input$bet_moneyline
    
    # Determine if bet team is home (team_a) or away (team_b)
    is_home    <- bet_team == p$team_a
    pred_score <- if (is_home) g$pred_score_a else g$pred_score_b
    opp_score  <- if (is_home) g$pred_score_b else g$pred_score_a
    pred_margin <- pred_score - opp_score  # positive = bet team wins
    
    # ── Spread analysis ──────────────────────────────────────────────────────
    # Negative spread means bet team is favorite (must win by more than |spread|)
    # Positive spread means bet team is underdog (can lose by less than spread)
    spread_cover <- pred_margin > -spread  # covers if margin beats the spread
    spread_diff  <- pred_margin - (-spread)  # how much better/worse than spread
    
    spread_class   <- if (spread_cover) "yes" else "no"
    spread_verdict <- if (spread_cover) "COVER ✓" else "NO COVER ✗"
    spread_detail  <- glue(
      "Model predicts {bet_team} by {round(pred_margin, 1)} pts. ",
      "Spread: {ifelse(spread > 0, '+', '')}{spread}. ",
      "Edge: {ifelse(spread_diff > 0, '+', '')}{round(spread_diff, 1)} pts"
    )
    
    # ── Moneyline analysis ───────────────────────────────────────────────────
    # Convert moneyline to implied probability
    ml_implied_prob <- if (moneyline < 0) {
      abs(moneyline) / (abs(moneyline) + 100)
    } else {
      100 / (moneyline + 100)
    }
    
    model_win_prob <- if (is_home) g$win_prob_a else g$win_prob_b
    edge <- model_win_prob - ml_implied_prob
    
    # Kelly criterion for bet sizing suggestion
    # Kelly fraction = (bp - q) / b where b = decimal odds - 1
    decimal_odds <- if (moneyline < 0) {
      100 / abs(moneyline) + 1
    } else {
      moneyline / 100 + 1
    }
    b <- decimal_odds - 1
    kelly_fraction <- (b * model_win_prob - (1 - model_win_prob)) / b
    kelly_pct <- max(0, kelly_fraction) * 100
    
    ml_class   <- if (edge > 0.03) "yes" else if (edge > 0) "neutral" else "no"
    ml_verdict <- if (edge > 0.03) "VALUE ✓" else if (edge > 0) "SLIGHT EDGE" else "NO VALUE ✗"
    ml_detail  <- glue(
      "Model win prob: {scales::percent(model_win_prob, 0.1)}. ",
      "Line implied: {scales::percent(ml_implied_prob, 0.1)}. ",
      "Edge: {ifelse(edge > 0, '+', '')}{scales::percent(edge, 0.1)}. ",
      "{if (kelly_pct > 0) glue('Kelly: {round(kelly_pct, 1)}% of bankroll') ",
      "else 'Kelly: no bet'}"
    )
    
    output$bet_analysis <- renderUI({
      tagList(
        hr(style = "border-color: var(--border); margin: 16px 0;"),
        fluidRow(
          column(6, bet_box("Spread Analysis", spread_verdict,
                            spread_detail, spread_class)),
          column(6, bet_box("Moneyline Analysis", ml_verdict,
                            ml_detail, ml_class))
        ),
        div(
          class = "alert alert-secondary",
          style = "margin-top: 8px; font-size: 0.78rem;",
          icon("exclamation-triangle"),
          " Model MAE is ~8.7 pts. Use as one input among many — not financial advice."
        )
      )
    })
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
      run_training(seasons = seasons, half_life = half_life,
                   full_train = input$full_train)
      
      output$train_log <- renderText(
        glue("✅ Training complete. Generating prediction cache...\n",
             "This will take 20-30 minutes for all D1 matchups.")
      )
      
      output$train_log <- renderText(
        glue("✅ Training and cache complete.\n",
             "Seasons: {paste(seasons, collapse=', ')}\n",
             "Half-life: {half_life} days\n",
             "Cached {length(PREDICTION_CACHE)} matchups.\n",
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
        column(3, result_card("Winner Accuracy",
                              scales::percent(tm$accuracy, accuracy = 0.1),
                              subtitle = "Derived from predicted margin",
                              color = "#4CAF50")),
        column(3, result_card("Score MAE",
                              glue("{round(tm$mae_points, 2)} pts"),
                              subtitle = "Avg error per team per game",
                              color = "#2196F3")),
        column(3, result_card("Margin MAE",
                              glue("{round(tm$mae_margin, 2)} pts"),
                              subtitle = "Avg error in point differential",
                              color = "#FF9800")),
        column(3, result_card("RMSE",
                              glue("{round(tm$rmse_points, 2)} pts"),
                              subtitle = "Penalizes large misses more",
                              color = "#9C27B0"))
      ),
      fluidRow(
        column(6, p(class = "text-muted",
                    glue("Train: {m$n_train} | Test: {m$n_test} | ",
                         "Trained: {m$trained_date}")))
      )
    )
  })
}