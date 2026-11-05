server <- function(input, output, session) {
  
  teams_data <- reactive({ get_teams() })
  team_names <- reactive({
    nm <- sort(teams_data()$TeamName)
    # only offer teams the 2.0 cache can predict
    if (!is.null(PREDICTION_CACHE_V2)) nm <- intersect(nm, unique(PREDICTION_CACHE_V2$df$team_a))
    nm
  })
  
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
    withProgress(message = "Running prediction...", {
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
    p   <- prediction()
    g   <- p$gnn
    aes <- get_team_aesthetics()
    wp_donut_plot(p$team_a, p$team_b, g$win_prob_a,
                  get_team_color(p$team_a, aes, primary = TRUE),
                  get_team_color(p$team_b, aes, primary = TRUE))
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
    
    aes <- get_team_aesthetics()
    stat_comparison_plot(stats_long, p$team_a, p$team_b,
                         get_team_color(p$team_a, aes, primary = TRUE),
                         get_team_color(p$team_b, aes, primary = TRUE))
  })
  
  # ── Update bet team choices after prediction ──────────────────────────────
  observeEvent(prediction(), {
    p <- prediction()
    updateSelectInput(session, "bet_team",
                      choices = c(p$team_a, p$team_b),
                      selected = if (p$gnn$pred_score_a > p$gnn$pred_score_b) p$team_a else p$team_b
    )
  })
  
  # Switching between opening and closing lines resets the trust sliders to the measured values
  observeEvent(input$line_type, {
    lt <- LINE_TYPES[[input$line_type]]
    updateSliderInput(session, "w_spread", value = round(100 * lt$weight_spread))
    updateSliderInput(session, "w_total",  value = round(100 * lt$weight_total))
  }, ignoreInit = TRUE)
  
  # ── Betting analysis (spread, moneyline, over/under) ───────────────────────
  observeEvent(input$analyze_bet_btn, {
    req(prediction(), input$bet_team, input$bet_spread, input$bet_moneyline, input$bet_total)
    p        <- prediction()
    g        <- p$gnn
    bet_team <- input$bet_team
    is_a     <- bet_team == p$team_a
    opp      <- if (is_a) p$team_b else p$team_a
    m_bet    <- if (is_a) g$pred_score_a - g$pred_score_b else g$pred_score_b - g$pred_score_a
    wp_bet   <- if (is_a) g$win_prob_a else g$win_prob_b
    t_model  <- g$pred_score_a + g$pred_score_b
    bank     <- if (is.null(input$bankroll) || is.na(input$bankroll)) 0 else input$bankroll
    w_s      <- input$w_spread / 100
    w_t      <- input$w_total / 100
    
    lt <- LINE_TYPES[[if (is.null(input$line_type)) "open" else input$line_type]]
    sp <- spread_bet(m_bet, input$bet_spread, input$bet_spread_odds, weight = w_s, sigma = lt$sigma_margin)
    ml <- moneyline_bet(wp_bet, input$bet_moneyline, weight = w_s)
    ou <- total_bet(t_model, input$bet_total, input$bet_total_odds, weight = w_t, sigma = lt$sigma_total)
    
    fmt_pts <- function(x) paste0(ifelse(x > 0, "+", ""), round(x, 1))
    fmt_odds <- function(x) paste0(ifelse(x > 0, "+", ""), x)
    box_for <- function(label, side, r, headline) {
      dollars <- if (bank > 0) paste0("$", format(round(bank * r$stake), big.mark = ",")) else NULL
      stats <- list(
        "Win chance" = scales::percent(r$p_win, 0.1),
        "Needed to break even" = paste0(scales::percent(r$breakeven, 0.1), " at ", fmt_odds(r$odds)),
        "Expected value" = paste0(ifelse(r$ev > 0, "+", ""), round(100 * r$ev, 1), "% per $1"),
        "Stake" = if (r$call == "bet")
          paste0(scales::percent(r$stake, 0.1), if (!is.null(dollars)) paste0(" (", dollars, ")") else "") else "none")
      bet_result_card(label, r$call, if (r$call == "none") "No edge" else side, stats, headline)
    }
    
    output$bet_analysis <- renderUI({
      tagList(
        div(class = "bet-section-title", style = "margin-top: 16px;", "3. Results"),
        fluidRow(
          column(4, box_for("Spread", bet_team, sp,
                            glue("Model: {bet_team} {fmt_pts(m_bet)} vs. {opp}; line {fmt_pts(input$bet_spread)}. ",
                                 "Raw edge {fmt_pts(sp$raw_edge)} pts."))),
          column(4, box_for("Moneyline", bet_team, ml,
                            glue("Model win prob {scales::percent(wp_bet, 0.1)} vs. market ",
                                 "{scales::percent(ml$market_p, 0.1)} (de-vigged)."))),
          column(4, box_for("Over / Under", paste(ou$side, input$bet_total), ou,
                            glue("Model total {round(t_model, 1)} vs. line {input$bet_total}. ",
                                 "Raw edge {fmt_pts(ou$raw_edge)} pts.")))
        ),
        div(
          class = "alert alert-secondary",
          style = "margin-top: 8px; font-size: 0.78rem;",
          icon("exclamation-triangle"),
          " Tested on one season and one sportsbook. Opening lines are available briefly and often with low ",
          "limits, and the model uses game-day data an early line could not have seen, so real results will be ",
          "worse than the backtest. Stakes use quarter-Kelly capped at 3% of bankroll. Not financial advice."
        )
      )
    })
  })
  
  # ── Today's picks: built live from ESPN lines; shared 10-minute memo so many viewers don't hammer ESPN ──
  picks_rv    <- reactiveVal(NULL)
  picks_error <- reactiveVal(NULL)
  
  load_picks <- function(force = FALSE) {
    fresh <- !is.null(PICKS_STATE$data) && difftime(Sys.time(), PICKS_STATE$time, units = "mins") < 10
    if (fresh && !force) { picks_rv(PICKS_STATE$data); picks_error(NULL); return(invisible()) }
    withProgress(message = "Pulling today's lines from ESPN...", value = 0, {
      res <- tryCatch(
        build_picks(progress = function(i, n) setProgress(i / n, detail = paste(i, "of", n, "games"))),
        error = function(e) e)
    })
    if (inherits(res, "error")) {
      picks_error(conditionMessage(res))
      # fall back to the last saved file, if there is one
      if (is.null(picks_rv()) && file.exists("data/picks.csv")) picks_rv(read.csv("data/picks.csv", stringsAsFactors = FALSE))
    } else {
      PICKS_STATE$data <- res; PICKS_STATE$time <- Sys.time()
      picks_rv(res); picks_error(NULL)
    }
  }
  
  # first time the tab is opened in a session, and whenever the button is pressed
  observeEvent(input$tabs, {
    if (identical(input$tabs, "picks") && is.null(picks_rv())) load_picks()
  })
  observeEvent(input$refresh_picks, load_picks(force = TRUE))
  
  picks_data <- reactive({ req(picks_rv()); picks_rv() })
  
  output$picks_header <- renderUI({
    err <- picks_error()
    d <- picks_rv()
    n_bet  <- if (is.null(d)) 0 else sum(d$spread_call == "BET", na.rm = TRUE) + sum(d$total_call == "BET", na.rm = TRUE)
    n_lean <- if (is.null(d)) 0 else sum(d$spread_call == "LEAN", na.rm = TRUE) + sum(d$total_call == "LEAN", na.rm = TRUE)
    tagList(
      if (!is.null(err))
        div(class = "alert alert-warning", style = "padding: 6px 10px; margin-bottom: 6px;",
            "Could not refresh: ", err, if (!is.null(d)) " Showing the last saved list."),
      if (!is.null(d))
        div(class = "picks-chips",
            span(class = "picks-chip chip-bet",  tags$b(n_bet),  "bets"),
            span(class = "picks-chip chip-lean", tags$b(n_lean), "leans"),
            span(class = "picks-chip", tags$b(nrow(d)), "games"),
            if (!is.null(attr(d, "stats")))
              span(class = "picks-updated", glue("{attr(d, 'stats')$board} on the board, {attr(d, 'stats')$with_line} with a sportsbook line")),
            span(class = "picks-updated", glue("Lines pulled {d$generated_at[1]}"))) else
        p(class = "text-muted", "Open this tab or press the button to pull today's lines.")
    )
  })
  
  output$picks_cards <- renderUI({
    d <- picks_data()
    if (is.null(d) || nrow(d) == 0) return(p(class = "text-muted", "No games with lines yet."))
    flt <- if (is.null(input$picks_filter)) "all" else input$picks_filter
    best <- pmax(d$spread_ev, d$total_ev, na.rm = TRUE)
    best[!is.finite(best)] <- -Inf
    d <- d[order(-best), , drop = FALSE]
    keep <- switch(flt,
      all   = rep(TRUE, nrow(d)),
      leans = d$spread_call %in% c("BET", "LEAN") | d$total_call %in% c("BET", "LEAN"),
      bets  = d$spread_call %in% "BET" | d$total_call %in% "BET")
    d <- d[keep %in% TRUE, , drop = FALSE]
    if (nrow(d) == 0) return(p(class = "text-muted", "No games match this filter."))
    tagList(lapply(seq_len(nrow(d)), function(i) pick_card(as.list(d[i, ]))))
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
  
  # ── Model Evaluation (walk-forward backtest, see lockbot2/README.md) ───────
  df_table <- function(df, digits = 2) {
    cell <- function(x) if (is.numeric(x)) format(round(x, digits), nsmall = digits) else as.character(x)
    tags$table(
      class = "table table-condensed",
      tags$thead(tags$tr(lapply(names(df), tags$th))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i)
        tags$tr(lapply(df[i, ], function(v) tags$td(cell(v))))))
    )
  }
  
  eval_data <- reactive({
    path <- "data/eval_v2.json"
    req(file.exists(path))
    jsonlite::fromJSON(path)
  })
  
  output$eval_summary <- renderUI({
    e   <- eval_data()
    wf  <- e$walk_forward
    sg  <- e$same_games_2026
    wp  <- e$winprob_2026$table
    inf <- e$information_test
    last <- wf[wf$season == max(wf$season), ]
    tagList(
      fluidRow(
        column(3, result_card("Winner Accuracy", paste0(last[["winner_acc%"]], "%"),
                              subtitle = glue("{last$season} season, never seen in training"),
                              color = "#4CAF50")),
        column(3, result_card("Margin MAE", glue("{round(last$margin_MAE, 2)} pts"),
                              subtitle = glue("LightGBM, all {format(last$n, big.mark = ',')} games of {last$season}"), color = "#FF9800")),
        column(3, result_card("Total MAE", glue("{round(last$total_MAE, 2)} pts"),
                              subtitle = "Same model and games", color = "#2196F3")),
        column(3, result_card("Games tested", format(last$n, big.mark = ","),
                              subtitle = glue("{min(wf$season)}-{max(wf$season)} walk-forward"),
                              color = "#9C27B0"))
      ),
      h4("Same games, 2026 (head to head)"),
      p(class = "text-muted",
        "Only games that have a DraftKings line (Nov 24 onward), so every row is scored on identical games. ",
        "Errors here are lower than the cards above, which also include the harder early-season games."),
      df_table(sg, 2),
      h4("Win probability quality (lower is better)"),
      df_table(wp, 4),
      p(class = "text-muted",
        glue("Does the model know something the closing line does not? Weight on model-minus-market: ",
             "{round(inf$b2, 2)} (t = {round(inf$t_b2, 1)}). That is suggestive, not significant. ",
             "Treat the model as a good independent estimate, not a proven edge."))
    )
  })
}
