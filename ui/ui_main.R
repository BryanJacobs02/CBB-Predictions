ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      style = "font-family: 'Barlow Condensed', sans-serif;
               font-weight: 900;
               font-size: 3rem;
               letter-spacing: 0.12em;
               text-transform: uppercase;
               color: #58a6ff;",
      "LockBot 2.0"
    ),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    do.call(sidebarMenu, Filter(Negate(is.null), list(
      menuItem("Predict Matchup",  tabName = "predict", icon = icon("basketball-ball")),
      menuItem("Team Stats",       tabName = "stats",   icon = icon("table")),
      menuItem("Model Evaluation", tabName = "eval",    icon = icon("chart-line"))
    )))
  ),
  
  dashboardBody(
    tags$head(tags$title("LockBot 2.0"),
              tags$script("document.title = 'LockBot 2.0';"),
              tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
              tags$meta(property = "og:title",       content = "LockBot 2.0"),
              tags$meta(property = "og:description", content = "NCAA Basketball Score Predictor powered by gradient-boosted models on KenPom and box-score form"),
              tags$meta(property = "og:image",       content = "https://bryanjacobs.shinyapps.io/lockbot/preview.png"),
              tags$meta(property = "og:type",        content = "website"),
              tags$meta(name = "twitter:card",       content = "summary_large_image"),
              tags$meta(name = "twitter:image",      content = "https://bryanjacobs.shinyapps.io/lockbot/preview.png")),
    
    do.call(tabItems, Filter(Negate(is.null), list(
      
      # ── Predict ─────────────────────────────────────────────────────────────
      tabItem("predict",
              fluidRow(
                box(width = 4, title = "Select Teams", status = "primary",
                    uiOutput("team_picker_a"),
                    uiOutput("team_picker_b"),
                    radioButtons("game_location", "Game Location",
                                 choices = c(
                                   "Team A is Home" = "a_home",
                                   "Team B is Home" = "b_home",
                                   "Neutral Site"   = "neutral"
                                 ),
                                 selected = "neutral"
                    ),
                    actionButton("predict_btn", "Predict Outcome",
                                 class = "btn-primary btn-block", icon = icon("play"))
                ),
                box(width = 8, title = "Prediction Results", status = "success",
                    uiOutput("prediction_results")
                )
              ),
              fluidRow(
                box(width = 6, title = "Win Probability",     plotlyOutput("wp_gauge")),
                box(width = 6, title = "Key Stat Comparison", plotlyOutput("stat_comparison"))
              ),
              fluidRow(
                box(width = 12, title = "Betting Analysis", status = "info",
                    p(class = "text-muted", style = "font-size: 0.82rem;",
                      "Enter the lines from your sportsbook. Edges are shrunk toward the market line. ",
                      "In the 2026 backtest the model beat the OPENING lines (the market later moved toward it) ",
                      "but not the closing lines, so pick which one you are looking at."),
                    radioButtons("line_type", "Which line are you looking at?",
                                 choices = c("Opening line (bet early)" = "open",
                                             "Current / closing line"   = "close"),
                                 selected = "open", inline = TRUE),
                    fluidRow(
                      column(3, selectInput("bet_team", "Team (spread & moneyline)",
                                            choices = c("Select after predicting" = ""), width = "100%")),
                      column(3, numericInput("bet_spread", "Spread for that team (-3.5 = favorite)",
                                             value = -3.5, step = 0.5, width = "100%")),
                      column(3, numericInput("bet_spread_odds", "Spread price", value = -110, step = 5, width = "100%")),
                      column(3, numericInput("bet_moneyline", "Moneyline for that team", value = -150, step = 5, width = "100%"))
                    ),
                    fluidRow(
                      column(3, numericInput("bet_total", "Over/Under line", value = 145.5, step = 0.5, width = "100%")),
                      column(3, numericInput("bet_total_odds", "Over/Under price", value = -110, step = 5, width = "100%")),
                      column(3, numericInput("bankroll", "Bankroll ($)", value = 1000, min = 0, step = 100, width = "100%")),
                      column(3, br(), actionButton("analyze_bet_btn", "Analyze Bets",
                                                   class = "btn-primary btn-block", icon = icon("dollar-sign")))
                    ),
                    fluidRow(
                      column(6, sliderInput("w_spread", "Trust in model vs. market: spread & moneyline (%)",
                                            min = 0, max = 80, value = 45, step = 5, width = "100%"),
                             p(class = "text-muted", style = "font-size: 0.75rem;",
                               "Measured on 4,751 games of 2026: about 54% vs opening lines, 10% vs closing lines.")),
                      column(6, sliderInput("w_total", "Trust in model vs. market: over/under (%)",
                                            min = 0, max = 80, value = 55, step = 5, width = "100%"),
                             p(class = "text-muted", style = "font-size: 0.75rem;",
                               "Measured: about 64% vs opening totals, 1% vs closing totals."))
                    ),
                    uiOutput("bet_analysis")
                )
              )
      ),
      
      # ── Stats ────────────────────────────────────────────────────────────────
      tabItem("stats",
              box(width = 12, title = "Current Team Ratings",
                  DTOutput("ratings_table"))
      ),
      
      # ── Eval ─────────────────────────────────────────────────────────────────
      tabItem("eval",
              fluidRow(
                box(width = 12, title = "Model Evaluation — Held-Out Test Set",
                    status = "info",
                    p("Walk-forward backtest: each season is predicted by models trained only on earlier seasons."),
                    uiOutput("eval_summary")
                )
              )
      )
      
    )))
  )
)