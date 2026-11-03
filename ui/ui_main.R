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
      if (file.exists(".venv"))
        menuItem("Train Model",    tabName = "train",   icon = icon("brain")),
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
                      "Enter the lines from your sportsbook. Edges are shrunk toward the market line ",
                      "because the closing line is hard to beat (see the Model Evaluation tab). ",
                      "Most games will honestly say NO BET."),
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
                                            min = 0, max = 50, value = 10, step = 5, width = "100%"),
                             p(class = "text-muted", style = "font-size: 0.75rem;",
                               "Measured on 4,751 games: about 10% (uncertainty roughly 0 to 25%).")),
                      column(6, sliderInput("w_total", "Trust in model vs. market: over/under (%)",
                                            min = 0, max = 50, value = 0, step = 5, width = "100%"),
                             p(class = "text-muted", style = "font-size: 0.75rem;",
                               "Measured: about 1%. The model has not beaten the totals market."))
                    ),
                    uiOutput("bet_analysis")
                )
              )
      ),
      
      # ── Train ────────────────────────────────────────────────────────────────
      if (file.exists(".venv"))
        tabItem("train",
                box(width = 12, title = "Model Training", status = "warning",
                    p("Trains the GNN on actual game results with point-in-time features.
               Expect 5–10 minutes (500 epochs)."),
                    fluidRow(
                      column(4,
                             sliderInput("half_life", "Recency half-life (days)",
                                         min = 14, max = 180, value = 60, step = 7),
                             p(class = "text-muted",
                               "Lower = recent games weighted much more heavily. ",
                               "60 days means a game from 2 months ago counts half as much as today's.")
                      ),
                      column(4,
                             checkboxGroupInput("train_seasons", "Seasons to include",
                                                choices = setNames(
                                                  c(SEASON_YEAR - 2, SEASON_YEAR - 1, SEASON_YEAR),
                                                  c(glue("{SEASON_YEAR-3}-{SEASON_YEAR-2}"),
                                                    glue("{SEASON_YEAR-2}-{SEASON_YEAR-1}"),
                                                    glue("{SEASON_YEAR-1}-{SEASON_YEAR}"))
                                                ),
                                                selected = c(SEASON_YEAR - 2, SEASON_YEAR - 1, SEASON_YEAR)
                             )
                      ),
                      column(4,
                             br(),
                             checkboxInput("full_train",
                                           "Full train mode (use all data, deploy-ready)",
                                           value = TRUE),
                             actionButton("train_btn", "Train Model",
                                          class = "btn-warning btn-block", icon = icon("cogs"))
                      )
                    ),
                    verbatimTextOutput("train_log")
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
                    p("Metrics computed on the most recent 20% of games, never seen during training."),
                    uiOutput("eval_summary")
                )
              )
      )
      
    )))
  )
)