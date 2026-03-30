ui <- dashboardPage(
  dashboardHeader(title = "CBB Matchup Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Predict Matchup", tabName = "predict", icon = icon("basketball-ball")),
      menuItem("Train Model",     tabName = "train",   icon = icon("brain")),
      menuItem("Team Stats",      tabName = "stats",   icon = icon("table")),
      menuItem("Model Evaluation", tabName = "eval", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    tabItems(
      
      # ── Predict ───────────────────────────────────────────────────────────
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
                    fluidRow(
                      column(3,
                             selectInput("bet_team", "Team to Bet On",
                                         choices = c("Select after predicting" = ""),
                                         width = "100%")
                      ),
                      column(3,
                             numericInput("bet_spread", "Point Spread (negative = favorite)",
                                          value = -3.5, step = 0.5, width = "100%")
                      ),
                      column(3,
                             numericInput("bet_moneyline", "Moneyline (e.g. -150 or +130)",
                                          value = -150, step = 5, width = "100%")
                      ),
                      column(3,
                             br(),
                             actionButton("analyze_bet_btn", "Analyze Bet",
                                          class = "btn-primary btn-block",
                                          icon = icon("dollar-sign"))
                      )
                    ),
                    uiOutput("bet_analysis")
                )
              )
      ),
      
      # ── Train ─────────────────────────────────────────────────────────────
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
      
      # ── Stats ─────────────────────────────────────────────────────────────
      tabItem("stats",
              box(width = 12, title = "Current Team Ratings",
                  DTOutput("ratings_table"))
      ),
      
      tabItem("eval",
              fluidRow(
                box(width = 12, title = "Model Evaluation — Held-Out Test Set",
                    status = "info",
                    p("Metrics computed on the most recent 20% of games, never seen during training."),
                    uiOutput("eval_summary")
                )
              )
      )
    )
  )
)