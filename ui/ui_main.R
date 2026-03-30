ui <- dashboardPage(
  dashboardHeader(title = "CBB Matchup Predictor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Predict Matchup", tabName = "predict", icon = icon("basketball-ball")),
      menuItem("Train Model",     tabName = "train",   icon = icon("brain")),
      menuItem("Team Stats",      tabName = "stats",   icon = icon("table"))
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
              )
      ),
      
      # ── Train ─────────────────────────────────────────────────────────────
      tabItem("train",
              box(width = 12, title = "Model Training", status = "warning",
                  p("Pulls 3 seasons of actual game results via hoopR and trains the GNN.
             Expect 3–8 minutes on first run."),
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
      )
    )
  )
)