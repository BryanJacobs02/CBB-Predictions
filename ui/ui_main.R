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
    do.call(sidebarMenu, c(list(id = "tabs"), Filter(Negate(is.null), list(
      menuItem("Predict Matchup",  tabName = "predict", icon = icon("basketball-ball")),
      menuItem("Today's Picks",    tabName = "picks",   icon = icon("list-ol")),
      menuItem("Team Stats",       tabName = "stats",   icon = icon("table")),
      menuItem("Model Evaluation", tabName = "eval",    icon = icon("chart-line"))
    ))))
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
                      "The model beat 2026 OPENING lines but not closing lines, so say which one you have."),
                    div(class = "bet-section-title", "1. Setup"),
                    div(class = "bet-setup",
                        fluidRow(
                          column(4, radioButtons("line_type", "Which line is this?",
                                                 choices = c("Opening (bet early)" = "open",
                                                             "Current / closing"   = "close"),
                                                 selected = "open")),
                          column(4, selectInput("bet_team", "Team for spread & moneyline",
                                                choices = c("Select after predicting" = ""), width = "100%")),
                          column(4, numericInput("bankroll", "Bankroll ($)", value = 1000, min = 0, step = 100, width = "100%"))
                        )),
                    div(class = "bet-section-title", "2. Lines from your book"),
                    fluidRow(
                      column(4, div(class = "bet-input-card",
                                    div(class = "bet-input-title", "Spread"),
                                    numericInput("bet_spread", "Line (-3.5 = favorite)", value = -3.5, step = 0.5, width = "100%"),
                                    numericInput("bet_spread_odds", "Price", value = -110, step = 5, width = "100%"))),
                      column(4, div(class = "bet-input-card c-ml",
                                    div(class = "bet-input-title", "Moneyline"),
                                    numericInput("bet_moneyline", "Price for that team", value = -150, step = 5, width = "100%"))),
                      column(4, div(class = "bet-input-card c-tot",
                                    div(class = "bet-input-title", "Over / Under"),
                                    numericInput("bet_total", "Total line", value = 145.5, step = 0.5, width = "100%"),
                                    numericInput("bet_total_odds", "Price", value = -110, step = 5, width = "100%")))
                    ),
                    tags$details(class = "bet-adv",
                      tags$summary("Advanced: how much to trust the model vs. the market"),
                      fluidRow(
                        column(6, sliderInput("w_spread", "Spread & moneyline (%)",
                                              min = 0, max = 80, value = 45, step = 5, width = "100%"),
                               p(class = "text-muted", style = "font-size: 0.75rem;",
                                 "Measured on 4,751 games of 2026: about 54% vs opening lines, 10% vs closing lines.")),
                        column(6, sliderInput("w_total", "Over/Under (%)",
                                              min = 0, max = 80, value = 55, step = 5, width = "100%"),
                               p(class = "text-muted", style = "font-size: 0.75rem;",
                                 "Measured: about 64% vs opening totals, 1% vs closing totals."))
                      )),
                    actionButton("analyze_bet_btn", "Analyze Bets", class = "btn-primary btn-block btn-lg",
                                 icon = icon("dollar-sign")),
                    uiOutput("bet_analysis")
                )
              )
      ),
      
      # ── Today's picks ────────────────────────────────────────────────────────
      tabItem("picks",
              box(width = 12, title = "Today's Picks", status = "success",
                  fluidRow(
                    column(3, actionButton("refresh_picks", "Refresh lines now",
                                           class = "btn-primary btn-block", icon = icon("sync"))),
                    column(9, uiOutput("picks_header"))
                  ),
                  radioButtons("picks_filter", NULL, inline = TRUE,
                               choices = c("All games" = "all", "Bets and Leans" = "leans", "Bets only" = "bets"),
                               selected = "all"),
                  uiOutput("picks_cards"),
                  tags$details(class = "picks-help",
                    tags$summary("How to read this"),
                    p("BET = the model's edge, after shrinking toward the market, is worth at least +2% expected value. ",
                      "LEAN = positive but too small to bet. NO BET = no edge. Edge is the gap in points between the ",
                      "model and the line, before shrinking."),
                    p("'open basis' means the line has not moved since it opened and tip-off is more than 3 hours away, ",
                      "so the measured opening-line weights apply. 'close basis' means the market has moved or game time ",
                      "is near, so the much smaller closing-line weights apply (these are almost never bets)."),
                    p("Stake is the suggested share of your bankroll (quarter-Kelly, capped at 3%). ",
                      "Lines move: always check your book before betting.")
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