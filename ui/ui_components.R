# ── Result card (dark theme) ──────────────────────────────────────────────────
result_card <- function(title, value, subtitle = NULL, color = "#58a6ff") {
  div(
    class = "result-card",
    style = glue("border-left: 3px solid {color};"),
    p(class = "card-label", title),
    h2(class = "card-value", style = glue("color: {color};"), value),
    if (!is.null(subtitle))
      p(class = "card-subtitle", subtitle)
  )
}

# ── Team selector ─────────────────────────────────────────────────────────────
team_selector <- function(id, label, choices) {
  pickerInput(id, label, choices = choices,
              options = list(`live-search` = TRUE, size = 10),
              width = "100%")
}

# ── Matchup header with logos ─────────────────────────────────────────────────
matchup_header <- function(team_a, team_b, logo_a, logo_b,
                           color_a, color_b) {
  div(
    class = "matchup-header",
    # Team A
    div(class = "team-block",
        tags$img(src = logo_a, class = "team-logo",
                 onerror = "this.src='https://a.espncdn.com/i/teamlogos/ncaa/500/default.png'"),
        div(class = "team-name", style = glue("color: {color_a};"), team_a)
    ),
    div(class = "vs-block", "VS"),
    # Team B
    div(class = "team-block",
        tags$img(src = logo_b, class = "team-logo",
                 onerror = "this.src='https://a.espncdn.com/i/teamlogos/ncaa/500/default.png'"),
        div(class = "team-name", style = glue("color: {color_b};"), team_b)
    )
  )
}

# ── Betting value box ─────────────────────────────────────────────────────────
bet_box <- function(label, verdict, detail, type = "neutral") {
  div(
    class = glue("bet-value-box bet-{type}"),
    p(class = "bet-label", label),
    p(class = "bet-verdict", verdict),
    p(class = "bet-detail", detail)
  )
}