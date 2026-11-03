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
# ── Today's Picks: one card per game ──────────────────────────────────────────
fmt_signed <- function(x, digits = 1) ifelse(is.na(x), "–", sprintf(paste0("%+.", digits, "f"), x))
fmt_plain  <- function(x) ifelse(is.na(x), "–", sprintf("%.1f", x))

# One bet panel (spread or total). `moved` is a short text like "open +2.5 → now +3.0" or NULL
pick_panel <- function(kind, pick, call, edge, ev, stake, basis, moved = NULL) {
  if (is.null(pick) || is.na(pick)) return(NULL)
  cls  <- c(BET = "bet", LEAN = "lean", NONE = "none")[[if (is.na(call)) "NONE" else call]]
  lab  <- c(BET = "BET", LEAN = "LEAN", NONE = "NO BET")[[if (is.na(call)) "NONE" else call]]
  div(class = paste("pick-panel", paste0("call-", cls)),
      div(class = "pick-panel-top",
          span(class = "pick-kind", kind),
          span(class = paste("pick-badge", paste0("badge-", cls)), lab)),
      div(class = "pick-main", pick),
      if (cls != "none")
        div(class = "pick-stats",
            span(HTML(glue("<b>{fmt_plain(edge)}</b> pt edge"))),
            span(HTML(glue("<b>{fmt_signed(ev)}%</b> EV"))),
            if (cls == "bet") span(HTML(glue("stake <b>{fmt_plain(stake)}%</b>")))),
      div(class = "pick-sub",
          if (!is.null(moved)) moved,
          if (!is.null(basis) && !is.na(basis)) span(class = "pick-basis", paste(basis, "basis"))))
}

pick_card <- function(r) {
  home_wins <- r$model_margin_home >= 0
  fav  <- if (home_wins) r$home else r$away
  model_txt <- glue("Model: {fav} by {fmt_plain(abs(r$model_margin_home))}  ·  total {fmt_plain(r$model_total)}")
  sp_move <- NULL
  if (!is.na(r$spread_pick)) {
    sgn <- if (startsWith(r$spread_pick, r$home)) 1 else -1
    sp_move <- if (!is.na(r$spread_open) && r$spread_open != r$spread_now)
      glue("line moved {fmt_signed(sgn * r$spread_open)} → {fmt_signed(sgn * r$spread_now)}") else "line unmoved"
  }
  tt_move <- NULL
  if (!is.na(r$total_pick))
    tt_move <- if (!is.na(r$total_open) && r$total_open != r$total_now)
      glue("line moved {fmt_plain(r$total_open)} → {fmt_plain(r$total_now)}") else "line unmoved"
  div(class = "pick-card",
      div(class = "pick-head",
          div(span(class = "pick-tip", r$tip_et),
              if (isTRUE(r$neutral)) span(class = "pick-tag", "neutral site")),
          div(class = "pick-game", glue("{r$away} @ {r$home}")),
          div(class = "pick-model", model_txt)),
      div(class = "pick-panels",
          pick_panel("Spread", r$spread_pick, r$spread_call, r$spread_edge, r$spread_ev, r$spread_stake,
                     r$spread_basis, sp_move),
          pick_panel("Total", r$total_pick, r$total_call, r$total_edge, r$total_ev, r$total_stake,
                     r$total_basis, tt_move)))
}

# ── Manual-bet result card: verdict, pick, key numbers in a grid ──────────────
bet_result_card <- function(kind, call, pick, stats, note = NULL) {
  cls <- c(bet = "r-yes", lean = "r-neutral", none = "r-no")[[call]]
  badge <- c(bet = "bet", lean = "lean", none = "none")[[call]]
  lab <- c(bet = "BET", lean = "LEAN", none = "NO BET")[[call]]
  div(class = paste("bet-result", cls),
      div(class = "bet-result-top",
          span(class = "pick-kind", kind),
          span(class = paste("pick-badge", paste0("badge-", badge)), lab)),
      div(class = "pick-main", pick),
      div(class = "bet-grid",
          lapply(names(stats), function(n) div(span(n), tags$b(stats[[n]])))),
      if (!is.null(note)) div(class = "bet-note", note))
}
