# ─────────────────────────────────────────────────────────────────────────────
# plots_v2.R — dark-theme versions of the Win Probability and Key Stat charts
#
# INSTALL (2 steps)
#   1. Save this file as  R/plots_v2.R  in the Lockbot project.
#   2. In global.R, add this line next to the other source() calls:
#          source("R/plots_v2.R")
#      then replace the two renderPlotly blocks in server/server_main.R
#      (output$wp_gauge and output$stat_comparison) with the versions at the
#      bottom of this file.
# ─────────────────────────────────────────────────────────────────────────────

# Colors match www/styles.css
LB <- list(
  card   = "#161b22",
  border = "#30363d",
  text   = "#e6edf3",
  text2  = "#8b949e",
  accent = "#58a6ff",
  orange = "#d29922"
)

# Team colors are often dark navy/maroon: lighten them until they're readable
# on the dark card background.
ensure_visible <- function(hex, min_lum = 0.42, fallback = LB$accent) {
  if (is.null(hex) || length(hex) == 0 || is.na(hex[1])) return(fallback)
  rgb <- tryCatch(grDevices::col2rgb(hex[1])[, 1], error = function(e) NULL)
  if (is.null(rgb)) return(fallback)
  lum <- sum(rgb * c(0.299, 0.587, 0.114)) / 255
  if (lum >= min_lum) return(hex[1])
  t   <- min(1, (min_lum - lum) / (1 - lum))      # blend toward white
  mix <- round(rgb + (255 - rgb) * t)
  grDevices::rgb(mix[1], mix[2], mix[3], maxColorValue = 255)
}

# Shared look: transparent background, theme fonts, no toolbar
lockbot_plot_theme <- function(p, ...) {
  p |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(family = "Barlow, sans-serif", color = LB$text2, size = 13),
      ...
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# Pick two distinguishable, readable colors for the matchup
matchup_colors <- function(color_a, color_b) {
  ca <- ensure_visible(color_a, fallback = LB$accent)
  cb <- ensure_visible(color_b, fallback = LB$orange)
  if (tolower(ca) == tolower(cb)) cb <- LB$orange
  c(ca, cb)
}

# ── Win probability: donut split between the two teams ───────────────────────
wp_donut_plot <- function(team_a, team_b, wp_a, color_a, color_b) {
  wp_a <- max(0, min(1, wp_a)); wp_b <- 1 - wp_a
  cols <- matchup_colors(color_a, color_b)
  fav_is_a <- wp_a >= wp_b
  fav      <- if (fav_is_a) team_a else team_b
  fav_col  <- if (fav_is_a) cols[1] else cols[2]
  fav_pct  <- round(100 * max(wp_a, wp_b))

  hover <- c(paste0(team_a, ": ", round(100 * wp_a, 1), "%"),
             paste0(team_b, ": ", round(100 * wp_b, 1), "%"))

  plotly::plot_ly(
    labels = c(team_a, team_b),
    values = c(wp_a, wp_b),
    type = "pie", hole = 0.74, sort = FALSE, direction = "clockwise",
    textinfo = "none", text = hover, hoverinfo = "text",
    marker = list(colors = cols, line = list(color = LB$card, width = 4)),
    showlegend = TRUE
  ) |>
    lockbot_plot_theme(
      margin = list(l = 10, r = 10, t = 10, b = 10),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.02,
                    font = list(size = 14, color = LB$text)),
      annotations = list(
        list(text = paste0("<b>", fav_pct, "%</b>"), x = 0.5, y = 0.56,
             xref = "paper", yref = "paper", showarrow = FALSE,
             font = list(size = 54, color = LB$text, family = "Barlow Condensed, sans-serif")),
        list(text = toupper(paste(fav, "to win")), x = 0.5, y = 0.40,
             xref = "paper", yref = "paper", showarrow = FALSE,
             font = list(size = 14, color = fav_col, family = "Barlow Condensed, sans-serif"))
      )
    )
}

# ── Key stat comparison: grouped percentile bars ─────────────────────────────
stat_comparison_plot <- function(stats_long, team_a, team_b, color_a, color_b) {
  cols <- stats::setNames(matchup_colors(color_a, color_b), c(team_a, team_b))
  stats_long$TeamName <- factor(stats_long$TeamName, levels = c(team_a, team_b))

  plotly::plot_ly(
    stats_long, x = ~Stat, y = ~value, color = ~TeamName, colors = cols,
    type = "bar",
    text = ~value, textposition = "outside", cliponaxis = FALSE,
    textfont = list(color = LB$text, size = 14),
    marker = list(line = list(width = 0)),
    hovertemplate = "%{x}<br>%{fullData.name}: %{y}th percentile<extra></extra>"
  ) |>
    lockbot_plot_theme(
      barmode = "group", bargap = 0.35, bargroupgap = 0.08,
      margin = list(l = 45, r = 10, t = 30, b = 10),
      yaxis = list(title = "", range = c(0, 112), tickvals = c(0, 25, 50, 75, 100),
                   gridcolor = LB$border, zeroline = FALSE, fixedrange = TRUE,
                   tickfont = list(color = LB$text2)),
      xaxis = list(title = "", fixedrange = TRUE, tickfont = list(color = LB$text, size = 14),
                   categoryorder = "array", categoryarray = unique(stats_long$Stat)),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12,
                    font = list(size = 14, color = LB$text)),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 50, y1 = 50,
                         line = list(color = LB$text2, width = 1, dash = "dot"))),
      annotations = list(list(text = "percentile rank among D1 teams (100 = best; for tempo, 100 = fastest)",
                              x = 0, y = 1.12, xref = "paper", yref = "paper",
                              xanchor = "left", showarrow = FALSE,
                              font = list(size = 12, color = LB$text2)))
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# REPLACE the two blocks in server/server_main.R with these:
# ─────────────────────────────────────────────────────────────────────────────
#
#  output$wp_gauge <- renderPlotly({
#    p <- prediction()
#    g <- p$gnn
#    aes <- get_team_aesthetics()
#    wp_donut_plot(p$team_a, p$team_b, g$win_prob_a,
#                  get_team_color(p$team_a, aes, primary = TRUE),
#                  get_team_color(p$team_b, aes, primary = TRUE))
#  })
#
#  output$stat_comparison <- renderPlotly({
#    req(prediction())
#    p <- prediction()
#    ... (keep your existing code that builds `stats_long`, and DELETE the
#         plot_ly(...) |> layout(...) call at the end) ...
#    aes <- get_team_aesthetics()
#    stat_comparison_plot(stats_long, p$team_a, p$team_b,
#                         get_team_color(p$team_a, aes, primary = TRUE),
#                         get_team_color(p$team_b, aes, primary = TRUE))
#  })
