team_selector <- function(id, label, choices) {
  pickerInput(id, label, choices = choices,
              options = list(`live-search` = TRUE, size = 10),
              width = "100%")
}

result_card <- function(title, value, subtitle = NULL, color = "#2196F3") {
  div(
    class = "result-card",
    style = glue("border-left: 5px solid {color}; padding: 12px 16px;
                  background: #f8f9fa; margin-bottom: 12px; border-radius: 4px;"),
    h4(title, style = "margin: 0 0 4px; font-size: 0.9em; color: #666;"),
    h2(value,  style = glue("margin: 0; color: {color}; font-size: 2em;")),
    if (!is.null(subtitle))
      p(subtitle, style = "margin: 4px 0 0; color: #888; font-size: 0.85em;")
  )
}