# ── Team logo and color lookup ─────────────────────────────────────────────────
get_team_aesthetics <- memoise(function(season = SEASON_YEAR) {
  tryCatch({
    raw <- hoopR::load_mbb_schedule(seasons = season) |>
      dplyr::select(
        home_display_name, home_logo, home_color, home_alternate_color,
        away_display_name, away_logo, away_color, away_alternate_color
      ) |>
      tidyr::pivot_longer(
        cols = everything(),
        names_to = c("side", ".value"),
        names_pattern = "(home|away)_(.*)"
      ) |>
      dplyr::select(-side) |>
      dplyr::distinct(display_name, .keep_all = TRUE) |>
      dplyr::rename(espn_name = display_name)
    
    # Join to KenPom names via crosswalk
    feats      <- build_feature_matrix()
    espn_teams <- raw$espn_name
    crosswalk  <- build_name_crosswalk(feats$TeamName, espn_teams)
    matched    <- crosswalk |> dplyr::filter(!is.na(espn))
    espn_to_kp <- setNames(matched$kenpom, matched$espn)
    
    raw |>
      dplyr::mutate(kenpom_name = espn_to_kp[espn_name]) |>
      dplyr::filter(!is.na(kenpom_name)) |>
      dplyr::mutate(
        color           = paste0("#", color),
        alternate_color = paste0("#", alternate_color)
      )
  }, error = function(e) {
    message("Team aesthetics unavailable: ", e$message)
    tibble()
  })
}, cache = cache)

get_team_logo <- function(team_name, aesthetics) {
  row <- aesthetics |> dplyr::filter(kenpom_name == team_name)
  if (nrow(row) == 0) return("https://a.espncdn.com/i/teamlogos/ncaa/500/default.png")
  row$logo[1]
}

get_team_color <- function(team_name, aesthetics, primary = TRUE) {
  row <- aesthetics |> dplyr::filter(kenpom_name == team_name)
  if (nrow(row) == 0) return(if (primary) "#2196F3" else "#9C27B0")
  if (primary) row$color[1] else row$alternate_color[1]
}