fetch_season_results <- function(season, max_retries = 3, pause_sec = 2) {
  attempt <- 0
  result  <- NULL
  
  while (attempt < max_retries && is.null(result)) {
    attempt <- attempt + 1
    tryCatch({
      raw <- hoopR::load_mbb_schedule(seasons = season)
      result <- raw |>
        filter(!is.na(home_score), !is.na(away_score),
               status_type_completed == TRUE) |>
        transmute(
          game_id      = as.character(game_id),
          game_date    = as.Date(game_date),
          home_team    = home_display_name,
          away_team    = away_display_name,
          home_score   = as.integer(home_score),
          away_score   = as.integer(away_score),
          neutral_site = neutral_site,
          home_win     = as.integer(home_score > away_score)
        )
      Sys.sleep(pause_sec)
    }, error = function(e) {
      message(glue("Season {season} attempt {attempt} failed: {e$message}"))
      if (attempt < max_retries) Sys.sleep(pause_sec * attempt)
    })
  }
  
  if (is.null(result)) {
    warning(glue("Could not fetch season {season} after {max_retries} attempts."))
    return(tibble())
  }
  result
}

load_game_results <- memoise(function(seasons = c(SEASON_YEAR - 2,
                                                  SEASON_YEAR - 1,
                                                  SEASON_YEAR)) {
  message(glue("Loading seasons: {paste(seasons, collapse=', ')}"))
  map_dfr(seasons, function(s) {
    message(glue("  Fetching {s}..."))
    fetch_season_results(s)
  }) |>
    distinct(game_id, .keep_all = TRUE) |>
    arrange(game_date)
}, cache = cache)

build_name_crosswalk <- function(kenpom_names, espn_names) {
  exact <- tibble(kenpom = kenpom_names) |>
    mutate(espn = if_else(kenpom %in% espn_names, kenpom, NA_character_))
  
  unmatched_kp   <- exact |> filter(is.na(espn)) |> pull(kenpom)
  unmatched_espn <- setdiff(espn_names, exact |> filter(!is.na(espn)) |> pull(espn))
  
  if (length(unmatched_kp) > 0 && length(unmatched_espn) > 0) {
    fuzzy <- map_dfr(unmatched_kp, function(kp) {
      dists    <- stringdist::stringdist(tolower(kp), tolower(unmatched_espn), method = "jw")
      best_idx <- which.min(dists)
      tibble(
        kenpom = kp,
        espn   = if (dists[best_idx] < 0.15) unmatched_espn[best_idx] else NA_character_,
        dist   = dists[best_idx]
      )
    })
    bind_rows(
      exact |> filter(!is.na(espn)) |> mutate(dist = 0),
      fuzzy
    )
  } else {
    exact |> mutate(dist = 0)
  }
}

build_actual_training_labels <- function(graph,
                                         seasons = c(SEASON_YEAR - 2,
                                                     SEASON_YEAR - 1,
                                                     SEASON_YEAR)) {
  results    <- load_game_results(seasons)
  team_names <- graph$team_names
  idx        <- setNames(seq_along(team_names) - 1L, team_names)
  
  if (nrow(results) == 0) stop("No game results loaded.")
  
  espn_teams <- unique(c(results$home_team, results$away_team))
  crosswalk  <- build_name_crosswalk(team_names, espn_teams)
  matched    <- crosswalk |> filter(!is.na(espn))
  
  n_unmatched <- sum(is.na(crosswalk$espn))
  if (n_unmatched > 0)
    message(glue("{n_unmatched} KenPom teams unmatched to ESPN — excluded from training."))
  
  espn_to_kp <- setNames(matched$kenpom, matched$espn)
  
  labels <- results |>
    filter(home_team %in% names(espn_to_kp),
           away_team %in% names(espn_to_kp)) |>
    mutate(
      kp_home = espn_to_kp[home_team],
      kp_away = espn_to_kp[away_team]
    ) |>
    filter(kp_home %in% team_names, kp_away %in% team_names) |>
    transmute(
      team_a_idx = idx[kp_away],
      team_b_idx = idx[kp_home],
      winner     = as.integer(home_win),
      score_a    = away_score,
      score_b    = home_score,
      game_date  = game_date,
      neutral    = as.integer(neutral_site)
    ) |>
    filter(!is.na(team_a_idx), !is.na(team_b_idx))
  
  message(glue("Training labels: {nrow(labels)} games across {length(seasons)} seasons."))
  labels
}