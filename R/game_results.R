fetch_season_results <- function(season, max_retries = 3, pause_sec = 2) {
  attempt <- 0
  result  <- NULL
  
  while (attempt < max_retries && is.null(result)) {
    attempt <- attempt + 1
    tryCatch({
      raw <- hoopR::load_mbb_schedule(seasons = season)
      
      # Debug: print column names on first successful fetch
      if (attempt == 1) {
        message(glue("Available columns: {paste(names(raw), collapse=', ')}"))
      }
      
      # hoopR column names can vary by version — detect which name columns exist
      home_col <- intersect(c("home_display_name", "home_name",
                              "home_team_display_name"), names(raw))[1]
      away_col <- intersect(c("away_display_name", "away_name",
                              "away_team_display_name"), names(raw))[1]
      
      if (is.na(home_col) || is.na(away_col)) {
        message("Could not find team name columns. Available: ", paste(names(raw), collapse=", "))
        return(tibble())
      }
      
      result <- raw |>
        filter(!is.na(home_score), !is.na(away_score),
               status_type_completed == TRUE) |>
        transmute(
          game_id      = as.character(game_id),
          game_date    = as.Date(game_date),
          home_team    = .data[[home_col]],
          away_team    = .data[[away_col]],
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
  # Filter out future seasons that won't have results yet
  current_season <- if (month(Sys.Date()) >= 11) {
    as.integer(format(Sys.Date(), "%Y")) + 1
  } else {
    as.integer(format(Sys.Date(), "%Y"))
  }
  seasons <- seasons[seasons <= current_season]
  
  message(glue("Loading seasons: {paste(seasons, collapse=', ')}"))
  map_dfr(seasons, function(s) {
    message(glue("  Fetching {s}..."))
    fetch_season_results(s)
  }) |>
    distinct(game_id, .keep_all = TRUE) |>
    arrange(game_date)
}, cache = cache)

build_name_crosswalk <- function(kenpom_names, espn_names) {
  # Normalize names for comparison
  normalize <- function(x) {
    x |>
      tolower() |>
      stringr::str_replace_all("\\bst\\.?\\b", "state") |>
      stringr::str_replace_all("\\buc\\b", "california") |>
      stringr::str_replace_all("[^a-z0-9 ]", "") |>
      stringr::str_squish()
  }
  
  kp_norm   <- normalize(kenpom_names)
  espn_norm <- normalize(espn_names)
  
  # Try exact match on normalized names first
  exact_idx <- match(kp_norm, espn_norm)
  exact <- tibble(
    kenpom = kenpom_names,
    espn   = ifelse(!is.na(exact_idx), espn_names[exact_idx], NA_character_),
    dist   = ifelse(!is.na(exact_idx), 0, NA_real_)
  )
  
  unmatched_kp_idx   <- which(is.na(exact$espn))
  unmatched_espn_idx <- which(!espn_names %in% exact$espn[!is.na(exact$espn)])
  
  # Fuzzy match on remaining
  if (length(unmatched_kp_idx) > 0 && length(unmatched_espn_idx) > 0) {
    unmatched_kp_norm   <- kp_norm[unmatched_kp_idx]
    unmatched_espn_norm <- espn_norm[unmatched_espn_idx]
    
    fuzzy_results <- map_dfr(seq_along(unmatched_kp_idx), function(j) {
      dists    <- stringdist::stringdist(
        unmatched_kp_norm[j], unmatched_espn_norm, method = "jw"
      )
      best_idx <- which.min(dists)
      best_dist <- dists[best_idx]
      tibble(
        kp_idx = unmatched_kp_idx[j],
        espn   = if (best_dist < 0.12) espn_names[unmatched_espn_idx[best_idx]]
        else NA_character_,
        dist   = best_dist
      )
    })
    
    for (i in seq_len(nrow(fuzzy_results))) {
      exact$espn[fuzzy_results$kp_idx[i]] <- fuzzy_results$espn[i]
      exact$dist[fuzzy_results$kp_idx[i]] <- fuzzy_results$dist[i]
    }
  }
  
  exact
}

build_actual_training_labels <- function(graph,
                                         seasons = c(SEASON_YEAR - 2,
                                                     SEASON_YEAR - 1,
                                                     SEASON_YEAR)) {
  results    <- load_game_results(seasons)
  team_names <- graph$team_names
  idx        <- setNames(seq_along(team_names) - 1L, team_names)
  
  if (nrow(results) == 0) stop("No game results loaded.")
  
  # Show sample ESPN names to help debug matching
  espn_sample <- head(sort(unique(c(results$home_team, results$away_team))), 20)
  message("Sample ESPN team names: ", paste(espn_sample, collapse = ", "))
  
  kp_sample <- head(sort(team_names), 20)
  message("Sample KenPom team names: ", paste(kp_sample, collapse = ", "))
  
  espn_teams <- unique(c(results$home_team, results$away_team))
  crosswalk  <- build_name_crosswalk(team_names, espn_teams)
  matched    <- crosswalk |> filter(!is.na(espn))
  
  n_matched   <- nrow(matched)
  n_unmatched <- sum(is.na(crosswalk$espn))
  message(glue("{n_matched} KenPom teams matched, {n_unmatched} unmatched."))
  
  if (n_matched < 50)
    stop(glue("Only {n_matched} teams matched between KenPom and ESPN. ",
              "Check team name formats above."))
  
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
      game_date  = as.character(game_date),
      neutral    = as.integer(neutral_site)
    ) |>
    filter(!is.na(team_a_idx), !is.na(team_b_idx))
  
  message(glue("Training labels: {nrow(labels)} games across {length(seasons)} seasons."))
  labels
}