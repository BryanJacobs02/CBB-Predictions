py_train   <- NULL
py_predict <- NULL

init_python_modules <- function(force_reload = FALSE) {
  if (is.null(py_train) || force_reload) {
    py_run_string("import importlib, sys")
    py_run_string("
for mod in ['train', 'predict', 'gnn_model']:
    if mod in sys.modules:
        del sys.modules[mod]
")
    py_train   <<- import_from_path("train",   path = "python")
    py_predict <<- import_from_path("predict", path = "python")
  }
}

run_training <- function(seasons   = c(SEASON_YEAR - 2,
                                       SEASON_YEAR - 1,
                                       SEASON_YEAR),
                         half_life = 60.0,
                         full_train = TRUE) {
  init_python_modules(force_reload = TRUE)
  
  withProgress(message = "Building current feature matrix...", value = 0.05, {
    feats <- build_feature_matrix()
    
    setProgress(0.1, message = "Building team graph...")
    graph <- build_team_graph(feats)
    
    setProgress(0.15, message = glue("Fetching weekly archive snapshots ",
                                     "(this takes 2-4 minutes)..."))
    snapshots <- build_historical_feature_matrix(seasons)
    
    setProgress(0.5, message = "Loading actual game results (hoopR)...")
    labels <- build_actual_training_labels(graph, snapshots, seasons)
    
    if (nrow(labels) < 50)
      stop(glue("Only {nrow(labels)} matched games — too few to train."))
    
    # ── Recency weights ──────────────────────────────────────────────────────
    today       <- as.numeric(Sys.Date())
    game_dates  <- as.numeric(as.Date(labels$game_date))
    days_ago    <- today - game_dates
    rec_weights <- pmax(0.5 ^ (days_ago / half_life), 1e-4)
    
    setProgress(0.6, message = glue("Training GNN on {nrow(labels)} games ",
                                    "(leak-free)..."))
    
    labels_clean <- labels |>
      select(team_a_idx, team_b_idx, winner, score_a, score_b, neutral)
    
    # Parse per-game feature vectors from JSON
    feats_a <- do.call(rbind, lapply(labels$feats_a, jsonlite::fromJSON))
    feats_b <- do.call(rbind, lapply(labels$feats_b, jsonlite::fromJSON))
    
    py_train$train(
      node_features   = graph$node_features,
      edge_src        = graph$edge_src,
      edge_dst        = graph$edge_dst,
      edge_weights    = graph$edge_weights,
      team_names      = graph$team_names,
      matchup_labels = list(
        team_a_idx = as.integer(labels_clean$team_a_idx),
        team_b_idx = as.integer(labels_clean$team_b_idx),
        winner     = as.integer(labels_clean$winner),
        score_a    = as.numeric(labels_clean$score_a),
        score_b    = as.numeric(labels_clean$score_b),
        neutral    = as.integer(labels_clean$neutral)
      ),
      game_feats_a    = feats_a,
      game_feats_b    = feats_b,
      recency_weights = rec_weights,
      half_life_days  = half_life,
      full_train      = full_train
    )
    
    setProgress(1.0, message = "Done.")
  })
}

predict_game <- function(team_a, team_b, location = "neutral") {
  init_python_modules()
  
  feats <- build_feature_matrix()
  graph <- build_team_graph(feats)
  
  idx_a <- which(graph$team_names == team_a) - 1L
  idx_b <- which(graph$team_names == team_b) - 1L
  
  if (length(idx_a) == 0 || length(idx_b) == 0)
    stop(glue("Team not found: '{team_a}' or '{team_b}'"))
  
  numeric_cols <- feats |>
    select(where(~ is.numeric(.) && !is.list(.))) |>
    names()
  
  team_a_row <- feats |> filter(TeamName == team_a) |> slice(1)
  team_b_row <- feats |> filter(TeamName == team_b) |> slice(1)
  
  if (nrow(team_a_row) == 0 || nrow(team_b_row) == 0)
    stop(glue("Team features not found for '{team_a}' or '{team_b}'"))
  
  safe_numeric <- function(df_row, cols) {
    vals <- sapply(cols, function(col) {
      val <- df_row[[col]]
      if (is.list(val)) return(0)
      v <- suppressWarnings(as.numeric(val))
      if (length(v) != 1 || is.na(v)) return(0)
      v
    })
    vals
  }
  
  feat_vec_a <- safe_numeric(team_a_row, numeric_cols)
  feat_vec_b <- safe_numeric(team_b_row, numeric_cols)
  
  run_pred <- function(home_idx, away_idx, home_feats, away_feats, neutral = 0) {
    py_predict$predict_matchup(
      node_features = graph$node_features,
      edge_src      = graph$edge_src,
      edge_dst      = graph$edge_dst,
      edge_weights  = graph$edge_weights,
      team_a_idx    = home_idx,
      team_b_idx    = away_idx,
      feat_vec_a    = home_feats,
      feat_vec_b    = away_feats,
      is_neutral    = neutral
    )
  }
  
  if (location == "a_home") {
    r <- run_pred(idx_a, idx_b, feat_vec_a, feat_vec_b, neutral = 0)
    wp_a    <- r$win_prob_a
    score_a <- r$pred_score_a
    score_b <- r$pred_score_b
    
  } else if (location == "b_home") {
    r <- run_pred(idx_b, idx_a, feat_vec_b, feat_vec_a, neutral = 0)
    wp_a    <- r$win_prob_b
    score_a <- r$pred_score_b
    score_b <- r$pred_score_a
    
  } else {
    # Neutral site — average both directions
    r_ab <- run_pred(idx_a, idx_b, feat_vec_a, feat_vec_b, neutral = 1)
    r_ba <- run_pred(idx_b, idx_a, feat_vec_b, feat_vec_a, neutral = 1)
    wp_a    <- (r_ab$win_prob_a + r_ba$win_prob_b) / 2
    score_a <- (r_ab$pred_score_a + r_ba$pred_score_b) / 2
    score_b <- (r_ab$pred_score_b + r_ba$pred_score_a) / 2
  }
  
  result <- list(
    win_prob_a   = wp_a,
    win_prob_b   = 1 - wp_a,
    pred_score_a = score_a,
    pred_score_b = score_b
  )
  
  kenpom_pred <- tryCatch({
    fm <- get_fanmatch() |>
      filter((Visitor == team_a & Home == team_b) |
               (Visitor == team_b & Home == team_a))
    if (nrow(fm) > 0) fm[1, ] else NULL
  }, error = \(e) NULL)
  
  list(gnn = result, kenpom = kenpom_pred,
       team_a = team_a, team_b = team_b)
}