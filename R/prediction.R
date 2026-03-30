py_train   <- NULL
py_predict <- NULL

init_python_modules <- function(force_reload = FALSE) {
  if (is.null(py_train) || force_reload) {
    # Force Python to reload the module fresh from disk
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
                         half_life = 60.0) {
  init_python_modules(force_reload = TRUE)
  
  withProgress(message = "Building feature matrix...", value = 0.1, {
    feats <- build_feature_matrix()
    
    setProgress(0.3, message = "Building team graph...")
    graph <- build_team_graph(feats)
    
    setProgress(0.5, message = "Loading actual game results (hoopR)...")
    labels <- build_actual_training_labels(graph, seasons)
    
    if (nrow(labels) < 50)
      stop(glue("Only {nrow(labels)} matched games — too few to train."))
    
    # ── Compute recency weights in R, pass directly to Python ─────────────
    today       <- as.numeric(Sys.Date())
    game_dates  <- as.numeric(as.Date(labels$game_date))
    days_ago    <- today - game_dates
    rec_weights <- pmax(0.5 ^ (days_ago / half_life), 1e-4)
    
    setProgress(0.7, message = glue("Training GNN on {nrow(labels)} games..."))
    
    py_train$train(
      node_features  = graph$node_features,
      edge_src       = graph$edge_src,
      edge_dst       = graph$edge_dst,
      edge_weights   = graph$edge_weights,
      team_names     = graph$team_names,
      matchup_labels = labels |>
        select(team_a_idx, team_b_idx, winner, score_a, score_b) |>
        purrr::transpose(),
      recency_weights = rec_weights,
      half_life_days  = half_life
    )
    
    setProgress(1.0, message = "Done.")
  })
}

predict_game <- function(team_a, team_b) {
  init_python_modules()
  
  feats <- build_feature_matrix()
  graph <- build_team_graph(feats)
  
  idx_a <- which(graph$team_names == team_a) - 1L
  idx_b <- which(graph$team_names == team_b) - 1L
  
  if (length(idx_a) == 0 || length(idx_b) == 0)
    stop(glue("Team not found: '{team_a}' or '{team_b}'"))
  
  result <- py_predict$predict_matchup(
    node_features = graph$node_features,
    edge_src      = graph$edge_src,
    edge_dst      = graph$edge_dst,
    edge_weights  = graph$edge_weights,
    team_a_idx    = idx_a,
    team_b_idx    = idx_b
  )
  
  # Pull KenPom's own fanmatch prediction as a reference point
  kenpom_pred <- tryCatch({
    fm <- get_fanmatch() |>
      filter((Visitor == team_a & Home == team_b) |
               (Visitor == team_b & Home == team_a))
    if (nrow(fm) > 0) fm[1, ] else NULL
  }, error = \(e) NULL)
  
  list(gnn = result, kenpom = kenpom_pred,
       team_a = team_a, team_b = team_b)
}