# ── Generate predictions for all D1 matchup combinations ─────────────────────
generate_prediction_cache <- function(locations = c("neutral", "a_home", "b_home")) {
  message("Building feature matrix and graph...")
  feats <- build_feature_matrix()
  graph <- build_team_graph(feats)
  teams <- sort(graph$team_names)
  n     <- length(teams)
  
  message(glue("Generating predictions for {n} teams = ~{n*(n-1)} matchups..."))
  
  # Pre-extract all feature vectors once
  numeric_cols <- feats |>
    select(where(~ is.numeric(.) && !is.list(.))) |>
    names()
  
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
  
  message("Pre-extracting feature vectors...")
  feat_vecs <- map(teams, function(tn) {
    row <- feats |> filter(TeamName == tn) |> slice(1)
    if (nrow(row) == 0) return(rep(0, length(numeric_cols)))
    safe_numeric(row, numeric_cols)
  })
  names(feat_vecs) <- teams
  
  # Pre-initialize Python modules
  init_python_modules()
  
  cache <- list()
  total <- n * (n - 1)
  done  <- 0
  
  message("Generating predictions...")
  
  for (i in seq_along(teams)) {
    team_a <- teams[i]
    idx_a  <- which(graph$team_names == team_a) - 1L
    fv_a   <- feat_vecs[[team_a]]
    
    for (j in seq_along(teams)) {
      if (i == j) next
      
      team_b <- teams[j]
      idx_b  <- which(graph$team_names == team_b) - 1L
      fv_b   <- feat_vecs[[team_b]]
      
      key <- paste0(team_a, "|||", team_b)
      
      tryCatch({
        # a_home: team_a is home
        r_home <- py_predict$predict_matchup(
          node_features = graph$node_features,
          edge_src      = graph$edge_src,
          edge_dst      = graph$edge_dst,
          edge_weights  = graph$edge_weights,
          team_a_idx    = idx_a,
          team_b_idx    = idx_b,
          feat_vec_a    = fv_a,
          feat_vec_b    = fv_b,
          is_neutral    = 0L
        )
        
        # neutral: average both directions
        r_ba <- py_predict$predict_matchup(
          node_features = graph$node_features,
          edge_src      = graph$edge_src,
          edge_dst      = graph$edge_dst,
          edge_weights  = graph$edge_weights,
          team_a_idx    = idx_b,
          team_b_idx    = idx_a,
          feat_vec_a    = fv_b,
          feat_vec_b    = fv_a,
          is_neutral    = 1L
        )
        r_ab <- py_predict$predict_matchup(
          node_features = graph$node_features,
          edge_src      = graph$edge_src,
          edge_dst      = graph$edge_dst,
          edge_weights  = graph$edge_weights,
          team_a_idx    = idx_a,
          team_b_idx    = idx_b,
          feat_vec_a    = fv_a,
          feat_vec_b    = fv_b,
          is_neutral    = 1L
        )
        wp_neutral    <- (r_ab$win_prob_a + r_ba$win_prob_b) / 2
        score_a_neut  <- (r_ab$pred_score_a + r_ba$pred_score_b) / 2
        score_b_neut  <- (r_ab$pred_score_b + r_ba$pred_score_a) / 2
        
        cache[[key]] <- list(
          team_a = team_a,
          team_b = team_b,
          # a_home results
          a_home = list(
            pred_score_a = r_home$pred_score_a,
            pred_score_b = r_home$pred_score_b,
            win_prob_a   = r_home$win_prob_a,
            win_prob_b   = r_home$win_prob_b,
            margin       = r_home$margin
          ),
          # neutral results
          neutral = list(
            pred_score_a = score_a_neut,
            pred_score_b = score_b_neut,
            win_prob_a   = wp_neutral,
            win_prob_b   = 1 - wp_neutral,
            margin       = score_a_neut - score_b_neut
          )
        )
      }, error = function(e) {
        message(glue("  Failed: {team_a} vs {team_b}: {e$message}"))
      })
      
      done <- done + 1
      if (done %% 1000 == 0)
        message(glue("  {done}/{total} predictions complete ",
                     "({round(done/total*100, 1)}%)..."))
    }
  }
  
  message(glue("Saving {length(cache)} cached predictions..."))
  dir.create("data", showWarnings = FALSE)
  saveRDS(cache, "data/predictions_cache.rds")
  message("Cache saved to data/predictions_cache.rds")
  invisible(cache)
}

# ── Load cache ────────────────────────────────────────────────────────────────
load_prediction_cache <- function() {
  path <- "data/predictions_cache.rds"
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

# ── Look up a cached prediction ───────────────────────────────────────────────
lookup_cached_prediction <- function(team_a, team_b, location, cache) {
  if (is.null(cache)) return(NULL)
  
  # For b_home: team_b is home = flip the key and use a_home result
  if (location == "b_home") {
    key    <- paste0(team_b, "|||", team_a)
    result <- cache[[key]]
    if (is.null(result)) return(NULL)
    # Flip scores back so team_a and team_b labels are correct
    g <- result$a_home
    return(list(
      pred_score_a = g$pred_score_b,
      pred_score_b = g$pred_score_a,
      win_prob_a   = g$win_prob_b,
      win_prob_b   = g$win_prob_a,
      margin       = -g$margin
    ))
  }
  
  key    <- paste0(team_a, "|||", team_b)
  result <- cache[[key]]
  if (is.null(result)) return(NULL)
  
  if (location == "a_home") return(result$a_home)
  if (location == "neutral") return(result$neutral)
  NULL
}