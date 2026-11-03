# ── Lockbot 2.0 prediction cache (written by lockbot2/make_cache.py) ─────────
# data/predictions_v2.csv has one row per ordered pair (team_a, team_b) with margin, total
# and win probability for "a_home" and "neutral". Lookups return the same list shape the
# 1.0 app expects, plus `total`, so existing UI code keeps working.

load_prediction_cache_v2 <- function(path = "data/predictions_v2.csv") {
  if (!file.exists(path)) return(NULL)
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  df$key <- paste0(df$team_a, "|||", df$team_b)
  meta_path <- sub("\\.csv$", ".meta.json", path)
  meta <- if (file.exists(meta_path)) jsonlite::fromJSON(meta_path) else list(sigma = 11.46)
  list(df = df, idx = stats::setNames(seq_len(nrow(df)), df$key), meta = meta)
}

.cache_v2_row <- function(cache, a, b, loc) {
  i <- cache$idx[paste0(a, "|||", b)]
  if (is.na(i)) return(NULL)
  r <- cache$df[i, ]
  m <- r[[paste0("margin_", loc)]]
  t <- r[[paste0("total_", loc)]]
  w <- r[[paste0("win_prob_", loc)]]
  list(pred_score_a = (t + m) / 2, pred_score_b = (t - m) / 2,
       win_prob_a = w, win_prob_b = 1 - w, margin = m, total = t)
}

lookup_cached_prediction_v2 <- function(team_a, team_b, location, cache) {
  if (is.null(cache)) return(NULL)
  if (location == "b_home") {
    g <- .cache_v2_row(cache, team_b, team_a, "a_home")
    if (is.null(g)) return(NULL)
    return(list(pred_score_a = g$pred_score_b, pred_score_b = g$pred_score_a,
                win_prob_a = g$win_prob_b, win_prob_b = g$win_prob_a,
                margin = -g$margin, total = g$total))
  }
  .cache_v2_row(cache, team_a, team_b, if (location == "a_home") "a_home" else "neutral")
}
