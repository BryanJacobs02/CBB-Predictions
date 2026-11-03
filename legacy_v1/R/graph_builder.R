build_team_graph <- function(feature_matrix, year = SEASON_YEAR) {
  teams      <- get_teams(year)
  team_names <- feature_matrix$TeamName
  n          <- nrow(feature_matrix)
  idx        <- setNames(seq_len(n) - 1L, team_names)
  
  # ── Edge set 1: same conference ─────────────────────────────────────────
  conf_edges <- teams |>
    distinct(TeamName, ConfShort) |>
    inner_join(
      teams |> distinct(TeamName, ConfShort),
      by = "ConfShort", suffix = c("_a", "_b"),
      relationship = "many-to-many"
    ) |>
    filter(TeamName_a != TeamName_b,
           TeamName_a %in% team_names,
           TeamName_b %in% team_names) |>
    transmute(src = idx[TeamName_a], dst = idx[TeamName_b], weight = 1.0)
  
  # ── Edge set 2: recent matchups via fanmatch ─────────────────────────────
  today <- Sys.Date()
  fanmatch_edges <- map_dfr(seq(today - 60, today - 1, by = "day"), function(d) {
    tryCatch({
      fm <- get_fanmatch(as.character(d))
      fm |>
        filter(Visitor %in% team_names, Home %in% team_names) |>
        transmute(
          src    = idx[Visitor],
          dst    = idx[Home],
          weight = abs(HomeWP - 0.5) * 2
        )
    }, error = \(e) tibble())
  })
  
  edges <- bind_rows(conf_edges, fanmatch_edges) |>
    distinct(src, dst, .keep_all = TRUE)
  
  list(
    node_features = feature_matrix |> select(where(is.numeric)) |> as.matrix(),
    team_names    = team_names,
    edge_src      = as.integer(edges$src),
    edge_dst      = as.integer(edges$dst),
    edge_weights  = edges$weight
  )
}