build_feature_matrix <- function(year = SEASON_YEAR) {
  ratings  <- get_ratings(year)
  ff       <- get_four_factors(year)
  misc     <- get_misc_stats(year)
  height   <- get_height(year)
  pdist    <- get_point_dist(year)
  conf_rtg <- get_conf_ratings(year)
  teams    <- get_teams(year) |> select(TeamName, ConfShort)
  
  df <- ratings |>
    select(TeamName, AdjEM, AdjOE, AdjDE, AdjTempo, Luck,
           SOS, NCSOS, Pythag, APL_Off, APL_Def) |>
    left_join(
      ff |> select(TeamName, eFG_Pct, TO_Pct, OR_Pct, FT_Rate,
                   DeFG_Pct, DTO_Pct, DOR_Pct, DFT_Rate),
      by = "TeamName") |>
    left_join(
      misc |> select(TeamName, FG3Pct, FG2Pct, FTPct, BlockPct,
                     StlRate, ARate, F3GRate,
                     OppFG3Pct, OppBlockPct, OppStlRate),
      by = "TeamName") |>
    left_join(
      height |> select(TeamName, AvgHgt, HgtEff, Exp, Bench, Continuity),
      by = "TeamName") |>
    left_join(
      pdist |> select(TeamName, OffFt, OffFg2, OffFg3,
                      DefFt, DefFg2, DefFg3),
      by = "TeamName") |>
    left_join(teams, by = "TeamName") |>
    left_join(
      conf_rtg |> select(ConfShort, Rating) |> rename(ConfRating = Rating),
      by = "ConfShort") |>
    select(-ConfShort)
  
  # ── Momentum: AdjEM change over last 30 days ─────────────────────────────
  today    <- Sys.Date()
  momentum <- tryCatch({
    now_arch  <- get_archive(as.character(today)) |>
      select(TeamName, AdjEM) |> rename(AdjEM_now = AdjEM)
    past_arch <- get_archive(as.character(today - 30)) |>
      select(TeamName, AdjEM) |> rename(AdjEM_30 = AdjEM)
    left_join(now_arch, past_arch, by = "TeamName") |>
      mutate(Momentum = AdjEM_now - AdjEM_30) |>
      select(TeamName, Momentum)
  }, error = function(e) {
    message("Momentum unavailable: ", e$message)
    tibble(TeamName = df$TeamName, Momentum = 0)
  })
  
  df <- df |> left_join(momentum, by = "TeamName")
  
  # ── Normalize all numeric features to [0, 1] ──────────────────────────────
  numeric_cols <- df |> select(where(is.numeric)) |> names()
  df <- df |>
    mutate(across(all_of(numeric_cols), ~ {
      mn <- min(.x, na.rm = TRUE)
      mx <- max(.x, na.rm = TRUE)
      if (mx == mn) 0 else (.x - mn) / (mx - mn)
    })) |>
    replace_na(as.list(setNames(rep(0, length(numeric_cols)), numeric_cols)))
  
  df
}