# ── Current feature matrix (used for live prediction) ─────────────────────────
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
  
  # ── Momentum ──────────────────────────────────────────────────────────────
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
  
  # Drop columns not available in archive snapshots to maintain
  # consistent feature dimensions between training and live prediction
  archive_unavailable <- c("SOS", "NCSOS", "Pythag", "APL_Off", "APL_Def", "Luck")
  df <- df |> select(-any_of(archive_unavailable))
  
  # ── Normalize ─────────────────────────────────────────────────────────────
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

# ── Historical feature matrix (used for leak-free training) ───────────────────
# Fetches weekly archive snapshots and builds a time-indexed feature lookup.
# Returns a named list: snapshot_date -> feature tibble
build_historical_feature_matrix <- function(seasons = c(SEASON_YEAR - 2,
                                                        SEASON_YEAR - 1,
                                                        SEASON_YEAR),
                                            year    = SEASON_YEAR) {
  # Generate one date per week for each season
  # NCAA season roughly runs Nov 1 through mid-April
  season_date_ranges <- map_dfr(seasons, function(s) {
    start <- as.Date(glue("{s - 1}-11-01"))
    end   <- as.Date(glue("{s}-04-15"))
    # Clamp end to today — can't fetch future archives
    end   <- min(end, Sys.Date() - 1)
    if (start >= end) return(tibble())
    tibble(
      snap_date = seq(start, end, by = "week"),
      season    = s
    )
  })
  
  if (nrow(season_date_ranges) == 0) stop("No valid archive date ranges.")
  
  message(glue("Fetching {nrow(season_date_ranges)} weekly archive snapshots..."))
  
  # Fetch static data once — height, misc, four factors don't have archive
  # endpoints so we use current season values (minor leakage, unavoidable)
  ff       <- get_four_factors(year)
  misc     <- get_misc_stats(year)
  height   <- get_height(year)
  pdist    <- get_point_dist(year)
  conf_rtg <- get_conf_ratings(year)
  teams    <- get_teams(year) |> select(TeamName, ConfShort)
  
  static_df <- tibble(TeamName = get_teams(year)$TeamName) |>
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
  
  # Fetch each weekly archive snapshot
  snapshots <- list()
  dates     <- season_date_ranges$snap_date
  
  for (i in seq_along(dates)) {
    d <- as.character(dates[i])
    tryCatch({
      arch <- get_archive(d) |>
        select(TeamName, AdjEM, AdjOE, AdjDE, AdjTempo) |>
        rename(
          AdjEM_snap    = AdjEM,
          AdjOE_snap    = AdjOE,
          AdjDE_snap    = AdjDE,
          AdjTempo_snap = AdjTempo
        )
      
      # Momentum: difference from 4 weeks ago (if available)
      prev_date <- as.character(dates[i] - 28)
      momentum <- tryCatch({
        prev_arch <- get_archive(prev_date) |>
          select(TeamName, AdjEM) |> rename(AdjEM_prev = AdjEM)
        arch |>
          left_join(prev_arch, by = "TeamName") |>
          mutate(Momentum = coalesce(AdjEM_snap - AdjEM_prev, 0)) |>
          select(TeamName, AdjEM_snap, AdjOE_snap, AdjDE_snap,
                 AdjTempo_snap, Momentum)
      }, error = function(e) {
        arch |> mutate(Momentum = 0)
      })
      
      # Join with static features
      snap_df <- static_df |>
        distinct(TeamName, .keep_all = TRUE) |>
        left_join(momentum |> distinct(TeamName, .keep_all = TRUE), by = "TeamName") |>
        rename(
          AdjEM    = AdjEM_snap,
          AdjOE    = AdjOE_snap,
          AdjDE    = AdjDE_snap,
          AdjTempo = AdjTempo_snap
        )
      
      # Normalize within this snapshot
      numeric_cols <- snap_df |> select(where(is.numeric)) |> names()
      snap_df <- snap_df |>
        mutate(across(all_of(numeric_cols), ~ {
          mn <- min(.x, na.rm = TRUE)
          mx <- max(.x, na.rm = TRUE)
          if (mx == mn) 0 else (.x - mn) / (mx - mn)
        })) |>
        replace_na(as.list(setNames(rep(0, length(numeric_cols)), numeric_cols)))
      
      snap_df <- snap_df |> distinct(TeamName, .keep_all = TRUE)
      snapshots[[d]] <- snap_df
      if (i %% 10 == 0)
        message(glue("  {i}/{length(dates)} snapshots fetched..."))
      
    }, error = function(e) {
      message(glue("  Snapshot {d} failed: {e$message}"))
    })
  }
  
  message(glue("Built {length(snapshots)} weekly snapshots."))
  snapshots
}

# ── Lookup: find nearest snapshot for a given game date ──────────────────────
get_snapshot_for_date <- function(game_date, snapshots) {
  snap_dates <- as.Date(names(snapshots))
  game_date  <- as.Date(game_date)
  # Find most recent snapshot before the game date
  valid <- snap_dates[snap_dates <= game_date]
  if (length(valid) == 0) return(NULL)
  as.character(max(valid))
}