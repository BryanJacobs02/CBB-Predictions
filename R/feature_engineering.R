# ── Season-level static data fetchers (cached per season) ─────────────────────
get_four_factors_season <- memoise(function(year) {
  kenpom_get("four-factors", y = year)
}, cache = cache)

get_misc_stats_season <- memoise(function(year) {
  kenpom_get("misc-stats", y = year)
}, cache = cache)

get_height_season <- memoise(function(year) {
  kenpom_get("height", y = year)
}, cache = cache)

get_point_dist_season <- memoise(function(year) {
  kenpom_get("pointdist", y = year)
}, cache = cache)

get_conf_ratings_season <- memoise(function(year) {
  kenpom_get("conf-ratings", y = year)
}, cache = cache)

get_teams_season <- memoise(function(year) {
  kenpom_get("teams", y = year)
}, cache = cache)

# ── Build static feature tibble for a specific season ─────────────────────────
build_static_features_for_season <- function(year) {
  ff       <- get_four_factors_season(year)
  misc     <- get_misc_stats_season(year)
  height   <- get_height_season(year)
  pdist    <- get_point_dist_season(year)
  conf_rtg <- get_conf_ratings_season(year)
  teams    <- get_teams_season(year) |> select(TeamName, ConfShort)
  
  tibble(TeamName = teams$TeamName |> unique()) |>
    left_join(
      ff |> select(TeamName, eFG_Pct, TO_Pct, OR_Pct, FT_Rate,
                   DeFG_Pct, DTO_Pct, DOR_Pct, DFT_Rate) |>
        distinct(TeamName, .keep_all = TRUE),
      by = "TeamName") |>
    left_join(
      misc |> select(TeamName, FG3Pct, FG2Pct, FTPct, BlockPct,
                     StlRate, ARate, F3GRate,
                     OppFG3Pct, OppBlockPct, OppStlRate) |>
        distinct(TeamName, .keep_all = TRUE),
      by = "TeamName") |>
    left_join(
      height |> select(TeamName, AvgHgt, HgtEff, Exp, Bench, Continuity) |>
        distinct(TeamName, .keep_all = TRUE),
      by = "TeamName") |>
    left_join(
      pdist |> select(TeamName, OffFt, OffFg2, OffFg3,
                      DefFt, DefFg2, DefFg3) |>
        distinct(TeamName, .keep_all = TRUE),
      by = "TeamName") |>
    left_join(
      teams |> distinct(TeamName, .keep_all = TRUE),
      by = "TeamName") |>
    left_join(
      conf_rtg |> select(ConfShort, Rating) |>
        distinct(ConfShort, .keep_all = TRUE) |>
        rename(ConfRating = Rating),
      by = "ConfShort") |>
    select(-ConfShort) |>
    distinct(TeamName, .keep_all = TRUE)
}

# ── Current feature matrix (used for live prediction) ─────────────────────────
build_feature_matrix <- function(year = SEASON_YEAR) {
  ratings <- get_ratings(year)
  static  <- build_static_features_for_season(year)
  
  df <- ratings |>
    select(TeamName, AdjEM, AdjOE, AdjDE, AdjTempo) |>
    distinct(TeamName, .keep_all = TRUE) |>
    left_join(
      static |> distinct(TeamName, .keep_all = TRUE),
      by = "TeamName")
  
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
  
  # Drop columns not available in archive snapshots
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
    replace_na(as.list(setNames(rep(0, length(numeric_cols)), numeric_cols))) |>
    distinct(TeamName, .keep_all = TRUE)
  
  df
}

# ── Historical feature matrix (leak-free, per season static + weekly archive) ──
build_historical_feature_matrix <- function(seasons = c(SEASON_YEAR - 2,
                                                        SEASON_YEAR - 1,
                                                        SEASON_YEAR)) {
  # Generate weekly archive dates per season
  season_date_ranges <- map_dfr(seasons, function(s) {
    start <- as.Date(glue("{s - 1}-11-01"))
    end   <- as.Date(glue("{s}-04-15"))
    end   <- min(end, Sys.Date() - 1)
    if (start >= end) return(tibble())
    tibble(snap_date = seq(start, end, by = "week"), season = s)
  })
  
  if (nrow(season_date_ranges) == 0) stop("No valid archive date ranges.")
  
  message(glue("Fetching {nrow(season_date_ranges)} weekly archive snapshots..."))
  
  # Pre-fetch season-level static data for each season — no leakage
  message("Fetching season-level static features (no leakage)...")
  season_static <- map(seasons, function(s) {
    message(glue("  Static features for season {s}..."))
    tryCatch(
      build_static_features_for_season(s),
      error = function(e) {
        message(glue("  Failed season {s}: {e$message}"))
        NULL
      }
    )
  })
  names(season_static) <- as.character(seasons)
  
  # Fetch weekly archive snapshots
  snapshots <- list()
  dates     <- season_date_ranges$snap_date
  seasons_for_date <- season_date_ranges$season
  
  for (i in seq_along(dates)) {
    d <- as.character(dates[i])
    s <- seasons_for_date[i]
    static_df <- season_static[[as.character(s)]]
    
    if (is.null(static_df)) next
    
    tryCatch({
      arch <- get_archive(d) |>
        select(TeamName, AdjEM, AdjOE, AdjDE, AdjTempo) |>
        rename(
          AdjEM_snap    = AdjEM,
          AdjOE_snap    = AdjOE,
          AdjDE_snap    = AdjDE,
          AdjTempo_snap = AdjTempo
        )
      
      # Momentum from 4 weeks prior
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
      
      # Join season-specific static features with point-in-time archive ratings
      snap_df <- static_df |>
        left_join(momentum, by = "TeamName") |>
        rename(
          AdjEM    = AdjEM_snap,
          AdjOE    = AdjOE_snap,
          AdjDE    = AdjDE_snap,
          AdjTempo = AdjTempo_snap
        ) |>
        distinct(TeamName, .keep_all = TRUE)
      
      # Normalize within snapshot
      numeric_cols <- names(snap_df)[sapply(snap_df, function(col) {
        is.numeric(col) && !is.list(col)
      })]
      
      snap_df <- snap_df |>
        mutate(across(all_of(numeric_cols), ~ {
          mn <- min(.x, na.rm = TRUE)
          mx <- max(.x, na.rm = TRUE)
          if (mx == mn) 0 else (.x - mn) / (mx - mn)
        })) |>
        replace_na(as.list(setNames(rep(0, length(numeric_cols)), numeric_cols))) |>
        distinct(TeamName, .keep_all = TRUE)
      
      snapshots[[d]] <- snap_df
      
      if (i %% 10 == 0)
        message(glue("  {i}/{length(dates)} snapshots fetched..."))
      
    }, error = function(e) {
      message(glue("  Snapshot {d} failed: {e$message}"))
    })
  }
  
  message(glue("Built {length(snapshots)} weekly snapshots (leak-free static features)."))
  snapshots
}

# ── Lookup: nearest snapshot for a given game date ────────────────────────────
get_snapshot_for_date <- function(game_date, snapshots) {
  snap_dates <- as.Date(names(snapshots))
  game_date  <- as.Date(game_date)
  valid      <- snap_dates[snap_dates <= game_date]
  if (length(valid) == 0) return(NULL)
  as.character(max(valid))
}