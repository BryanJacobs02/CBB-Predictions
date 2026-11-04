# ─────────────────────────────────────────────────────────────────────────────
# refresh_data.R  —  pulls the CURRENT season's KenPom + ESPN data into lockbot_export/
#
# Run from the project root (the folder with app.R), after the one-time export has created
# lockbot_export/ :
#     Rscript scripts/refresh_data.R           (or  source("scripts/refresh_data.R")  in RStudio)
#
# What it does (only for the current season; older seasons are left alone):
#   * ESPN schedule/results           -> lockbot_export/schedule_extra.csv
#   * KenPom team list                -> lockbot_export/kp_teams.csv
#   * KenPom weekly archive snapshots -> lockbot_export/kp_archive_weekly.csv   (+ one snapshot of TODAY's ratings)
#   * KenPom ratings (season so far)  -> lockbot_export/kp_ratings.csv          (final values once the season ends)
# API calls for past dates are cached in lockbot_export/cache_refresh/, so reruns are fast.
# Then run:   python3 lockbot2/refresh.py          (or use scripts/refresh.sh for both steps)
# ─────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(lubridate); library(glue); library(hoopR); library(stringdist)
})

OUT          <- "lockbot_export"
PAUSE_SEC    <- 0.4
SNAP_EVERY   <- 7
KENPOM_BASE  <- "https://kenpom.com/api.php"
KENPOM_KEY   <- Sys.getenv("KENPOM_API_KEY")
if (!nzchar(KENPOM_KEY)) stop("KENPOM_API_KEY not found. Run from the project folder so .Renviron loads.")
if (!dir.exists(OUT)) stop("lockbot_export/ not found. Run scripts/export_lockbot_data.R once first (or unzip lockbot_export.zip here).")

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
SEASON <- as.integer(Sys.getenv("LOCKBOT_SEASON", unset = if (month(Sys.Date()) >= 11) CURRENT_YEAR + 1 else CURRENT_YEAR))
message(glue("Refreshing season {SEASON} ({SEASON - 1}-{substr(SEASON, 3, 4)})"))

source("R/kenpom_api.R")   # kenpom_get()

CACHE <- file.path(OUT, "cache_refresh"); dir.create(CACHE, showWarnings = FALSE)
cached_call <- function(key, fn, refresh = FALSE) {
  f <- file.path(CACHE, paste0(key, ".rds"))
  if (!refresh && file.exists(f)) return(readRDS(f))
  val <- tryCatch(fn(), error = function(e) { message(glue("  ! {key}: {conditionMessage(e)}")); NULL })
  if (!is.null(val)) saveRDS(val, f)
  Sys.sleep(PAUSE_SEC)
  val
}

# Replace this season's rows in an existing CSV with fresh ones (everything stored as text so types never clash)
replace_season <- function(file, fresh) {
  path  <- file.path(OUT, file)
  fresh <- fresh |> mutate(across(where(is.list), ~ vapply(.x, toString, character(1)))) |>
    mutate(across(everything(), as.character))
  old <- if (file.exists(path)) read.csv(path, colClasses = "character", check.names = FALSE) else NULL
  if (!is.null(old)) old <- old[old$season != as.character(SEASON), , drop = FALSE]
  out <- bind_rows(old, fresh)
  write.csv(out, path, row.names = FALSE)
  message(glue("  {file}: {nrow(fresh)} fresh rows for {SEASON}, {nrow(out)} total"))
}

# ── 1. ESPN schedule + results ───────────────────────────────────────────────
message("\n[1/4] ESPN schedule and results")
raw <- hoopR::load_mbb_schedule(seasons = SEASON)
sched <- raw |> mutate(game_id = as.character(game_id)) |>
  select(any_of(c(
    "game_id", "game_date", "season", "season_type", "neutral_site",
    "conference_competition", "tournament_id", "notes_headline", "attendance",
    "home_id", "away_id", "home_display_name", "away_display_name",
    "home_score", "away_score", "home_conference_id", "away_conference_id",
    "home_current_rank", "away_current_rank", "status_type_completed")))
replace_season("schedule_extra.csv", sched)
message(glue("  completed games so far: {sum(sched$status_type_completed %in% c(TRUE, 'TRUE'), na.rm = TRUE)}"))

# ── 2. KenPom team list ──────────────────────────────────────────────────────
message("\n[2/4] KenPom teams")
teams <- cached_call(glue("teams_{SEASON}"), \() kenpom_get("teams", y = SEASON), refresh = TRUE)
if (!is.null(teams)) replace_season("kp_teams.csv", mutate(teams, season = SEASON))

# ── 3. KenPom archive snapshots (weekly) + today's ratings ───────────────────
message("\n[3/4] KenPom archive snapshots")
dates <- seq(as.Date(glue("{SEASON - 1}-11-01")), as.Date(glue("{SEASON}-04-15")), by = SNAP_EVERY)
dates <- dates[dates < Sys.Date()]
snaps <- map_dfr(dates, function(d) {
  a <- cached_call(glue("archive_{d}"), \() kenpom_get("archive", d = as.character(d)))
  if (is.null(a) || nrow(a) == 0) tibble() else mutate(a, snap_date = as.character(d), season = SEASON)
})
# today's ratings, saved as a snapshot dated today (kept in the cache, so snapshots accumulate run by run)
live <- cached_call(glue("live_{Sys.Date()}"), \() kenpom_get("ratings", y = SEASON), refresh = TRUE)
live_files <- list.files(CACHE, pattern = "^live_.*\\.rds$", full.names = TRUE)
live_all <- map_dfr(live_files, function(f) {
  x <- readRDS(f)
  if (is.null(x) || nrow(x) == 0) tibble() else
    mutate(x, snap_date = sub("^live_(.*)\\.rds$", "\\1", basename(f)), season = SEASON)
})
snaps <- bind_rows(snaps, live_all)
if (nrow(snaps) > 0) replace_season("kp_archive_weekly.csv", snaps) else message("  no snapshots yet (preseason)")

# ── 4. KenPom ratings so far ─────────────────────────────────────────────────
message("\n[4/4] KenPom ratings")
if (!is.null(live)) replace_season("kp_ratings.csv", mutate(live, season = SEASON))

# ── 5. KenPom's preseason ratings for this season (pulled once; they never change) ──
pre_file <- file.path(OUT, "kp_preseason.csv")
have_pre <- if (file.exists(pre_file)) unique(read.csv(pre_file)$season) else integer()
if (!(SEASON %in% have_pre)) {
  message("\n[5/5] KenPom preseason ratings")
  pre <- tryCatch(kenpom_get("archive", y = SEASON, preseason = "true"), error = function(e) { message("  ! ", conditionMessage(e)); NULL })
  if (!is.null(pre) && nrow(pre) > 50) replace_season("kp_preseason.csv", mutate(pre, season = SEASON, pull_method = 1L))
}

message(glue("\nDone. Next:  python3 lockbot2/refresh.py"))
