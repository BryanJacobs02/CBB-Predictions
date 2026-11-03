# ─────────────────────────────────────────────────────────────────────────────
# export_lockbot_data.R  —  collects the data Claude needs for Lockbot 2.0
#
# HOW TO RUN
#   1. Open the Lockbot project in RStudio (so .Renviron / renv load and your
#      KENPOM_API_KEY is available).
#   2. Save this file in the project root (next to app.R).
#   3. In the console:  source("export_lockbot_data.R")
#   4. When it finishes it creates  lockbot_export.zip  -> attach that to the chat.
#
# It is safe to stop and re-run: every API call is cached to disk in
# lockbot_export/cache/, so a re-run only fetches what is still missing.
# ─────────────────────────────────────────────────────────────────────────────

# ── Settings you may want to change ──────────────────────────────────────────
SEASONS          <- 2022:2026   # season = year it ends in (2026 = 2025-26)
SNAP_EVERY_DAYS  <- 7           # spacing of KenPom archive snapshots
FANMATCH_SEASONS <- 2026        # KenPom's own daily predictions (benchmark);
                                #   use NULL to skip, ~160 API calls per season
ODDS_TEST_GAMES  <- 25          # test how well ESPN odds cover your games
PAUSE_SEC        <- 0.4         # polite pause between API calls
OUT              <- "lockbot_export"

# ── Setup ────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(lubridate); library(glue); library(hoopR); library(stringdist)
})

KENPOM_BASE  <- "https://kenpom.com/api.php"
KENPOM_KEY   <- Sys.getenv("KENPOM_API_KEY")
if (!nzchar(KENPOM_KEY)) stop("KENPOM_API_KEY not found. Open the .Rproj so .Renviron loads.")
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
SEASON_YEAR  <- if (month(Sys.Date()) >= 11) CURRENT_YEAR + 1 else CURRENT_YEAR

source("R/kenpom_api.R")      # kenpom_get()
source("R/game_results.R")    # load_game_results(), build_name_crosswalk()

dir.create(file.path(OUT, "cache"), recursive = TRUE, showWarnings = FALSE)

write_out <- function(df, name) {
  df <- df |> mutate(across(where(is.list), ~ vapply(.x, toString, character(1))))
  write.csv(df, file.path(OUT, name), row.names = FALSE)
  message(glue("  wrote {name}  ({nrow(df)} rows, {ncol(df)} cols)"))
  invisible(df)
}

# Disk-cached API call: re-runs skip anything already fetched
cached_call <- function(key, fn) {
  f <- file.path(OUT, "cache", paste0(key, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  val <- tryCatch(fn(), error = function(e) {
    message(glue("  ! {key}: {conditionMessage(e)}")); NULL })
  if (!is.null(val)) saveRDS(val, f)
  Sys.sleep(PAUSE_SEC)
  val
}

season_dates <- function(s, by_days) {
  d <- seq(as.Date(glue("{s - 1}-11-01")), as.Date(glue("{s}-04-15")), by = by_days)
  d[d < Sys.Date()]
}

# ── 1. Game results (same loader Lockbot 1.0 trains on) ──────────────────────
message("\n[1/7] Game results (hoopR / ESPN)")
results <- load_game_results(SEASONS)
write_out(results, "game_results.csv")

# Extra schedule columns (season type, tournament, conference game, etc.)
sched <- map_dfr(SEASONS, function(s) {
  raw <- cached_call(glue("schedule_{s}"), \() hoopR::load_mbb_schedule(seasons = s))
  if (is.null(raw)) return(tibble())
  writeLines(names(raw), file.path(OUT, "schedule_columns.txt"))
  raw |> mutate(game_id = as.character(game_id)) |>
    select(any_of(c(
      "game_id", "game_date", "season", "season_type", "neutral_site",
      "conference_competition", "tournament_id", "notes_headline", "attendance",
      "home_id", "away_id", "home_display_name", "away_display_name",
      "home_score", "away_score", "home_conference_id", "away_conference_id",
      "home_current_rank", "away_current_rank", "status_type_completed")))
})
write_out(sched, "schedule_extra.csv")

# ── 2. Teams / conferences per season ────────────────────────────────────────
message("\n[2/7] Teams per season")
teams <- map_dfr(SEASONS, function(s) {
  t <- cached_call(glue("teams_{s}"), \() kenpom_get("teams", y = s))
  if (is.null(t)) tibble() else mutate(t, season = s)
})
write_out(teams, "kp_teams.csv")

# ── 3. Name crosswalk ESPN <-> KenPom (needed to join odds later) ────────────
message("\n[3/7] Team-name crosswalk")
cw <- build_name_crosswalk(sort(unique(teams$TeamName)),
                           unique(c(results$home_team, results$away_team)))
write_out(cw, "name_crosswalk.csv")

# ── 4. KenPom weekly archive snapshots (point-in-time ratings) ───────────────
message("\n[4/7] KenPom weekly archive snapshots")
snaps <- map_dfr(SEASONS, function(s) {
  map_dfr(season_dates(s, SNAP_EVERY_DAYS), function(d) {
    a <- cached_call(glue("archive_{d}"), \() kenpom_get("archive", d = as.character(d)))
    if (is.null(a)) tibble() else mutate(a, snap_date = as.character(d), season = s)
  })
})
write_out(snaps, "kp_archive_weekly.csv")

# ── 5. Season-level KenPom tables (END-of-season values) ─────────────────────
message("\n[5/7] Season-level KenPom tables")
for (ep in c("ratings", "four-factors", "misc-stats", "height", "pointdist", "conf-ratings")) {
  tbl <- map_dfr(SEASONS, function(s) {
    x <- cached_call(glue("{ep}_{s}"), \() kenpom_get(ep, y = s))
    if (is.null(x)) tibble() else mutate(x, season = s)
  })
  write_out(tbl, glue("kp_{gsub('-', '_', ep)}.csv"))
}

# ── 6. KenPom fanmatch (his own game predictions = benchmark) ────────────────
message("\n[6/7] KenPom fanmatch predictions")
if (length(FANMATCH_SEASONS) > 0) {
  fm <- map_dfr(FANMATCH_SEASONS, function(s) {
    map_dfr(season_dates(s, 1), function(d) {
      x <- cached_call(glue("fanmatch_{d}"), \() kenpom_get("fanmatch", d = as.character(d)))
      if (is.null(x) || nrow(x) == 0) tibble() else mutate(x, fm_date = as.character(d), season = s)
    })
  })
  write_out(fm, "kp_fanmatch.csv")
} else message("  skipped")

# ── 7. Odds coverage test (ESPN pickcenter via hoopR) ────────────────────────
message("\n[7/7] ESPN odds coverage test")
if (ODDS_TEST_GAMES > 0) {
  set.seed(1)
  test_ids <- results |>
    filter(game_date >= as.Date(glue("{max(SEASONS) - 1}-12-01")),
           game_date <= as.Date(glue("{max(SEASONS)}-03-10"))) |>
    slice_sample(n = ODDS_TEST_GAMES) |> pull(game_id)
  odds <- map_dfr(test_ids, function(id) {
    r <- cached_call(glue("odds_{id}"), \() hoopR::espn_mbb_betting(game_id = id))
    pc <- if (is.null(r)) NULL else r$pickcenter
    if (is.null(pc) || nrow(pc) == 0) tibble() else mutate(as_tibble(pc), game_id = id)
  })
  if (nrow(odds) > 0) {
    write_out(odds, "odds_test_pickcenter.csv")
    message(glue("  games with any odds row: {n_distinct(odds$game_id)} of {length(test_ids)}"))
  } else {
    message(glue("  NO odds returned for any of {length(test_ids)} test games"))
  }
}

# ── Existing Lockbot 1.0 artifacts (if present on this machine) ──────────────
message("\nCopying Lockbot 1.0 model artifacts (if present)")
v1 <- file.path(OUT, "v1_artifacts"); dir.create(v1, showWarnings = FALSE)
for (f in c(list.files("data/models", full.names = TRUE), "data/predictions_cache.rds")) {
  if (file.exists(f) && file.size(f) < 60e6) file.copy(f, v1, overwrite = TRUE)
}

# ── Wrap up ──────────────────────────────────────────────────────────────────
sink(file.path(OUT, "session_info.txt")); print(sessionInfo()); sink()
unlink(file.path(OUT, "cache"), recursive = TRUE)     # keep zip small

files <- list.files(OUT, recursive = TRUE, full.names = TRUE)
man <- data.frame(path = files, size_mb = round(file.size(files) / 1e6, 2))
write.csv(man, file.path(OUT, "manifest.csv"), row.names = FALSE)
print(man)

ok <- tryCatch({ zip(paste0(OUT, ".zip"), files = list.files(OUT, full.names = TRUE, recursive = TRUE)); TRUE },
               error = function(e) FALSE, warning = function(w) FALSE)
if (ok && file.exists(paste0(OUT, ".zip"))) {
  message(glue("\nDONE. Attach {OUT}.zip ({round(file.size(paste0(OUT, '.zip'))/1e6, 1)} MB) to the chat."))
} else {
  message(glue("\nDONE. Zip failed on this machine: right-click the '{OUT}' folder and compress it, then attach the zip."))
}
