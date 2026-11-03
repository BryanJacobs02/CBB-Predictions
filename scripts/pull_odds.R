# ─────────────────────────────────────────────────────────────────────────────
# pull_odds.R — pulls DraftKings open/close odds (via ESPN) for every D1 game
#
# HOW TO RUN
#   1. Save in the Lockbot project root (the folder that contains lockbot_export/).
#   2. In RStudio: source("pull_odds.R")
#   3. Takes roughly 40-60 min per season (one request per game). Keep the
#      computer awake. You can stop (Esc) and re-run any time: it resumes.
#   4. When done it creates lockbot_odds.zip -> attach it to the chat.
#
# Start with one season. To add more later, change SEASONS (e.g. 2025:2026)
# and re-run; finished seasons are skipped automatically.
# ─────────────────────────────────────────────────────────────────────────────

SEASONS    <- 2026            # 2026 = the 2025-26 season
PAUSE_SEC  <- 0.3
RETRY_MISSING <- FALSE   # TRUE = re-query games previously recorded as "no odds"
                         # (use after a run where a block of games came back empty)
EXPORT_DIR <- "lockbot_export"
OUT        <- "lockbot_odds"

suppressPackageStartupMessages({ library(dplyr); library(glue); library(hoopR) })
dir.create(OUT, showWarnings = FALSE)

KEEP <- c(
  "game_id", "provider_name", "spread", "over_under", "over_odds", "under_odds",
  "point_spread_home_open_line",  "point_spread_home_open_odds",
  "point_spread_home_close_line", "point_spread_home_close_odds",
  "point_spread_away_open_line",  "point_spread_away_open_odds",
  "point_spread_away_close_line", "point_spread_away_close_odds",
  "moneyline_home_open_odds", "moneyline_home_close_odds",
  "moneyline_away_open_odds", "moneyline_away_close_odds",
  "total_over_open_line",  "total_over_open_odds",
  "total_over_close_line", "total_over_close_odds",
  "total_under_open_line",  "total_under_open_odds",
  "total_under_close_line", "total_under_close_odds"
)

sched <- read.csv(file.path(EXPORT_DIR, "schedule_extra.csv"), stringsAsFactors = FALSE) |>
  filter(status_type_completed %in% c(TRUE, "TRUE"),
         !is.na(home_conference_id), !is.na(away_conference_id))

empty_row <- function(id, has_odds) {
  out <- as.data.frame(setNames(as.list(rep(NA_character_, length(KEEP))), KEEP))
  out$game_id <- id
  out$has_odds <- has_odds
  out
}

# Returns a one-row data.frame, or NULL if the request itself failed (retried next run)
get_row <- function(id) {
  r <- tryCatch(hoopR::espn_mbb_betting(game_id = id), error = function(e) "ERR")
  if (identical(r, "ERR")) return(NULL)
  pc <- r$pickcenter
  if (is.null(pc) || nrow(pc) == 0) return(empty_row(id, FALSE))
  pc <- as.data.frame(pc)[1, , drop = FALSE]
  for (m in setdiff(KEEP, names(pc))) pc[[m]] <- NA
  out <- pc[KEEP]
  out[] <- lapply(out, as.character)
  out$game_id  <- id
  out$has_odds <- TRUE
  out
}

for (s in SEASONS) {
  f    <- file.path(OUT, glue("odds_{s}.csv"))
  ids  <- sched |> filter(season == s) |> pull(game_id) |> as.character() |> unique()
  if (RETRY_MISSING && file.exists(f)) {
    prev   <- read.csv(f, colClasses = "character")
    n_miss <- sum(prev$has_odds != "TRUE")
    if (n_miss > 0) {
      message(glue("  retrying {n_miss} games previously recorded as 'no odds'"))
      write.csv(prev[prev$has_odds == "TRUE", ], f, row.names = FALSE)
    }
  }
  done <- if (file.exists(f)) read.csv(f, colClasses = "character")$game_id else character(0)
  todo <- setdiff(ids, done)
  message(glue("\nSeason {s}: {length(ids)} D1 games, {length(done)} done, {length(todo)} to go"))

  n_new <- 0; n_odds <- 0; consec_err <- 0
  for (id in todo) {
    row <- get_row(id)
    if (is.null(row)) {
      consec_err <- consec_err + 1
      if (consec_err >= 25) { message("Too many consecutive errors (rate limit or offline). Stopping; re-run later."); break }
      next
    }
    consec_err <- 0
    write.table(row, f, sep = ",", row.names = FALSE, qmethod = "double",
                col.names = !file.exists(f), append = file.exists(f))
    n_new <- n_new + 1; n_odds <- n_odds + isTRUE(row$has_odds)

    if (n_new == 60 && n_odds / n_new < 0.5 && length(done) == 0) {
      message(glue("Only {n_odds} of 60 games had odds in season {s}: coverage looks too low, skipping this season."))
      break
    }
    if (n_new %% 250 == 0) message(glue("  {n_new} new games ({round(100 * n_odds / n_new)}% with odds)"))
    Sys.sleep(PAUSE_SEC)
  }
}

files <- list.files(OUT, full.names = TRUE)
ok <- tryCatch({ zip("lockbot_odds.zip", files); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
if (ok) message("\nDONE. Attach lockbot_odds.zip to the chat.") else
  message("\nDONE. Zip failed: right-click the 'lockbot_odds' folder, compress it, and attach the zip.")
