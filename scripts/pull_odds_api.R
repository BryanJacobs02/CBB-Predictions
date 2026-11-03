# ─────────────────────────────────────────────────────────────────────────────
# pull_odds_api.R  —  historical DraftKings spreads/totals from The Odds API (paid plan)
#
# GOAL: closing-ish lines for 2022-2025 so we can test whether the model beats the market over
# 4 more seasons (right now we only have 2026). Lines are saved raw; lockbot2/odds_api_merge.py
# turns them into the same columns the rest of the pipeline uses.
#
# SET UP   1. Make an account at https://the-odds-api.com and put the key in .Renviron:
#             ODDS_API_KEY=your_key_here          (then restart R)
#          2. Historical odds need a PAID plan (the free plan does not include them).
#
# USE      Rscript scripts/pull_odds_api.R estimate   # prints the credit cost, calls nothing
#          Rscript scripts/pull_odds_api.R probe      # ~3 snapshots, ~60 credits, checks coverage
#          Rscript scripts/pull_odds_api.R run        # the real pull (resumable; stop/restart any time)
#
# COST     Each snapshot = 10 credits x markets x regions. With bookmakers=draftkings and
#          markets = spreads,totals that is 20 credits per snapshot. A snapshot returns every game
#          that has odds at that moment, so we take SNAPSHOTS_PER_DAY snapshots on each game day and
#          later keep, for every game, the last snapshot before tip-off (a closing-line proxy).
# ─────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({ library(httr2); library(jsonlite); library(glue); library(purrr); library(dplyr) })

SEASONS            <- 2025:2025              # start with ONE season; add more once the probe looks good
SNAPSHOT_TIMES_UTC <- c("16:30", "21:00", "23:45")   # ~11:30am, 4pm, 6:45pm Eastern (winter)
MARKETS            <- "spreads,totals"
BOOKMAKERS         <- "draftkings"
STOP_BELOW_CREDITS <- 300                    # stop the run if the account falls below this
OUT <- "lockbot_oddsapi"; dir.create(file.path(OUT, "raw"), recursive = TRUE, showWarnings = FALSE)

mode <- if (length(commandArgs(TRUE)) > 0) commandArgs(TRUE)[1] else "estimate"
KEY  <- Sys.getenv("ODDS_API_KEY")
credits_per <- 10 * length(strsplit(MARKETS, ",")[[1]]) * ceiling(length(strsplit(BOOKMAKERS, ",")[[1]]) / 10)

season_days <- function(s) seq(as.Date(glue("{s - 1}-11-04")), as.Date(glue("{s}-04-08")), by = "day")
all_stamps <- unlist(map(SEASONS, \(s) {
  d <- season_days(s)
  as.vector(outer(format(d), SNAPSHOT_TIMES_UTC, \(a, b) glue("{a}T{b}:00Z")))
}))
# Games after ~7pm ET fall on the next UTC day; the 23:45Z snapshot still precedes most of them.

if (mode == "estimate") {
  n <- length(all_stamps)
  message(glue("{length(SEASONS)} season(s), {n} snapshots x {credits_per} credits = {format(n * credits_per, big.mark = ',')} credits"))
  message("Plans reported by a third-party review (verify on the site): $30/mo = 20,000 credits, $59/mo = 100,000.")
  message("Change SNAPSHOT_TIMES_UTC to take fewer snapshots per day (cheaper, less accurate closing lines).")
  quit(save = "no")
}
if (!nzchar(KEY)) stop("ODDS_API_KEY is empty. Add it to .Renviron and restart R.")

fetch_snapshot <- function(stamp) {
  f <- file.path(OUT, "raw", paste0(gsub("[:]", "", stamp), ".json"))
  if (file.exists(f)) return(list(skipped = TRUE))
  resp <- request("https://api.the-odds-api.com/v4/historical/sports/basketball_ncaab/odds") |>
    req_url_query(apiKey = KEY, regions = "us", markets = MARKETS, bookmakers = BOOKMAKERS,
                  oddsFormat = "american", date = stamp) |>
    req_error(is_error = \(r) FALSE) |> req_perform()
  hdr <- \(h) suppressWarnings(as.numeric(resp_header(resp, h) %||% NA))
  if (resp_status(resp) != 200) {
    message(glue("  ! {stamp}: HTTP {resp_status(resp)}  {substr(resp_body_string(resp), 1, 200)}"))
    return(list(error = TRUE, status = resp_status(resp), remaining = hdr("x-requests-remaining")))
  }
  writeLines(resp_body_string(resp), f)
  Sys.sleep(0.4)
  list(remaining = hdr("x-requests-remaining"), last = hdr("x-requests-last"))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

if (mode == "probe") {
  stamps <- c("2025-02-08T21:00:00Z", "2025-02-22T23:45:00Z", "2024-12-14T21:00:00Z")
  for (s in stamps) {
    r <- fetch_snapshot(s)
    f <- file.path(OUT, "raw", paste0(gsub("[:]", "", s), ".json"))
    if (file.exists(f)) {
      j <- fromJSON(f, simplifyVector = FALSE)
      n_ev <- length(j$data); n_dk <- sum(map_lgl(j$data, \(e) length(e$bookmakers) > 0))
      message(glue("{s}: {n_ev} games, {n_dk} with DraftKings lines | credits left {r$remaining %||% '?'}, this call cost {r$last %||% '?'}"))
    }
  }
  message("Probe done. Paste these lines to Claude. Expect roughly 20-60 games per snapshot, most with DraftKings lines.")
  quit(save = "no")
}

if (mode == "run") {
  todo <- all_stamps[!file.exists(file.path(OUT, "raw", paste0(gsub("[:]", "", all_stamps), ".json")))]
  message(glue("{length(todo)} snapshots to fetch ({length(all_stamps) - length(todo)} already saved)"))
  for (i in seq_along(todo)) {
    r <- fetch_snapshot(todo[i])
    if (isTRUE(r$error) && r$status %in% c(401, 402, 429)) { message("Stopping: account/quota problem."); break }
    if (!is.null(r$remaining) && !is.na(r$remaining) && r$remaining < STOP_BELOW_CREDITS) {
      message(glue("Stopping: only {r$remaining} credits left.")); break }
    if (i %% 25 == 0) message(glue("  {i}/{length(todo)} done, credits left: {r$remaining %||% '?'}"))
  }
  message("Finished (re-run to resume). Next: python3 lockbot2/odds_api_merge.py")
}
