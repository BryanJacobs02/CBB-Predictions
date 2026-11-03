# ─────────────────────────────────────────────────────────────────────────────
# snapshot_injuries.R  —  saves ESPN's injury / availability info once a day
#
# WHY: ESPN only shows the CURRENT injury report. To use injuries in a model we need what was
# known BEFORE each game, so we must save a snapshot every day starting now.
#
# FIRST RUN = probe. It tries three ESPN endpoints and tells you which (if any) return data for
# college basketball. I could not test this from my side (ESPN is blocked there), so the probe
# decides. Paste its output back to me.
#
#   Rscript scripts/snapshot_injuries.R probe     # try the endpoints, save samples
#   Rscript scripts/snapshot_injuries.R           # daily snapshot (run from the project root)
#
# Output: lockbot_injuries/raw/<date>__<source>.json  (raw, so nothing is lost if the format changes)
#         lockbot_injuries/injuries_<date>.csv        (best-effort flattened table)
# ─────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({ library(httr2); library(jsonlite); library(dplyr); library(purrr); library(glue); library(tibble) })

`%||%` <- function(a, b) if (is.null(a)) b else a
args  <- commandArgs(trailingOnly = TRUE)
PROBE <- length(args) > 0 && args[1] == "probe"
OUT   <- "lockbot_injuries"
dir.create(file.path(OUT, "raw"), recursive = TRUE, showWarnings = FALSE)
today <- format(Sys.Date(), "%Y-%m-%d")
SITE  <- "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball"
CORE  <- "https://sports.core.api.espn.com/v2/sports/basketball/leagues/mens-college-basketball"

get_json <- function(url, query = list()) {
  Sys.sleep(0.3)
  tryCatch({
    resp <- request(url) |> req_url_query(!!!query) |> req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36") |>
      req_headers(Accept = "application/json") |>
      req_timeout(30) |> req_error(is_error = \(r) FALSE) |> req_perform()
    if (resp_status(resp) != 200)
      message(glue("  HTTP {resp_status(resp)} for {url}: {substr(resp_body_string(resp), 1, 160)}"))
    list(status = resp_status(resp),
         text = if (resp_status(resp) == 200) resp_body_string(resp) else NA_character_)
  }, error = function(e) list(status = NA_integer_, text = NA_character_, err = conditionMessage(e)))
}
save_raw <- function(name, text) {
  if (is.na(text)) return(invisible(NULL))
  writeLines(text, file.path(OUT, "raw", glue("{today}__{name}.json")))
}

# today's games (id + teams)
sb <- get_json(glue("{SITE}/scoreboard"), list(dates = format(Sys.Date(), "%Y%m%d"), groups = 50, limit = 400))
games <- tibble()
if (!is.na(sb$text)) {
  ev <- fromJSON(sb$text, simplifyVector = FALSE)$events
  games <- map_dfr(ev, \(e) tibble(game_id = e$id, name = e$name, date = e$date))
}
message(glue("Scoreboard: status {sb$status}, {nrow(games)} games today"))

# 1. league-wide injuries
inj_all <- get_json(glue("{SITE}/injuries"))
message(glue("[1] {SITE}/injuries  -> status {inj_all$status}",
             "{if (!is.na(inj_all$text)) glue(', {nchar(inj_all$text)} chars') else ''}"))
save_raw("site_injuries", inj_all$text)

# 2. per-game summary (pro leagues put an 'injuries' block here)
n_try <- if (PROBE) min(5, nrow(games)) else nrow(games)
sum_hits <- 0; flat <- list()
for (i in seq_len(n_try)) {
  s <- get_json(glue("{SITE}/summary"), list(event = games$game_id[i]))
  if (is.na(s$text)) next
  j <- fromJSON(s$text, simplifyVector = FALSE)
  if (length(j$injuries) > 0) {
    sum_hits <- sum_hits + 1
    save_raw(glue("summary_{games$game_id[i]}"), s$text)
    for (tm in j$injuries) for (it in tm$injuries) {
      flat[[length(flat) + 1]] <- tibble(
        snapshot = today, game_id = games$game_id[i], team = tm$team$displayName %||% NA_character_,
        athlete = it$athlete$displayName %||% NA_character_,
        status = it$status %||% NA_character_, detail = it$details$type %||% it$type$description %||% NA_character_)
    }
  }
}
message(glue("[2] summary endpoint: {sum_hits} of {n_try} games had an 'injuries' block"))

# 3. per-team core API (probe on a few team ids; full run covers every team playing today)
team_ids <- if (nrow(games) > 0) {
  ev <- fromJSON(sb$text, simplifyVector = FALSE)$events
  unique(unlist(map(ev, \(e) map_chr(e$competitions[[1]]$competitors, \(c) c$id))))
} else character()
team_ids <- if (PROBE) head(team_ids, 5) else team_ids
core_hits <- 0
for (tid in team_ids) {
  r <- get_json(glue("{CORE}/teams/{tid}/injuries"))
  if (is.na(r$text)) next
  j <- fromJSON(r$text, simplifyVector = FALSE)
  if ((j$count %||% 0) > 0) { core_hits <- core_hits + 1; save_raw(glue("core_team_{tid}"), r$text) }
}
message(glue("[3] core API team injuries: {core_hits} of {length(team_ids)} teams returned entries"))

if (length(flat) > 0) {
  df <- bind_rows(flat); write.csv(df, file.path(OUT, glue("injuries_{today}.csv")), row.names = FALSE)
  message(glue("Wrote injuries_{today}.csv with {nrow(df)} rows"))
}
if (PROBE) message("\nProbe done. Paste the four '[ ]' / 'Scoreboard' lines above to Claude. Samples are in lockbot_injuries/raw/.")
