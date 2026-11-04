# ─────────────────────────────────────────────────────────────────────────────
# pull_preseason.R  —  KenPom's own PRESEASON ratings for each season -> lockbot_export/kp_preseason.csv
#
# Run from the project root (so .Renviron loads your KENPOM_API_KEY):
#     Rscript scripts/pull_preseason.R           # seasons 2022-2027
#     Rscript scripts/pull_preseason.R probe     # just tests the request on one season and prints what comes back
#
# KenPom's archive endpoint can return the preseason ratings (before any games) for a season. The exact
# parameter spelling is not something I can test from here, so this tries a few and keeps the first that works.
# ~6-12 API calls in total. Attach lockbot_export/kp_preseason.csv to the chat when it finishes.
# ─────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({ library(dplyr); library(purrr); library(tibble); library(glue); library(lubridate) })
args <- commandArgs(trailingOnly = TRUE)
OUT <- "lockbot_export"
KENPOM_BASE <- "https://kenpom.com/api.php"
KENPOM_KEY  <- Sys.getenv("KENPOM_API_KEY")
if (!nzchar(KENPOM_KEY)) stop("KENPOM_API_KEY not found. Run from the project folder so .Renviron loads.")
dir.create(OUT, showWarnings = FALSE)
source("R/kenpom_api.R")

tries <- list(
  function(y) kenpom_get("archive", y = y, preseason = "true"),
  function(y) kenpom_get("archive", y = y, preseason = "1"),
  function(y) kenpom_get("archive", y = y, preseason = "Y"),
  function(y) kenpom_get("archive", d = glue("{y - 1}-10-25"))     # fallback: a date before the season starts
)

pull_one <- function(y) {
  for (i in seq_along(tries)) {
    r <- tryCatch(tries[[i]](y), error = function(e) { message(glue("  [{y}] attempt {i} failed: {substr(conditionMessage(e), 1, 120)}")); NULL })
    if (!is.null(r) && nrow(r) > 50) {
      message(glue("  [{y}] attempt {i} worked: {nrow(r)} teams, columns: {paste(names(r), collapse = ', ')}"))
      return(mutate(r, season = y, pull_method = i))
    }
    Sys.sleep(0.4)
  }
  NULL
}

seasons <- if (identical(args[1], "probe")) 2026 else 2022:2027
res <- map_dfr(seasons, pull_one)
if (nrow(res) == 0) stop("Nothing came back. Copy the messages above into the chat.")
if (identical(args[1], "probe")) { print(head(res, 5)); quit(save = "no") }
write.csv(res, file.path(OUT, "kp_preseason.csv"), row.names = FALSE)
message(glue("Wrote {file.path(OUT, 'kp_preseason.csv')}: {nrow(res)} rows ({paste(sort(unique(res$season)), collapse = ', ')})"))
print(res |> count(season, pull_method))
