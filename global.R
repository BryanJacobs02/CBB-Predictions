library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(reticulate)
library(ggplot2)
library(plotly)
library(DT)
library(glue)
library(memoise)
library(cachem)
library(hoopR)
library(stringdist)
library(scales)

# ── KenPom ───────────────────────────────────────────────────────────────────
KENPOM_BASE  <- "https://kenpom.com/api.php"
KENPOM_KEY   <- Sys.getenv("KENPOM_API_KEY")
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
SEASON_YEAR  <- if (month(Sys.Date()) >= 11) CURRENT_YEAR + 1 else CURRENT_YEAR

# ── Python / reticulate ───────────────────────────────────────────────────────
# Only initialize Python if running locally (not on shinyapps.io)
IS_LOCAL <- file.exists(".venv")

if (IS_LOCAL) {
  library(reticulate)
  use_virtualenv(file.path(getwd(), ".venv"), required = TRUE)
} else {
  message("No .venv found — running in deployed mode (cache only)")
}

# ── Cache (1-hour TTL) ────────────────────────────────────────────────────────
cache <- cache_mem(max_age = 3600)

# ── Source modules ────────────────────────────────────────────────────────────
source("R/kenpom_api.R")
source("R/feature_engineering.R")
source("R/graph_builder.R")
source("R/game_results.R")
source("R/prediction_cache.R")
source("R/team_data.R")
source("ui/ui_main.R")
source("ui/ui_components.R")
source("server/server_main.R")

if (IS_LOCAL) {
  source("R/prediction.R")
} else {
  # Deployed mode — define stub functions that use cache only
  run_training <- function(...) stop("Training not available in deployed mode.")
  init_python_modules <- function(...) invisible(NULL)
}

# ── Load prediction cache ─────────────────────────────────────────────────────
PREDICTION_CACHE <- load_prediction_cache()
IS_DEPLOYED      <- !IS_LOCAL
message(glue("Mode: {if (IS_DEPLOYED) 'DEPLOYED (cache)' else 'LOCAL (live Python)'}"))