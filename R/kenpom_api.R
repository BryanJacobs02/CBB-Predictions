if (!requireNamespace("memoise", quietly = TRUE)) install.packages("memoise", repos = "https://cloud.r-project.org")
if (!requireNamespace("cachem",  quietly = TRUE)) install.packages("cachem",  repos = "https://cloud.r-project.org")
library(memoise)
library(cachem)

kenpom_get <- function(endpoint, ...) {
  params <- list(endpoint = endpoint, ...)
  resp <- request(KENPOM_BASE) |>
    req_headers(Authorization = paste("Bearer", KENPOM_KEY)) |>
    req_url_query(!!!params) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  
  if (resp_status(resp) != 200)
    stop(glue("KenPom API error {resp_status(resp)}: {resp_body_string(resp)}"))
  
  resp_body_json(resp, simplifyVector = TRUE) |> as_tibble()
}

get_teams <- memoise(function(year = SEASON_YEAR) {
  kenpom_get("teams", y = year)
}, cache = cache)

get_ratings <- memoise(function(year = SEASON_YEAR) {
  kenpom_get("ratings", y = year)
}, cache = cache)

get_four_factors <- memoise(function(year = SEASON_YEAR) {
  kenpom_get("four-factors", y = year)
}, cache = cache)

get_misc_stats <- memoise(function(year = SEASON_YEAR) {
  kenpom_get("misc-stats", y = year)
}, cache = cache)

get_height <- memoise(function(year = SEASON_YEAR) {
  kenpom_get("height", y = year)
}, cache = cache)

get_point_dist <- memoise(function(year = SEASON_YEAR) {
  kenpom_get("pointdist", y = year)
}, cache = cache)

get_conf_ratings <- memoise(function(year = SEASON_YEAR) {
  kenpom_get("conf-ratings", y = year)
}, cache = cache)

get_archive <- memoise(function(date) {
  kenpom_get("archive", d = date)
}, cache = cache)

get_fanmatch <- memoise(function(date = as.character(Sys.Date())) {
  kenpom_get("fanmatch", d = date)
}, cache = cache)