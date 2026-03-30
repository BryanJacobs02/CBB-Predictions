if (!requireNamespace("memoise", quietly = TRUE)) install.packages("memoise", repos = "https://cloud.r-project.org")
if (!requireNamespace("cachem",  quietly = TRUE)) install.packages("cachem",  repos = "https://cloud.r-project.org")

cache <- cachem::cache_mem(max_age = 3600)

kenpom_get <- function(endpoint, ...) {
  params <- list(endpoint = endpoint, ...)
  resp <- httr2::request(KENPOM_BASE) |>
    httr2::req_headers(Authorization = paste("Bearer", KENPOM_KEY)) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_error(is_error = \(r) FALSE) |>
    httr2::req_perform()
  
  if (httr2::resp_status(resp) != 200)
    stop(glue::glue("KenPom API error {httr2::resp_status(resp)}: {httr2::resp_body_string(resp)}"))
  
  httr2::resp_body_json(resp, simplifyVector = TRUE) |> dplyr::as_tibble()
}

get_teams <- memoise::memoise(function(year = SEASON_YEAR) {
  kenpom_get("teams", y = year)
}, cache = cache)

get_ratings <- memoise::memoise(function(year = SEASON_YEAR) {
  kenpom_get("ratings", y = year)
}, cache = cache)

get_four_factors <- memoise::memoise(function(year = SEASON_YEAR) {
  kenpom_get("four-factors", y = year)
}, cache = cache)

get_misc_stats <- memoise::memoise(function(year = SEASON_YEAR) {
  kenpom_get("misc-stats", y = year)
}, cache = cache)

get_height <- memoise::memoise(function(year = SEASON_YEAR) {
  kenpom_get("height", y = year)
}, cache = cache)

get_point_dist <- memoise::memoise(function(year = SEASON_YEAR) {
  kenpom_get("pointdist", y = year)
}, cache = cache)

get_conf_ratings <- memoise::memoise(function(year = SEASON_YEAR) {
  kenpom_get("conf-ratings", y = year)
}, cache = cache)

get_archive <- memoise::memoise(function(date) {
  kenpom_get("archive", d = date)
}, cache = cache)

get_fanmatch <- memoise::memoise(function(date = as.character(Sys.Date())) {
  kenpom_get("fanmatch", d = date)
}, cache = cache)