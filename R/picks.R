# ── Daily pick list (used by the app's refresh button and by scripts/make_picks.R) ──
# Needs R/prediction_cache_v2.R and R/betting_v2.R. Logic and weights are explained in
# scripts/make_picks.R and lockbot2/README.md.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

PICKS_SITE <- "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball"
PICKS_UA   <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"

# progress: optional function(i, n) called after each game's lines are fetched
build_picks <- function(days_ahead = 1, cache = PREDICTION_CACHE_V2, progress = NULL) {
  tmap <- read.csv("data/team_map.csv", colClasses = "character")
  if (is.null(cache)) stop("data/predictions_v2.csv not found. Run python3 lockbot2/refresh.py first.")
  kp_of <- setNames(tmap$kp_name, tmap$espn_id)
  SITE <- PICKS_SITE; UA <- PICKS_UA

  num <- function(x) suppressWarnings(as.numeric(gsub("^[ou+]", "", as.character(x[1]))))   # "o145.5" -> 145.5, "+110" -> 110

  scoreboard <- function(day) {
    r <- request(SITE) |> req_url_path_append("scoreboard") |>
      req_url_query(dates = format(day, "%Y%m%d"), groups = 50, limit = 400) |>
      req_user_agent(UA) |> req_error(is_error = \(r) FALSE) |> req_perform()
    if (resp_status(r) != 200) { message(glue("scoreboard {day}: HTTP {resp_status(r)}")); return(list()) }
    fromJSON(resp_body_string(r), simplifyVector = FALSE)$events
  }

  today_et <- as.Date(format(Sys.time(), tz = "America/New_York"))     # servers run on UTC
  events <- unlist(map(today_et + 0:days_ahead, scoreboard), recursive = FALSE)
  message(glue("{length(events)} games on the scoreboard"))

  parse_event <- function(e) {
    comp <- e$competitions[[1]]
    if (!identical(comp$status$type$state %||% e$status$type$state, "pre")) return(NULL)      # only games not yet started
    side <- \(w) keep(comp$competitors, \(c) c$homeAway == w)[[1]]
    h <- side("home"); a <- side("away")
    tibble(game_id = e$id, tip = as.POSIXct(e$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"),
           neutral = isTRUE(comp$neutralSite),
           home_espn = h$id, away_espn = a$id, home_name = h$team$displayName, away_name = a$team$displayName)
  }
  games <- map_dfr(events, parse_event)
  message(glue("{nrow(games)} not yet started"))

  get_lines <- function(id) {
    pc <- tryCatch(hoopR::espn_mbb_betting(game_id = id)$pickcenter, error = \(e) NULL)
    Sys.sleep(0.3)
    if (is.null(pc) || nrow(pc) == 0) return(NULL)
    pc <- as.data.frame(pc); pc <- pc[grepl("draft", pc$provider_name, ignore.case = TRUE), , drop = FALSE] |> (\(x) if (nrow(x)) x else as.data.frame(pc)[1, , drop = FALSE])()
    pc <- pc[1, , drop = FALSE]
    g <- \(col) if (col %in% names(pc)) pc[[col]] else NA
    cur_sp <- num(g("point_spread_home_close_line"))
    if (is.na(cur_sp) && !is.na(num(g("spread")))) {                      # fall back: spread is the favorite's number
      fav_home <- isTRUE(as.logical(g("home_team_odds_favorite")))
      cur_sp <- if (fav_home) -abs(num(g("spread"))) else abs(num(g("spread")))
    }
    tibble(game_id = id,
           spread_open = num(g("point_spread_home_open_line")), spread_now = cur_sp,
           total_open = num(g("total_over_open_line")),
           total_now = num(g("total_over_close_line")) %||% num(g("over_under")),
           spread_price = num(g("point_spread_home_close_odds")) %||% -110,
           total_price  = num(g("total_over_close_odds")) %||% -110)
  }
  n_games <- nrow(games)
    lines <- map_dfr(seq_len(n_games), function(i) { if (!is.null(progress)) progress(i, n_games); get_lines(games$game_id[i]) })
  message(glue("{nrow(lines)} games with DraftKings lines"))
  if (nrow(lines) == 0) stop("No lines returned. Lines usually appear a day or less before tip-off; try again later.")

  pick_weight <- function(open, now, hours) if (!is.na(open) && !is.na(now) && open == now && hours > 3) "open" else "close"

  rows <- games |> inner_join(lines, by = "game_id") |> mutate(row = row_number())
  res <- pmap_dfr(rows, function(...) {
    r <- list(...)
    hk <- kp_of[r$home_espn]; ak <- kp_of[r$away_espn]
    if (is.na(hk) || is.na(ak)) return(NULL)
    pr <- lookup_cached_prediction_v2(hk, ak, if (r$neutral) "neutral" else "a_home", cache)
    if (is.null(pr)) return(NULL)
    hours <- as.numeric(difftime(r$tip, Sys.time(), units = "hours"))
    out <- tibble(tip_et = format(as.POSIXct(r$tip, tz = "UTC"), tz = "America/New_York", "%a %b %d %I:%M %p"),
                  away = r$away_name, home = r$home_name, neutral = r$neutral,
                  model_margin_home = round(pr$margin, 1), model_total = round(pr$total, 1),
                  spread_open = r$spread_open, spread_now = r$spread_now,
                  total_open = r$total_open, total_now = r$total_now)
    # spread: evaluate both sides against the CURRENT line, keep the better one
    if (!is.na(r$spread_now)) {
      lt <- LINE_TYPES[[pick_weight(r$spread_open, r$spread_now, hours)]]
      h <- spread_bet(pr$margin, r$spread_now, r$spread_price, lt$weight_spread, lt$sigma_margin)
      a <- spread_bet(-pr$margin, -r$spread_now, r$spread_price, lt$weight_spread, lt$sigma_margin)
      best <- if (h$ev >= a$ev) h else a; home_side <- h$ev >= a$ev
      out$spread_pick <- if (home_side) glue("{r$home_name} {sprintf('%+.1f', r$spread_now)}") else glue("{r$away_name} {sprintf('%+.1f', -r$spread_now)}")
      out$spread_edge <- round(best$raw_edge, 1); out$spread_ev <- round(100 * best$ev, 1)
      out$spread_call <- toupper(best$call); out$spread_stake <- round(100 * best$stake, 1)
      out$spread_basis <- pick_weight(r$spread_open, r$spread_now, hours)
    }
    if (!is.na(r$total_now)) {
      lt <- LINE_TYPES[[pick_weight(r$total_open, r$total_now, hours)]]
      t <- total_bet(pr$total, r$total_now, r$total_price, lt$weight_total, lt$sigma_total)
      out$total_pick <- glue("{t$side} {r$total_now}")
      out$total_edge <- round(abs(t$raw_edge), 1); out$total_ev <- round(100 * t$ev, 1)
      out$total_call <- toupper(t$call); out$total_stake <- round(100 * t$stake, 1)
      out$total_basis <- pick_weight(r$total_open, r$total_now, hours)
    }
    out
  })
  if (nrow(res) == 0) stop("No games could be matched to the prediction cache (check data/team_map.csv).")

  res$generated_at <- format(Sys.time(), tz = "America/New_York", "%Y-%m-%d %I:%M %p ET")
  res |> arrange(desc(coalesce(spread_ev, -99) + coalesce(total_ev, -99)))
}
