# ─────────────────────────────────────────────────────────────────────────────
# make_picks.R  —  today's (and tomorrow's) games ranked by the model's edge vs the sportsbook line
#
#   Rscript scripts/make_picks.R            # from the project root
#
# Reads  data/predictions_v2.csv + data/team_map.csv  (made by lockbot2/refresh.py)
# Calls  ESPN scoreboard + hoopR::espn_mbb_betting for DraftKings lines (open and current)
# Writes data/picks.csv  -> shown on the app's "Today's Picks" tab (redeploy to publish).
#
# Which trust weights apply to each game (from the 2026 backtest, see lockbot2/README.md):
#   * line has NOT moved since it opened, tip-off more than 3 hours away  -> "open" weights
#   * line has moved, or tip-off is close                                 -> "close" weights
#   (once the market has moved toward the model, the model's edge shrinks to almost nothing)
# ─────────────────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({ library(httr2); library(jsonlite); library(dplyr); library(purrr); library(glue); library(hoopR) })
source("R/prediction_cache_v2.R"); source("R/betting_v2.R"); source("R/picks.R")
res <- build_picks(cache = load_prediction_cache_v2())
write.csv(res, "data/picks.csv", row.names = FALSE)
message(glue("\nWrote data/picks.csv: {nrow(res)} games | ",
             "spread BETs: {sum(res$spread_call == 'BET', na.rm = TRUE)} | total BETs: {sum(res$total_call == 'BET', na.rm = TRUE)}"))
print(head(select(res, tip_et, away, home, spread_pick, spread_edge, spread_call, total_pick, total_edge, total_call), 12))
