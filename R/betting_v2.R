# ── Lockbot 2.0 betting math ─────────────────────────────────────────────────
# Pure functions (no Shiny) so they are easy to test.
#
# Why everything is "shrunk" toward the market line:
#   Backtest on 4,751 2026 games with DraftKings closing lines. Regressing the actual result
#   on (market line) and (model - market) gives the model's disagreement a weight of
#   about 0.10 for margins (se 0.07) and 0.01 for totals (se 0.08). The closing line is
#   already very good, so when the model disagrees with it, the market is usually right.
#   Treating the raw model number as the true mean would manufacture fake "edges".
#
# Residual SDs of (actual - market line): margin 11.2 pts, total 16.8 pts.

# Measured on 4,751 games of 2026 (DraftKings), model trained only on earlier seasons:
#   vs the CLOSING line: weight 0.10 (margin), 0.01 (total);  residual SD 11.2 / 16.8
#   vs the OPENING line: weight 0.54 (margin, se 0.08), 0.64 (total, se 0.09);  residual SD 11.3 / 17.0
# i.e. the market moves toward the model between open and close, so the model's edge is mostly an
# edge against early lines. Defaults below are the measured weights rounded down.
LINE_TYPES <- list(
  open  = list(label = "Opening line (bet early)", weight_spread = 0.45, weight_total = 0.55,
               sigma_margin = 11.3, sigma_total = 17.0),
  close = list(label = "Current / closing line",   weight_spread = 0.10, weight_total = 0.00,
               sigma_margin = 11.2, sigma_total = 16.8)
)

BET_PARAMS <- list(
  sigma_margin   = 11.2,   # SD of (actual margin - market margin)
  sigma_total    = 16.8,   # SD of (actual total  - market total)
  weight_spread  = 0.10,   # default trust in model vs. market for spread / moneyline
  weight_total   = 0.00,   # default trust in model vs. market for totals
  ml_hold        = 0.045,  # typical two-way moneyline hold, used to de-vig one-sided prices
  kelly_mult     = 0.25,   # fractional Kelly
  max_stake      = 0.03,   # never suggest more than 3% of bankroll
  min_ev         = 0.02    # need at least +2% expected value per $1 to call it a bet
)

american_to_decimal <- function(odds) {
  ifelse(odds < 0, 100 / abs(odds) + 1, odds / 100 + 1)
}

american_to_prob <- function(odds) {
  ifelse(odds < 0, abs(odds) / (abs(odds) + 100), 100 / (odds + 100))
}

# EV per $1 staked, full Kelly, and a capped fractional-Kelly stake (share of bankroll)
bet_value <- function(p_win, odds, kelly_mult = BET_PARAMS$kelly_mult,
                      max_stake = BET_PARAMS$max_stake,
                      min_ev = BET_PARAMS$min_ev) {
  b      <- american_to_decimal(odds) - 1
  ev     <- p_win * b - (1 - p_win)
  kelly  <- max(0, (b * p_win - (1 - p_win)) / b)
  stake  <- min(max_stake, kelly_mult * kelly)
  call   <- if (ev >= min_ev) "bet" else if (ev > 0) "lean" else "none"
  list(p_win = p_win, odds = odds, ev = ev, kelly = kelly,
       stake = if (call == "bet") stake else 0,
       breakeven = american_to_prob(odds), call = call)
}

# Spread: `line` is the bet team's spread (-3.5 = favored by 3.5, +6 = 6-point underdog).
# `model_margin` is the model's margin for the bet team (positive = bet team wins).
spread_bet <- function(model_margin, line, odds = -110,
                       weight = BET_PARAMS$weight_spread,
                       sigma = BET_PARAMS$sigma_margin) {
  raw_edge   <- model_margin + line                 # pts the model likes this side more than market
  fair_edge  <- weight * raw_edge                   # after shrinking toward the market
  p_cover    <- stats::pnorm(fair_edge / sigma)
  c(list(raw_edge = raw_edge, fair_edge = fair_edge), bet_value(p_cover, odds))
}

# Totals: evaluate Over and Under, return the better side.
# `odds` is the Over price; `odds_under` is the Under price (defaults to the same number).
total_bet <- function(model_total, line, odds = -110,
                      weight = BET_PARAMS$weight_total,
                      sigma = BET_PARAMS$sigma_total,
                      odds_under = odds) {
  raw_edge  <- model_total - line                   # positive = model says Over
  fair_edge <- weight * raw_edge
  p_over    <- stats::pnorm(fair_edge / sigma)
  over      <- bet_value(p_over, odds)
  under     <- bet_value(1 - p_over, odds_under)
  side      <- if (over$ev >= under$ev) "Over" else "Under"
  c(list(side = side, raw_edge = raw_edge, fair_edge = fair_edge),
    if (side == "Over") over else under)
}

# Moneyline: shrink the model's win probability toward the (de-vigged) market price.
moneyline_bet <- function(model_wp, odds, weight = BET_PARAMS$weight_spread,
                          hold = BET_PARAMS$ml_hold) {
  market_p <- american_to_prob(odds) / (1 + hold / 2)   # rough de-vig of a one-sided price
  p_fair   <- market_p + weight * (model_wp - market_p)
  c(list(market_p = market_p, raw_edge = model_wp - market_p, fair_edge = p_fair - market_p),
    bet_value(p_fair, odds))
}
