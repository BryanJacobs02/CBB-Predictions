# LockBot 2.0 — NCAA Basketball Score Predictor and Betting Model

*Ridge + gradient-boosted models on point-in-time KenPom ratings and rolling box-score form, predicting NCAA D1 men's basketball margins, totals, and win probabilities, with a betting analyzer calibrated against real sportsbook lines.*

## Live App

**https://bryan-jacobs.shinyapps.io/lockbot/**

## What it does

- **Predict Matchup** — projected score, margin, total, and win probability for any two D1 teams (home, away, or neutral site), with a win-probability donut and a stat-percentile comparison.
- **Today's Picks** — pulls today's and tomorrow's DraftKings lines from ESPN, compares them with the model, and lists spread and over/under picks ranked by expected value. A refresh button re-pulls the lines.
- **Betting Analysis** — enter any spread, total, or moneyline and get an edge, expected value, and a suggested stake (quarter-Kelly, capped at 3% of bankroll), or "no bet".
- **Team Stats** and **Model Evaluation** — current KenPom ratings and the walk-forward backtest.

## Performance

Every number below comes from **walk-forward testing**: each season is predicted by models trained only on earlier seasons, using only information available before tip-off.

| Season | Games | Winner accuracy | Margin MAE | Total MAE |
|---|---|---|---|---|
| 2024 | 5,724 | 71.9% | 8.98 | 13.12 |
| 2025 | 5,748 | 72.5% | 8.95 | 12.93 |
| 2026 | 5,749 | 71.9% | 9.14 | 13.64 |

Head to head on the 4,766 games of 2026 that have a DraftKings line (identical games for every row):

| | Margin MAE | Winner accuracy | Total MAE | Brier (win prob.) |
|---|---|---|---|---|
| DraftKings closing line | 8.86 | 70.8% | 13.32 | 0.1867 |
| KenPom (fanmatch) | 8.99 | 70.3% | 13.69 | 0.1875 |
| **LockBot 2.0** | **8.99** | **70.1%** | **13.48** | **0.1877** |

LockBot 2.0 is on par with KenPom on margins, a little better on totals, and slightly behind the closing line.

### Betting findings (2026, 4,751 games with opening and closing lines)

- The market **moves toward the model** between open and close. The model's weight in predicting results is about 0.54 (spread) and 0.64 (total) against **opening** lines, but only about 0.10 and 0.01 against **closing** lines.
- So the edge is against early lines, not closing lines. The betting tool therefore shrinks the model toward the market, using different weights for opening and closing lines.
- Caveats: one season, one sportsbook, and the model uses game-day data an opening line could not have seen, so real results will be worse than the backtest. See `lockbot2/README.md` for the full analysis.

### What was tested and did not help

Player-level features (rotation absences, returning minutes, transfers) and extra model types added no measurable accuracy. ESPN publishes no college-basketball injury data. Details are in `lockbot2/README.md`.

## Changes from LockBot 1.0

- The Graph Attention Network was replaced by a ridge + LightGBM blend; at this data size the simpler models did at least as well.
- Rolling box-score form (efficiency, four factors, pace) was added.
- Win probability is calibrated from out-of-sample residuals instead of a fixed constant.
- Over/under, expected value, and capped fractional-Kelly sizing were added; the betting model was calibrated against real lines.
- 1.0's published figures came from a model trained on all games and covered only about 73% of D1 games, so they are not comparable with the walk-forward numbers above.

## How it works

1. `scripts/refresh_data.R` pulls the current season's KenPom ratings (weekly snapshots plus today's) and the ESPN schedule into `lockbot_export/`.
2. `lockbot2/refresh.py` downloads ESPN box scores, builds a leak-free table (latest KenPom snapshot strictly before game day, rolling box-score form, rest, recent margin, league scoring level), trains the blend on every completed game, and writes `data/predictions_v2.csv` for every ordered pair of teams, at home and neutral sites.
3. The Shiny app reads that cache; win probability is a normal CDF of the predicted margin with an out-of-sample sigma (about 11.4 points).
4. `R/picks.R` and `R/betting_v2.R` turn lines plus predictions into picks and stakes.

## Repository Structure
```
├── app.R, global.R           # Shiny entry point and shared setup
├── R/
│   ├── prediction_cache_v2.R # 2.0 cache loader and lookup
│   ├── betting_v2.R          # edge / EV / Kelly math and measured line weights
│   ├── picks.R               # builds the daily pick list from live ESPN lines
│   ├── plots_v2.R            # dark-theme win-probability and stat charts
│   └── kenpom_api.R, team_data.R, game_results.R   # data access; prediction.R / prediction_cache.R still hold 1.0 fallbacks
├── lockbot2/                 # model pipeline (see lockbot2/README.md)
│   ├── build_dataset.py, box_features.py, team_match.py
│   ├── make_cache.py, refresh.py, eval_blend.py
│   └── backtest_baselines.py, line_movement.py, player_features.py
├── scripts/                  # refresh_data.R, refresh.sh, make_picks.R, export/odds pulls
├── data/                     # predictions_v2.csv, team_map.csv, eval_v2.json
├── ui/, server/, www/        # dashboard layout, server logic, dark theme
└── legacy_v1/                # LockBot 1.0 GNN (python/, feature + graph builders); not used by 2.0
```

## How to Run Locally

### Prerequisites

- R 4.4+ and Python 3.12+
- A KenPom API key

### Setup

1. Clone the repository and check out the `lockbot-2.0` branch.
2. Create `.Renviron` in the project root:
```
KENPOM_API_KEY=your_key_here
```
3. Install R packages (`renv::restore()` or the list in `global.R`).
4. Install Python packages: `pip install -r lockbot2/requirements.txt`
5. Launch the app:
```r
shiny::runApp()
```

### Refreshing the predictions (weekly, or twice a week in season)

The app's predictions come from a saved file, so they only change when you refresh. **What a refresh does:**
1. Pulls this season's newest KenPom ratings and ESPN schedule and results.
2. Downloads the latest box scores.
3. Rebuilds the training table and retrains the model on every game played so far.
4. Rewrites `data/predictions_v2.csv` with new predictions for every team pairing.

It takes a few minutes. Today's Picks does not need it, because that tab pulls live lines every time you open it.

**How to do it** (Terminal, in the project folder, the one containing `app.R`):
```bash
bash scripts/refresh.sh                       # steps 1-4 above
git add data/ && git commit -m "Refresh predictions" && git push
```
Then in the R console, to publish to shinyapps.io:
```r
source("global.R"); deploy_lockbot()
```
After the season ends, run the refresh once more so the final ratings are saved for next season. If it stops with an "unmatched team" error, a new or renamed team needs adding to `lockbot2/team_match.py`.

The first time only, run `scripts/export_lockbot_data.R` to create the historical export (`lockbot_export/`) that the refresh builds on.

## Data Sources

- **KenPom API** — AdjEM, AdjOE, AdjDE, tempo, weekly archive snapshots
- **ESPN / hoopR / sportsdataverse** — schedules, results, team box scores, and DraftKings lines (via ESPN)

## AI Acknowledgment

This project was built with the assistance of [Claude](https://claude.ai) (Anthropic) across extended conversations covering architecture, R and Python implementation, debugging, model evaluation, data-leakage remediation, UI design, and deployment. All code was written collaboratively with Claude and reviewed, tested, and iterated on by the author.

## Contact

bryanj82202@gmail.com
