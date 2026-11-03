# Lockbot 2.0 pipeline (Python)

Offline training/backtesting. The Shiny app keeps serving a prediction cache, as in 1.0.

    python3 -I lockbot2/build_dataset.py --export-dir <lockbot_export> --odds <odds_2026.csv> --out lockbot2/out/games_model.csv
    python3 -I lockbot2/backtest_baselines.py --data lockbot2/out/games_model.csv --out lockbot2/out/backtest_results.json

- `team_match.py`: ESPN id -> KenPom name, per season. Matches 100% of D1 games (1.0's crosswalk
  contained wrong pairs, e.g. Alabama -> Alabama St., and dropped roughly a quarter of games).
- `build_dataset.py`: one row per D1 game, point-in-time features only (latest KenPom snapshot
  before game day; last season's final ratings for early games; rest, games played, league scoring level).
  Never uses Seed/Event/*Final columns or same-season end-of-year tables.
- `backtest_baselines.py`: walk-forward (train on earlier seasons, test 2024/25/26) with a
  KenPom-style formula, ridge, and LightGBM; 2026 compared with the DraftKings close and KenPom fanmatch.

## Baseline results (2026, 4,766 games with lines)

| source | margin MAE | winner acc | total MAE |
|---|---|---|---|
| DraftKings close | 8.86 | 70.8% | 13.32 |
| KenPom fanmatch | 8.99 | 70.3% | 13.70 |
| Lockbot2 ridge | 9.01 | 70.1% | 13.63 |
| Lockbot2 lightgbm | 9.06 | 70.1% | 13.68 |

Model-minus-market disagreement carries no information (b2 = 0.03, t = 0.5); simulated ATS and
over/under bets lose roughly the vig. Next: richer point-in-time features (box-score form),
calibrated probabilities, and a betting tab that says "no bet" when there is no edge.

## Update: rolling box-score form (`box_features.py`)

Adds exponentially weighted (half-life 6 games, prior games only) shooting, turnover, rebound,
free-throw, pace and efficiency rates from ESPN box scores (sportsdataverse
`team_box_{season}.parquet`, downloaded from the GitHub release, no API key needed).

| 2026, 4,766 games with lines | Margin MAE | Total MAE | Brier |
|---|---|---|---|
| DraftKings close | 8.86 | 13.32 | 0.1866 |
| KenPom fanmatch | 8.99 | 13.70 | 0.1902 |
| Lockbot2 ridge+box | 8.98 | 13.50 | |
| Lockbot2 lightgbm+box | 9.01 | 13.56 | 0.1909 |

Model-minus-market information test improves to b2 = 0.10 (t = 1.4): suggestive, not significant.
Confirming it needs more seasons of closing lines.

Download box scores:
    for y in 2022 2023 2024 2025 2026; do curl -sSL -o team_box_$y.parquet \
      https://github.com/sportsdataverse/sportsdataverse-data/releases/download/espn_mens_college_basketball_team_boxscores/team_box_$y.parquet; done
