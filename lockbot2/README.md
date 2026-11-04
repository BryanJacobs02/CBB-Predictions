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

## Shipped model: ridge + LightGBM blend

`make_cache.py` blends ridge and LightGBM on the same features (margin 50/50, total 70% ridge).
Walk-forward 2024-26 averages: margin MAE 9.02 (ridge 9.04, LightGBM 9.06), total MAE ~13.2.
Gains over either single model are ~0.02 pts: consistent but small. On the 4,766 2026 games with a
DraftKings line: margin MAE 8.985 vs DraftKings 8.863 vs KenPom 8.987; total MAE 13.48 vs 13.32 vs 13.69.
Model-minus-market weight (regression on the close): margin 0.13 (se 0.08), total 0.05 (se 0.09).
`eval_blend.py` writes `data/eval_v2.json`, which the app's Model Evaluation tab reads.

## Player-level features (tested, NOT shipped)

`player_features.py` builds 7 pre-game features per team from ESPN player box scores: rotation players who
missed the previous game (minutes and production), minutes missed over the last 3 games, star dependence,
rotation size, returning-minutes share and incoming-transfer minutes. Build with `build_dataset.py --player-dir`.
Walk-forward 2024-26 (blend): margin MAE 9.023 -> 9.013, total MAE 13.230 -> 13.243; first 45 days of each
season 9.358 -> 9.338. Differences are inside noise, so the shipped model does not use them.
Likely reason: the ratings and rolling team form already absorb most of what a lost rotation player does, and
real injury news (which the market has) cannot be reconstructed from last night's box score.

## Preseason ratings from roster turnover (`preseason.py`, shipped)

Problem: before KenPom's first snapshot of a season (~Nov 8) the model only had LAST season's final ratings, so a team
that lost its roster (e.g. a title team that graduated its stars) was still rated like last year's team.

Fix: project each team's starting AdjEM from last year's final ratings plus roster turnover, using ESPN player box
scores (no key needed): share of last year's minutes / production returning, the top-5 returners, returning rotation
size, incoming transfers' minutes and production, and an "elite program" hinge (teams rated 15+ keep more than a straight
line says). The roster is whoever appears in a team's first game, so it works from the day after opening night. The
change in AdjEM is split evenly between offense and defense, so the total (OE + DE) and tempo keep last year's values.
(Projecting OE, DE and tempo separately made early totals worse.) Ridge regression, fit on earlier seasons only.

| walk-forward 2024-26 (blend) | before | after |
|---|---|---|
| margin MAE, games where a team still used last year's ratings (635) | 10.77 | 10.14 |
| margin MAE, first 14 days (1,343 games) | 9.96 | 9.65 |
| margin MAE, first 30 days (3,599 games) | 9.63 | 9.49 |
| margin MAE, whole season | 9.023 | 8.990 |
| total MAE, any window | unchanged | unchanged |
| Brier, games on last year's ratings | 0.1078 | 0.1038 |

With KenPom's own preseason ratings averaged in (below): prior-rating games 10.08, first 14 days 9.62, first 30 days 9.49.

Projection RMSE for a team's final AdjEM: 7.61 (last year's rating as is) -> 7.37 (regressed to the mean) -> 6.25
(with roster features); projected vs realized changes match across all size buckets.

KenPom preseason ratings: `Rscript scripts/pull_preseason.R` saves them to `lockbot_export/kp_preseason.csv` (2022-2027);
`scripts/refresh_data.R` adds each new season's once. The pipeline reads only the AdjEM column (the file also holds
Seed/Event/*Final columns that would leak the season's outcome). If the file is missing the projection runs without it.

Limits: it only sees minutes and box-score production, not recruiting rankings, injuries or portal hype, which the
market prices in. Record-high prior ratings are capped at 35 inside the projection so one outlier cannot be extrapolated.
Roster turnover has risen every year (returning minutes 51% in 2022 -> 29% in 2026 -> 26% so far in 2027), so
2027 inputs sit at the edge of what the model has seen. It helps the first weeks only; after KenPom's first
snapshot the model behaves as before.

## Keeping it current (refresh)

```
bash scripts/refresh.sh            # = Rscript scripts/refresh_data.R  +  python3 lockbot2/refresh.py
git add data/ && git commit -m "Refresh predictions" && git push
# then in R:  source("global.R"); deploy_lockbot()
```
`refresh_data.R` pulls only the current season (ESPN schedule, KenPom teams, weekly + today's ratings) into
`lockbot_export/`; `refresh.py` downloads box scores, rebuilds the table, retrains on every completed game and
rewrites `data/predictions_v2.csv`. First time: `pip install -r lockbot2/requirements.txt`.
Run it weekly (daily during conference play if you like). After the season ends, run it once more so the final
ratings are saved; next season's early games build on them. Preseason works: until KenPom's first snapshot (~Nov 8) a team's
rating is a projection from last year's rating and roster turnover (next section); if the home-court edge drifts above
~4.6 pts it is rescaled to 4.1. `refresh.py` also downloads player box scores for this (`lockbot_players/`, git-ignored).

## Injuries (no source)

ESPN's injury endpoints return nothing for college basketball (checked Nov 2026: scoreboard works, injuries
list is empty, per-game and per-team endpoints return no entries), and the NCAA only required availability
reports starting with the 2026 tournament. The player-box-score proxy added no accuracy (see above), so the model
runs without injury information, which the market does have.

## Historical lines (The Odds API, paid)

`scripts/pull_odds_api.R estimate|probe|run` saves DraftKings spread/total snapshots; `odds_api_merge.py`
converts them (last snapshot before tip = closing proxy) to the odds csv format `build_dataset.py --odds` takes
(comma-separate several files).

## Opening vs closing lines (2026, 4,751 games with both)

Model trained on 2022-25 only. The market moves toward the model between open and close:
* closing-line move regressed on (model - open): spread coef 0.42 (t = 8.0), total coef 0.47 (t = 35)
* result vs line, weight on (model - line): spread 0.54 vs OPEN (se 0.08) but 0.13 vs CLOSE (se 0.08);
  total 0.64 vs OPEN (se 0.09) but 0.01 vs CLOSE
* ATS vs the open line by raw edge: >=3 pts 58.8% (556 bets), >=4 pts 65.3% (225), >=5 pts 75% (92);
  totals vs open 55-58% at every threshold. Against the close the same rules lose roughly the vig.
Monthly spread weight vs open: Dec 0.47, Jan 0.14, Feb 0.47, Mar 0.64 (noisy but never negative).

Caveats that matter: one season, one book (DraftKings via ESPN); we do not know when the "open" was posted or its
limits; the model uses game-day data (latest ratings, last night's box score) that an early line could not have
seen, which overstates what a bettor could capture at open. Treat the numbers as evidence the model has real
information, not as a promised ROI. Confirming on 2022-25 needs historical lines (scripts/pull_odds_api.R,
which only has closing-proxy lines, so the open-vs-close test would not extend to those seasons).
