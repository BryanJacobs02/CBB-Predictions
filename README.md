# LockBot 1.0 — NCAA Basketball Score Predictor

*A Graph Neural Network trained on KenPom advanced statistics to predict NCAA D1 men's basketball game scores and outcomes*

## Project Overview

LockBot uses a Graph Attention Network (GAT) to predict final scores for any NCAA Division I men's basketball matchup. Unlike models that evaluate teams in isolation, LockBot learns from inter-team relationships (conference rivalries, common opponents, and competitive history) on top of 35 advanced statistical features sourced from the KenPom API. Predictions include projected final scores, implied win probability, point spread coverage analysis, and moneyline value assessment against user-entered betting lines.

The model is trained on ~12,600 historical games across three seasons using point-in-time features to prevent data leakage. A 2024 game is evaluated using only statistics available in 2024, not end-of-2026 ratings.

## Live App

**https://bryan-jacobs.shinyapps.io/lockbot/**

## Performance vs. Benchmarks

Evaluated on a chronologically held-out test set of 2,530 games never seen during training:

| Metric | LockBot | KenPom Fanmatch | Vegas Closing Line |
|---|---|---|---|
| Winner Accuracy | **71.2%** | ~70-72% | ~73-75% |
| Score MAE | **7.69 pts** | 8.07 pts | ~7.0-7.5 pts |
| Margin MAE | **8.39 pts** | — | — |
| Brier Score (implied) | **0.187** | — | ~0.170 |
| Home advantage learned | **5.07 pts** | 5.1 pts (actual avg) | — |

LockBot matches or exceeds KenPom's raw score prediction accuracy and approaches Vegas-level winner prediction without access to line movement, injury reports, or real-time data.

## Authors

**Bryan Jacobs**

## Languages / Packages

**R**
- shiny, shinydashboard, shinyWidgets
- httr2, jsonlite
- dplyr, tidyr, purrr, lubridate
- reticulate, plotly, DT
- hoopR, memoise, glue, scales, stringdist

**Python**
- PyTorch
- torch-geometric (Graph Attention Networks)
- pandas, numpy, scikit-learn

## Data Sources

- **KenPom API** — AdjEM, AdjOE, AdjDE, Tempo, Four Factors, Misc Stats, Height, Point Distribution, Conference Ratings, weekly archive snapshots
- **hoopR / ESPN** — actual game results and final scores for training labels

## Model Architecture

- **Graph Attention Network (GAT)** — 2-layer, 4 attention heads, learns team embeddings encoding inter-team relationships
- **MatchupPredictor** — separate encoding paths for home and away teams with an explicit learnable home advantage parameter
- **Features** — 35 per-team features normalized with direction-aware inversion (lower defensive metrics inverted so higher always = better)
- **Training** — 500 epochs, cosine annealing LR schedule, recency-weighted MSE loss (60-day half-life), full dataset training for deployment
- **Leak prevention** — season-specific static features, weekly point-in-time archive ratings, chronological train/test split

## Repository Structure
```
cbb-predictor/
├── app.R                     # Shiny entry point
├── global.R                  # Shared config, package loads, mode detection
├── R/
│   ├── kenpom_api.R          # KenPom API calls and caching
│   ├── feature_engineering.R # Feature matrix + historical snapshots
│   ├── graph_builder.R       # Team relationship graph construction
│   ├── game_results.R        # hoopR game results + name crosswalk
│   ├── prediction.R          # Live prediction + cache lookup
│   ├── prediction_cache.R    # Bulk prediction cache generator
│   └── team_data.R           # Team logos and colors
├── python/
│   ├── gnn_model.py          # GAT + MatchupPredictor architecture
│   ├── train.py              # Training loop with evaluation metrics
│   └── predict.py            # Inference
├── data/
│   ├── models/               # Saved model weights (not tracked in git)
│   └── predictions_cache.rds # Pre-computed predictions (not tracked in git)
├── ui/
│   ├── ui_main.R             # Dashboard layout
│   └── ui_components.R       # Reusable UI components
├── server/
│   └── server_main.R         # Shiny server logic
└── www/
    └── styles.css            # Dark theme styling
```

## How to Run Locally

### Prerequisites

- R 4.4+
- Python 3.12+
- KenPom API key

### Setup

1. Clone the repository
2. Create `.Renviron` in the project root:
```
KENPOM_API_KEY=your_key_here
```
3. Install R packages:
```r
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "httr2",
                   "jsonlite", "dplyr", "tidyr", "purrr", "lubridate",
                   "reticulate", "plotly", "DT", "glue", "memoise",
                   "cachem", "hoopR", "stringdist", "scales"))
```
4. Create the Python virtual environment:
```bash
python python/setup_env.py
```
5. Launch the app:
```r
source("global.R")
shiny::runApp()
```

### Retraining

1. Go to the **Train Model** tab
2. Select seasons and half-life settings
3. Check **Full train mode**
4. Click **Train Model** (~10 minutes)
5. After training, generate the prediction cache:
```r
PREDICTION_CACHE <- generate_prediction_cache()
```
6. Redeploy:
```r
rsconnect::deployApp(
  appDir   = ".",
  appName  = "lockbot",
  appFiles = c("app.R", "global.R", "packages.R", ".Renviron",
               "R/", "ui/", "server/", "www/",
               "data/models/", "data/predictions_cache.rds")
)
```

## AI Acknowledgment

This project was built with the assistance of [Claude](https://claude.ai) (Anthropic) through a single extended conversation covering the full development lifecycle — architecture design, R and Python implementation, debugging, model tuning, data leakage remediation, UI design, and deployment. All code was written collaboratively with Claude and reviewed, tested, and iterated on by the author.

## Contact

bryanj82202@gmail.com
