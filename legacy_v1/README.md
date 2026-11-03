# LockBot 1.0 (archived)

The original graph-neural-network predictor. Nothing in LockBot 2.0 needs these files.

- `python/` - GNN training and prediction (torch / torch-geometric)
- `requirements.txt` - Python packages for the GNN
- `R/feature_engineering.R`, `R/graph_builder.R` - feature matrix and team graph builders used by `R/prediction.R::run_training()`

`R/prediction.R` and `R/prediction_cache.R` still contain the 1.0 code paths (Python fallback, 1.0 cache). They are only used when a matchup is missing from the 2.0 cache. To run 1.0 training again, `source("legacy_v1/R/feature_engineering.R")` and `source("legacy_v1/R/graph_builder.R")` first.
