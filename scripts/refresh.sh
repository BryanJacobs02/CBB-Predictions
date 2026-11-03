#!/usr/bin/env bash
# Weekly (or daily in season) refresh: pull data in R, rebuild the cache in Python.
# Usage, from the project root:   bash scripts/refresh.sh
set -euo pipefail
cd "$(dirname "$0")/.."
Rscript scripts/refresh_data.R
python3 lockbot2/refresh.py "$@"
