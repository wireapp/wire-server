#!/usr/bin/env bash

set -euo pipefail

# Prepare local helm charts by clearing repositories and updating/packaging charts
# Usage: prepare-local-charts.sh [chart1 chart2 ...]
# If no arguments provided, processes all charts in .local/charts

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
CHARTS_DIR="$SCRIPT_DIR/../../.local/charts"

# get rid of all helm repositories
# We need to deal with helm repo list failing because of https://github.com/helm/helm/issues/10028
(helm repo list -o json || echo '[]') | jq -r '.[] | .name' | xargs -I% helm repo remove %

cd "$CHARTS_DIR"

# If arguments provided, process those charts; otherwise process all subdirectories
if [[ $# -gt 0 ]]; then
  charts=("$@")
else
  # Find all chart directories (those containing Chart.yaml)
  charts=()
  for dir in */; do
    if [[ -f "$dir/Chart.yaml" ]]; then
      charts+=("${dir%/}")
    fi
  done
fi

for chart in "${charts[@]}"; do
  ../../hack/bin/update.sh "$chart"
  helm package "$chart"
done
