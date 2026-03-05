#!/usr/bin/env bash

USAGE="$0 <target-backend-version>"
target_version=${1?$USAGE}

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHARTS_DIR="$TOP_LEVEL/.local/charts"

charts=(cassandra-migrations elasticsearch-index federator backoffice integration wire-server-enterprise)

for chart in "${charts[@]}"; do
    values_file="$CHARTS_DIR/$chart/values.yaml"
    if [[ -f "$values_file" ]]; then
        sed -i "s/^  tag: .*/  tag: $target_version/g" "$values_file"
    fi
done

# special case nginz
sed -i "s/^    tag: .*/    tag: $target_version/g" "$CHARTS_DIR/nginz/values.yaml"

# Brig, Galley, Cargohold, BackgroundWorker, Cannon, Proxy, Gundeck, and Spar are inlined into the umbrella chart.
sed -i "s/^    tag: .*/    tag: $target_version/g" "$CHARTS_DIR/wire-server/values.yaml"
