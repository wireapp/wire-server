#!/usr/bin/env bash

USAGE="$0 <target-backend-version>"
target_version=${1?$USAGE}

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHARTS_DIR="$TOP_LEVEL/.local/charts"

charts=(brig cannon galley gundeck spar cargohold proxy cassandra-migrations elasticsearch-index federator backoffice background-worker integration wire-server-enterprise)

for chart in "${charts[@]}"; do
    sed -i "s/^  tag: .*/  tag: $target_version/g" "$CHARTS_DIR/$chart/values.yaml"
done

# special case nginz
sed -i "s/^    tag: .*/    tag: $target_version/g" "$CHARTS_DIR/nginz/values.yaml"
