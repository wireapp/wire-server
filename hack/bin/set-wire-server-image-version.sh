#!/usr/bin/env bash

USAGE="$0 <target-backend-version>"
target_version=${1?$USAGE}

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHARTS_DIR="$TOP_LEVEL/.local/charts"

charts=(brig cannon galley gundeck spar cargohold proxy cassandra-migrations elasticsearch-index federator)

for chart in "${charts[@]}"; do
    sed -i "s/^  tag: .*/  tag: $target_version/g" "$CHARTS_DIR/$chart/values.yaml"
done

# special case nginz
sed -i "s/^    tag: .*/    tag: $target_version/g" "$CHARTS_DIR/nginz/values.yaml"

# special case backoffice as there are two images at the same level and we want
# update only one.
sed -i "s/tag: do-not-use/tag: $target_version/g" "$CHARTS_DIR/backoffice/values.yaml"
