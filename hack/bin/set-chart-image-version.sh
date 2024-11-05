#!/usr/bin/env bash

USAGE="$0 <docker-tag> <chart-name>..."
docker_tag=${1?$USAGE}
charts=${*:2}

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHARTS_DIR="$TOP_LEVEL/.local/charts"

for chart in $charts
do
if [[ "$chart" == "nginz" ]]; then
    # nginz has a different docker tag indentation
    sed -i "s/^    tag: .*/    tag: $docker_tag/g" "$CHARTS_DIR/$chart/values.yaml"
else
    sed -i "s/^  tag: .*/  tag: $docker_tag/g" "$CHARTS_DIR/$chart/values.yaml"
fi
done
