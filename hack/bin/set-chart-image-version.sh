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
elif [[ "$chart" == "wire-server" ]]; then
    # Anchored to quay.io/wire/ repository: lines so non-wire images (cannon's
    # alpine configuratorImage) keep their own tag instead of being stamped.
    sed -i -E "/^[[:space:]]*repository: quay\.io\/wire\//{n; s/^([[:space:]]*)tag: .*/\1tag: $docker_tag/}" "$CHARTS_DIR/$chart/values.yaml"
elif [[ "$chart" == "cassandra-migrations" ]]; then
    # cassandra-migrations shares one images.tag with no adjacent repository:
    # line, so anchor to the images: block to avoid stamping jobDoneImage.
    sed -i -E "/^images:/,/^[^[:space:]]/ s/^  tag: .*/  tag: $docker_tag/" "$CHARTS_DIR/$chart/values.yaml"
else
    sed -i "s/^  tag: .*/  tag: $docker_tag/g" "$CHARTS_DIR/$chart/values.yaml"
fi
done
