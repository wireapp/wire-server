#!/usr/bin/env bash

USAGE="$0 <target-backend-version>"
target_version=${1?$USAGE}

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHARTS_DIR="$TOP_LEVEL/.local/charts"

charts=(proxy cassandra-migrations elasticsearch-index federator backoffice integration mlsstats wire-server-enterprise)

for chart in "${charts[@]}"; do
    values_file="$CHARTS_DIR/$chart/values.yaml"
    if [[ -f "$values_file" ]]; then
        sed -i "s/^  tag: .*/  tag: $target_version/g" "$values_file"
    fi
done

# special case nginz: its tag sits one level deeper than the charts above
if [[ -f "$CHARTS_DIR/nginz/values.yaml" ]]; then
    sed -i "s/^    tag: .*/    tag: $target_version/g" "$CHARTS_DIR/nginz/values.yaml"
fi

# Brig, Galley, Cargohold, BackgroundWorker, Cannon, Gundeck, and Spar are inlined into the umbrella chart.
if [[ -f "$CHARTS_DIR/wire-server/values.yaml" ]]; then
    sed -i "s/^    tag: .*/    tag: $target_version/g" "$CHARTS_DIR/wire-server/values.yaml"
fi

# HIP-0015: a chart's helm.sh/images annotation cannot know the tag its own
# images will be published under, so charts commit the placeholder :do-not-use
# and it is stamped here.
#
# Applied to every chart rather than the list above, because charts carrying an
# annotation are not the same set as charts whose values.yaml needs a tag
# rewrite. Charts without the placeholder are unaffected.
for chart_yaml in "$CHARTS_DIR"/*/Chart.yaml; do
    [[ -f "$chart_yaml" ]] || continue
    sed -i "s|:do-not-use|:$target_version|g" "$chart_yaml"
done
