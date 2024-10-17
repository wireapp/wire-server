#!/usr/bin/env bash

set -e

# Used in 'make charts' to create a copy of charts before modifying tags and helm versions.

USAGE="Copy charts from top-level chart folder source to ./.local/charts $0 <chart-name>"
CHART=${1:?$USAGE}

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHART_SOURCE=$TOP_LEVEL/charts
CHART_DEST=$TOP_LEVEL/.local/charts

# TODO sanity check folder must exist

mkdir -p .local/charts
rm -rf "${CHART_DEST:?}/$CHART"
cp -r "$CHART_SOURCE/$CHART" "$CHART_DEST/"

if [ -f "$CHART_SOURCE/$CHART/requirements.yaml" ]; then
  # very hacky bash, I'm sorry
  for subpath in $(grep "file://" "$CHART_SOURCE/$CHART/requirements.yaml" | awk '{ print $2 }' | xargs -n 1 | cut -c 8-)
  do
    rm -rf "${CHART_DEST:?}/$CHART/$subpath"
    cp -r "$CHART_SOURCE/$CHART/$subpath" "$CHART_DEST/"
  done
fi

echo "copied $CHART_SOURCE/$CHART (and its local dependencies) to $CHART_DEST/$CHART"
