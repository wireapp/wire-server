#!/usr/bin/env bash

# This script can be used to template a helm chart with values filled in from
# hack/helm_vars as overrrides, if available.  This allows debugging helm
# templating issues without actually installing anything, and without needing
# access to a kubernetes cluster
USAGE="Usage: $0"

set -e

chart=${1:?$USAGE}

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
CHARTS_DIR="${TOP_LEVEL}/.local/charts"

valuesfile="${DIR}/../helm_vars/${chart}/values.yaml"
certificatesfile="${DIR}/../helm_vars/${chart}/certificates.yaml"
declare -a options=()
if [ -f "$valuesfile" ]; then
    options+=(-f "$valuesfile")
fi
if [ -f "$certificatesfile" ]; then
    options+=(-f "$certificatesfile")
fi

"$DIR/update.sh" "$CHARTS_DIR/$chart"
helm template $"chart" "$CHARTS_DIR/$chart" ${options[*]}
