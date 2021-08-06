#!/bin/bash -e

: ${HELM_SERVER_PORT:=4001}

# get rid of all helm repositories
helm repo list -o json | jq -r '.[] | .name' | xargs -I% helm repo remove %

cd "$(dirname "$BASH_SOURCE[0]")/../../.local/charts"
for chart in $@; do
  ../../hack/bin/update.sh "$chart"
  helm package "$chart"
done
helm repo index .
python -m http.server $HELM_SERVER_PORT
