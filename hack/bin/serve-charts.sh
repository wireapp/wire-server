#!/usr/bin/env bash

set -euo pipefail

: ${HELM_SERVER_PORT:=4001}

# get rid of all helm repositories
# We need to deal with helm repo list failing because of https://github.com/helm/helm/issues/10028
(helm repo list -o json || echo '[]') | jq -r '.[] | .name' | xargs -I% helm repo remove %

cd "$(dirname "$BASH_SOURCE[0]")/../../.local/charts"
for chart in $@; do
  ../../hack/bin/update.sh "$chart"
  helm package "$chart"
done
helm repo index .
python -m http.server $HELM_SERVER_PORT
