#!/usr/bin/env bash

set -xeou pipefail

TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
CHARTS_DIR="$TOP_LEVEL/charts"

cd $1

git checkout master > /dev/null
webappversion=$(git describe --tags)
git checkout $webappversion
ref=$(git rev-parse --short=6 HEAD)
# NOTE: the docker tags seem to get the app-config version wrong. But lets duplicate this bug.
configversion=$(jq -r '.dependencies["wire-web-config-default-staging"]' ./app-config/package.json  | cut -d'#' -f2)


version="$webappversion-$ref-$configversion-production"


sed -i "s/  tag: .*/  tag: $version/g" "$CHARTS_DIR/webapp/values.yaml"


