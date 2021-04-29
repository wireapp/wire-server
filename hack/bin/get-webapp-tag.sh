#!/usr/bin/env bash

set -eou pipefail

cd $1

git checkout master
webappversion=$(git describe --tags)
git checkout $webappversion
ref=$(git rev-parse --short=6 HEAD)
# NOTE: the docker tags seem to get the app-config version wrong. But lets duplicate this bug.
configversion=$(jq -r '.dependencies["wire-web-config-default-staging"]' ./app-config/package.json  | cut -d'#' -f2)

repository="quay.io/wire/webapp"

image="$repository:$webappversion-$ref-$configversion-production"

echo "$image"


