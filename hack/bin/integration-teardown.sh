#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

NAMESPACE=${NAMESPACE:-test-integration}
# doesn't matter for destruction but needs to be set
export FEDERATION_DOMAIN="."

set -ex

. "$DIR/helm_overrides.sh"
helmfile --file "${TOP_LEVEL}/hack/helmfile-single.yaml" destroy
