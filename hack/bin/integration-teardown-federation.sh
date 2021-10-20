#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

set -ex

NAMESPACE=${NAMESPACE:-test-integration}
export NAMESPACE_1="$NAMESPACE"
export NAMESPACE_2="$NAMESPACE-fed2"
# these don't matter for destruction but have to be set.
export FEDERATION_DOMAIN_1="."
export FEDERATION_DOMAIN_2="."

. "$DIR/helm_overrides.sh"
helmfile --file "${TOP_LEVEL}/hack/helmfile.yaml" destroy
