#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

set -ex

NAMESPACE=${NAMESPACE:-test-integration}
export NAMESPACE_1="$NAMESPACE"
export NAMESPACE_2="$NAMESPACE-fed2"
export FEDERATION_DOMAIN_1="." # doesn't matter for destruction
export FEDERATION_DOMAIN_2="." # doesn't matter for destruction

helmfile --file "${TOP_LEVEL}/hack/helmfile.yaml" destroy
