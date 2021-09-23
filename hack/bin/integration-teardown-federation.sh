#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

NAMESPACE=${NAMESPACE:-test-integration}
export NAMESPACE_1=$NAMESPACE
export NAMESPACE_2=$NAMESPACE_1-fed2
export FEDERATION_DOMAIN_1="." # doesn't matter for destruction
export FEDERATION_DOMAIN_2="." # doesn't matter for destruction

set -ex

helmfile --file "${TOP_LEVEL}/hack/helmfile.yaml" destroy
