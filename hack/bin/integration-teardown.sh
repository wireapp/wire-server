#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

NAMESPACE=${NAMESPACE:-test-integration}
export FEDERATION_DOMAIN="." # doesn't matter for destruction

set -ex

helmfile --file "${TOP_LEVEL}/hack/helmfile.yaml" destroy
