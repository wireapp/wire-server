#!/usr/bin/env bash

USAGE="Usage: $0"

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$DIR/../.."

export NAMESPACE=${NAMESPACE:-test-integration}

$DIR/integration-setup.sh

# The suffix '-fed2' must be kept in sync with configuration inside
# charts/brig/templates/tests/configmap.yaml and
# hack/bin/integration-teardown-federation.sh
export NAMESPACE=${NAMESPACE}-fed2

$DIR/integration-setup.sh
