#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export NAMESPACE=${NAMESPACE:-test-integration}

$DIR/integration-teardown.sh

# The suffix '-fed2' must be kept in sync with configuration inside
# charts/brig/templates/tests/configmap.yaml and
# hack/bin/integration-setup-federation.sh
export NAMESPACE=${NAMESPACE}-fed2

$DIR/integration-teardown.sh
