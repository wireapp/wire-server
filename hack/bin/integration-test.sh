#!/usr/bin/env bash

NAMESPACE=${NAMESPACE:-test-integration}

echo "Running integration tests on wire-server"

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHART=wire-server
helm test -n "${NAMESPACE}" "${NAMESPACE}-${CHART}" --timeout 600s |
  "$DIR/integration-test-logs.sh"
