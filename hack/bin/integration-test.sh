#!/usr/bin/env bash
set -euo pipefail

NAMESPACE=${NAMESPACE:-test-integration}

echo "Running integration tests on wire-server"

CHART=wire-server
helm test --logs -n "${NAMESPACE}" "${NAMESPACE}-${CHART}" --timeout 600s
