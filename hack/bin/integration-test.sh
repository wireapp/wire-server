#!/usr/bin/env bash
set -euo pipefail

NAMESPACE=${NAMESPACE:-test-integration}

echo "Running integration tests on wire-server"

CHART=wire-server
tests=(cargohold gundeck spar brig galley federator)
# Run tests in parallel using GNU parallel (see https://www.gnu.org/software/parallel/)
printf '%s\n' "${tests[@]}" | parallel echo "Running helm tests for {}..."
printf '%s\n' "${tests[@]}" | parallel \
    helm test -n "${NAMESPACE}" "${NAMESPACE}-${CHART}" --timeout 900s --filter name="${NAMESPACE}-${CHART}-{}-integration" '| grep -v NOTES;' \
    echo "==== Done testing {}. Here are logs: ====" ';' \
    kubectl -n "${NAMESPACE}" logs "${NAMESPACE}-${CHART}-{}-integration" ';' \
    echo "==== Above logs are for {}.===="
