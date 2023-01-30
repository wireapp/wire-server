#!/usr/bin/env bash
set -euo pipefail

NAMESPACE=${NAMESPACE:-test-integration}

echo "Running integration tests on wire-server"

CHART=wire-server
tests=(cargohold gundeck spar brig galley federator)
# Run tests in parallel using GNU parallel (see https://www.gnu.org/software/parallel/)
# The below commands are a little convoluted, but we wish to:
# - run integration tests. If they fail, keep track of this, but still go and get logs, so we see what failed
# - run all tests. Perhaps multiple flaky tests in multiple services exist, if so, we wish to see all problems
# --halt now,fail=1
printf '%s\n' "${tests[@]}" | parallel echo "Running helm tests for {}..."
printf '%s\n' "${tests[@]}" | parallel -P 6 --results results.csv \
    helm test -n "${NAMESPACE}" "${NAMESPACE}-${CHART}" --timeout 900s --filter name="${NAMESPACE}-${CHART}-{}-integration" '> {};' \
    echo '$? > stat-{};' \
    echo "==== Done testing {}. Here are logs: ====" '>> {};' \
    kubectl -n "${NAMESPACE}" logs "${NAMESPACE}-${CHART}-{}-integration" '>> {};'

echo "==============="
echo "=== summary ==="
echo "==============="
printf '%s\n' "${tests[@]}" | parallel echo "=== tail {}: ===" ';' tail -3 {}

echo "======================="
echo "=== failed job logs ==="
echo "======================="
# in case a integration test suite failed, print relevant logs
for t in "${tests[@]}"; do
    x=$(cat "stat-$t")
    if ((x > 0)); then
        cat "$t"
    fi
done

# in case any integration test suite failed, exit this script with an error.
for t in "${tests[@]}"; do
    x=$(cat "stat-$t")
    if ((x > 0)); then
        echo "$t FAILED. pfff..."
        exit 1
    fi
done
