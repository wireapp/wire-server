#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NAMESPACE=${NAMESPACE:-test-integration}
# set to 1 to disable running helm tests in parallel
HELM_PARALLELISM=${HELM_PARALLELISM:-1}
CLEANUP_LOCAL_FILES=${CLEANUP_LOCAL_FILES:-1} # set to 0 to keep files
OUTPUT_DIR=./test-logs

echo "Running integration tests on wire-server with parallelism=${HELM_PARALLELISM} ..."

CHART=wire-server
tests=(galley cargohold gundeck federator spar brig)

mkdir -p $OUTPUT_DIR

cleanup() {
    if (( CLEANUP_LOCAL_FILES > 0 )); then
        rm -r $OUTPUT_DIR
    fi
}

summary() {
    echo "==============="
    echo "=== summary ==="
    echo "==============="
    printf '%s\n' "${tests[@]}" | parallel echo "=== tail {}: ===" ';' tail -2 $OUTPUT_DIR/logs-{}

    for t in "${tests[@]}"; do
        x=$(cat "$OUTPUT_DIR/stat-$t")
        if ((x > 0)); then
            echo "$t-integration FAILED ❌. pfff..."
        else
            echo "$t-integration passed ✅."
        fi
    done
}

# Run tests in parallel using GNU parallel (see https://www.gnu.org/software/parallel/)
# The below commands are a little convoluted, but we wish to:
# - run integration tests. If they fail, keep track of this, but still go and get logs, so we see what failed
# - run all tests. Perhaps multiple flaky tests in multiple services exist, if so, we wish to see all problems
mkdir -p ~/.parallel && touch ~/.parallel/will-cite
printf '%s\n' "${tests[@]}" | parallel echo "Running helm tests for {}..."
printf '%s\n' "${tests[@]}" | parallel -P "${HELM_PARALLELISM}" \
    helm test -n "${NAMESPACE}" "${NAMESPACE}-${CHART}" --timeout 900s --filter name="${NAMESPACE}-${CHART}-{}-integration" "> $OUTPUT_DIR/logs-{};" \
    echo "$? >  $OUTPUT_DIR/stat-{};" \
    echo "==== Done testing {}. ====" '};' \
    kubectl -n "${NAMESPACE}" logs "${NAMESPACE}-${CHART}-{}-integration" ">> $OUTPUT_DIR/logs-{};"

summary

# in case any integration test suite failed, exit this script with an error.
exit_code=0
for t in "${tests[@]}"; do
    x=$(cat "$OUTPUT_DIR/stat-$t")
    if ((x > 0)); then
        exit_code=1
    fi
done

if ((exit_code > 0)); then
    echo "======================="
    echo "=== failed job logs ==="
    echo "======================="
    # in case a integration test suite failed, print relevant logs
    for t in "${tests[@]}"; do
        x=$(cat "$OUTPUT_DIR/stat-$t")
        if ((x > 0)); then
            echo "=== logs for failed $t-integration ==="
            cat "$OUTPUT_DIR/logs-$t"
        fi
    done
    summary
    for t in "${tests[@]}"; do
        x=$(cat "$OUTPUT_DIR/stat-$t")
        if ((x > 0)); then
            echo "=== (relevant) logs for failed $t-integration ==="
            "$DIR/integration-logs-relevant-bits.sh" < "$OUTPUT_DIR/logs-$t"
        fi
    done
    summary
fi

cleanup

if ((exit_code > 0)); then
    echo "Tests failed."
    exit 1
else
    echo "All integration tests passed ✅."
fi
