#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NAMESPACE=${NAMESPACE:-test-integration}
# set to 1 to disable running helm tests in parallel
HELM_PARALLELISM=${HELM_PARALLELISM:-1}
CLEANUP_LOCAL_FILES=${CLEANUP_LOCAL_FILES:-1} # set to 0 to keep files
UPLOAD_LOGS=${UPLOAD_LOGS:-0}

echo "Running integration tests on wire-server with parallelism=${HELM_PARALLELISM} ..."

CHART=wire-server
tests=(integration) # stern galley cargohold gundeck federator spar brig)

cleanup() {
    if ((CLEANUP_LOCAL_FILES > 0)); then
        for t in "${tests[@]}"; do
            rm -f "stat-$t"
            rm -f "logs-$t"
        done
    fi
}

# Copy to the concourse output (indetified by $OUTPUT_DIR) for propagation to
# following steps.
copyToAwsS3() {
    build_ts=$(date +%s)
    if ((UPLOAD_LOGS > 0)); then
        for t in "${tests[@]}"; do
            echo "Copy logs-$t to s3://wire-server-test-logs/test-logs-$VERSION/$t-$VERSION-$build_ts.log"
            aws s3 cp "logs-$t" "s3://wire-server-test-logs/test-logs-$VERSION/$t-$VERSION-$build_ts.log"
        done
    fi
}

summary() {
    echo "==============="
    echo "=== summary ==="
    echo "==============="
    printf '%s\n' "${tests[@]}" | parallel echo "=== tail {}: ===" ';' tail -2 logs-{}

    for t in "${tests[@]}"; do
        x=$(cat "stat-$t")
        if ((x > 0)); then
            echo "$t-integration FAILED ❌. pfff..."
        else
            echo "$t-integration passed ✅."
        fi
    done
}

# Copy the secrets from the wire-federation-v0 namespace to the current namespace to be able to delete RabbitMQ queues that are created by the integration tests to avoid overflows
kubectl -n wire-federation-v0 get secrets rabbitmq -ojson | jq 'del(.metadata.namespace) | .metadata.name="rabbitmq-v0"' | kubectl -n $NAMESPACE apply -f -
kubectl -n wire-federation-v1 get secrets rabbitmq -ojson | jq 'del(.metadata.namespace) | .metadata.name="rabbitmq-v1"' | kubectl -n $NAMESPACE apply -f -

# Run tests in parallel using GNU parallel (see https://www.gnu.org/software/parallel/)
# The below commands are a little convoluted, but we wish to:
# - run integration tests. If they fail, keep track of this, but still go and get logs, so we see what failed
# - run all tests. Perhaps multiple flaky tests in multiple services exist, if so, we wish to see all problems
mkdir -p ~/.parallel && touch ~/.parallel/will-cite
printf '%s\n' "${tests[@]}" | parallel echo "Running helm tests for {}..."
printf '%s\n' "${tests[@]}" | parallel -P "${HELM_PARALLELISM}" \
    helm test -n "${NAMESPACE}" "${CHART}" --timeout 900s --filter name="${CHART}-{}-integration" '> logs-{};' \
    echo '$? > stat-{};' \
    echo "==== Done testing {}. ====" '};' \
    kubectl -n "${NAMESPACE}" logs "${CHART}-{}-integration" '>> logs-{};'

summary

# in case any integration test suite failed, exit this script with an error.
exit_code=0
for t in "${tests[@]}"; do
    x=$(cat "stat-$t")
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
        x=$(cat "stat-$t")
        if ((x > 0)); then
            echo "=== logs for failed $t-integration ==="
            cat "logs-$t"
        fi
    done
    summary
    for t in "${tests[@]}"; do
        x=$(cat "stat-$t")
        if ((x > 0)); then
            echo "=== (relevant) logs for failed $t-integration ==="
            "$DIR/integration-logs-relevant-bits.sh" <"logs-$t"
        fi
    done
    summary
fi

copyToAwsS3
cleanup

if ((exit_code > 0)); then
    echo "Tests failed."
    exit 1
else
    echo "All integration tests passed ✅."
fi
