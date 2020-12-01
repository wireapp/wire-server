#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."

NAMESPACE=${NAMESPACE:-test-integration}

echo "Running integration tests on wire-server"

INTEGRATION_RESULTS=/tmp/results
INTEGRATION_LOGS=/tmp/logs
FAIL_LOG_LINES=150

var=wire-server-integration
helm test -n "${NAMESPACE}" "${NAMESPACE}-${var}" --timeout 600s | tee "$INTEGRATION_RESULTS"
exit_test=${PIPESTATUS[0]}
(( exit_status = exit_status || $exit_test ))
if [[ $exit_test -eq 0 ]]; then
    echo "%%%%%%%%%%%%%%%%%%% ${var}-integration: OK"
    for chart in brig galley gundeck cargohold spar; do
        echo "%%%%%%%%%%%%%%%%%%% Chart: $chart"
        kubectl --namespace ${NAMESPACE} logs po/${NAMESPACE}-${var}-${chart}-integration | tail -2
    done
else
    echo "%%%%%%%%%%%%%%%%%%% ${var}-integration: FAIL"
    echo "%%%%%%%%%%%%%%%%%%% Following: Last $FAIL_LOG_LINES lines of logs of services"
    for chart in brig galley gundeck cargohold spar; do
        echo "%%%%%%%%%%%%%%%%%%% Chart: $chart"

        pods=$(kubectl --namespace ${NAMESPACE} get pods | grep "$chart" | grep -v integration | awk '{print $1}' | sort -u)
        for pod in $pods; do
            echo "%%%%%%%%%%%%%%%%%%% Chart: $chart, Pod: $pod"
            kubectl --namespace ${NAMESPACE} logs ${pod} --tail $FAIL_LOG_LINES
        done

    done
    echo ""
    echo "%%%%%%%%%%%%%%%%%%% failing tests:"
    echo ""

    # ugh!
    grep FAILED "$INTEGRATION_RESULTS" | awk -F '`' '{print $2}' > "$INTEGRATION_LOGS" && sync && bash "$INTEGRATION_LOGS"
fi
echo ""

exit ${exit_status}  # 0 if they all succeeded, 1 if any failed
