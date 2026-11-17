#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."

chart=charts/wire-ingress
if [[ -z "${1:-}" ]]; then
    "$chart/test/check-scan-tests.sh"
    # Give each child's EXIT trap time to delete its namespace on cancellation.
    for values in "$chart"/test/*/values.yaml; do basename "$(dirname "$values")"; done |
        parallel --tag --line-buffer --term-seq TERM,10000,KILL,25 --jobs "${HELM_PARALLELISM:-2}" "$0" '{}'
    exit
fi
profile=$1
[[ "$profile" =~ ^[a-z0-9-]+$ && -f "$chart/test/$profile/values.yaml" ]] || { echo "Unknown TLS profile: $profile" >&2; exit 2; }
mkdir -p tmp/ingress-tests
results=$(mktemp -d "$PWD/tmp/ingress-tests/$profile.XXXXXX")
echo "TLS test results: $results"
namespace=
cleanup() {
    local status=$?
    if ((status != 0 && status != 130 && status != 143)) && [[ -n "$namespace" ]]; then
        kubectl -n "$namespace" logs pod/tls-test-tls-scan > "$results/scan.log" 2>&1 || true
        cat "$results/scan.log" >&2
        kubectl -n "$namespace" get gateway,listenerset,envoypatchpolicy,pod -o yaml > "$results/resources.yaml" 2>&1 || true
        kubectl -n "$namespace" get events --sort-by=.lastTimestamp > "$results/events.log" 2>&1 || true
        echo "Failure diagnostics: $results" >&2
    fi
    if [[ -n "$namespace" ]]; then
        kubectl --request-timeout=5s delete namespace "$namespace" --wait=false || true
    fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# One namespace per process: profile runs share no mutable state or resources.
namespace=$(jq -n --arg owner "${NAMESPACE:-standalone}" '
    {apiVersion:"v1", kind:"Namespace", metadata:{generateName:"test-wire-ingress-",
     labels:{"wire.com/ingress-test-owner":$owner}}}' |
    kubectl create -f - -o jsonpath='{.metadata.name}')
echo "$profile: namespace $namespace"
helm install tls-test "$chart" -n "$namespace" \
    -f "$chart/test/values.yaml" -f "$chart/test/$profile/values.yaml" \
    --set "tests.profile=$profile" \
    --set "gateway.className=${WIRE_INGRESS_GATEWAY_CLASS:-envoy}" \
    --set "gateway.controllerNamespace=${ENVOY_GATEWAY_NAMESPACE:-envoy-gateway-system}" \
    --set "gateway.envoyProxy.spec.provider.kubernetes.envoyService.name=$namespace" \
    --set "gateway.patchPolicies.xdsNameSchemeV2=${WIRE_INGRESS_XDS_NAME_SCHEME_V2:-true}"
kubectl -n "$namespace" wait gateway/tls-test listenerset/tls-test-wire-ingress \
    --for=condition=Programmed --timeout=180s
helm test tls-test -n "$namespace" --timeout=330s
kubectl -n "$namespace" logs pod/tls-test-tls-scan > "$results/scan.json"
jq -e --slurpfile policy "$chart/test/$profile/tls.json" \
    -f "$chart/test/check-scan.jq" "$results/scan.json" > /dev/null
echo "$profile TLS profile passed"
