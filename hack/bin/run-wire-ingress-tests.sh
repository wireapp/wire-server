#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."

if [[ -z "${NAMESPACE:-}" && -z "${KUBECONFIG:-}" && -f tmp/kubeconfig ]]; then export KUBECONFIG="$PWD/tmp/kubeconfig"; fi
WIRE_INGRESS_XDS_NAME_SCHEME_V2=${WIRE_INGRESS_XDS_NAME_SCHEME_V2:-true}
profiles=(charts/wire-ingress/test/*/values.yaml)
if [[ -n "${1:-}" ]]; then
    [[ "$1" =~ ^[a-z0-9-]+$ && -f "charts/wire-ingress/test/$1/values.yaml" ]] || { echo "Unknown TLS profile: $1" >&2; exit 2; }
    profiles=("charts/wire-ingress/test/$1/values.yaml")
fi
mkdir -p tmp/ingress-tests
results=$(mktemp -d "$PWD/tmp/ingress-tests/run.XXXXXX")
echo "TLS test results: $results"
work=$(mktemp -d)
namespace=
cleanup() {
    if [[ $? != 0 && -n "$namespace" ]]; then
        kubectl -n "$namespace" logs pod/tls-test-tls-scan > "$results/${profile##*/}.log" 2>&1 || true
        cat "$results/${profile##*/}.log" >&2
        kubectl -n "$namespace" get gateway,listenerset,envoypatchpolicy,pod -o yaml > "$results/${profile##*/}-resources.yaml" 2>&1 || true
        kubectl -n "$namespace" get events --sort-by=.lastTimestamp > "$results/${profile##*/}-events.log" 2>&1 || true
        echo "Failure diagnostics: $results" >&2
    fi
    if [[ -n "$namespace" ]]; then
        kubectl delete namespace "$namespace" --wait=false || true
    fi
    rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

umask 077
openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:P-256 -nodes \
    -keyout "$work/key.pem" -out "$work/cert.pem" -days 2 \
    -subj /CN=wire-ingress-test-ca -addext keyUsage=critical,keyCertSign,cRLSign
openssl req -new -newkey ec -pkeyopt ec_paramgen_curve:P-256 -nodes \
    -keyout "$work/server.key" -out "$work/server.csr" -subj /CN=ingress.example.test
openssl x509 -req -in "$work/server.csr" -CA "$work/cert.pem" -CAkey "$work/key.pem" \
    -CAcreateserial -days 2 -out "$work/server.pem" -extfile <(printf "%s\n" \
        "basicConstraints=critical,CA:false" "keyUsage=critical,digitalSignature" \
        "extendedKeyUsage=serverAuth" "subjectAltName=DNS:ingress.example.test")

for profile in "${profiles[@]}"; do
    profile=${profile%/values.yaml}
    echo "Testing ${profile##*/} TLS profile"
    # generateName avoids collisions and cleanup only deletes namespaces we created.
    namespace=$(jq -n --arg owner "${NAMESPACE:-standalone}" '
        {apiVersion:"v1", kind:"Namespace", metadata:{generateName:"test-wire-ingress-",
         labels:{"wire.com/ingress-test-owner":$owner}}}' |
        kubectl create -f - -o jsonpath='{.metadata.name}')
    echo "${profile##*/}: namespace $namespace"
    kubectl -n "$namespace" create secret tls server-tls \
        --cert="$work/server.pem" --key="$work/server.key"
    helm install tls-test charts/wire-ingress -n "$namespace" \
        -f charts/wire-ingress/test/values.yaml -f "$profile/values.yaml" \
        --set "gateway.className=${WIRE_INGRESS_GATEWAY_CLASS:-envoy}" \
        --set "gateway.patchPolicies.xdsNameSchemeV2=$WIRE_INGRESS_XDS_NAME_SCHEME_V2"
    kubectl -n "$namespace" wait gateway/tls-test listenerset/tls-test-wire-ingress \
        --for=condition=Programmed --timeout=180s
    address=$(kubectl -n "${ENVOY_GATEWAY_NAMESPACE:-envoy-gateway-system}" get service \
        -l "gateway.envoyproxy.io/owning-gateway-namespace=$namespace" \
        -o json | jq -er '.items | if length == 1 then .[0].spec.clusterIP else error("Expected one proxy service") end')
    kubectl -n "$namespace" create configmap tls-test-tls-scan \
        --from-file=ca.pem="$work/cert.pem" --from-file=policy.json="$profile/tls.json" \
        --from-literal=target="ingress.example.test:443{$address}"
    helm test tls-test -n "$namespace" --timeout=330s
    kubectl -n "$namespace" logs pod/tls-test-tls-scan > "$results/${profile##*/}.json"
    # SSLyze allowlists forbid extras; also require a completed scan and every expected TLS version.
    jq -e --slurpfile policy "$profile/tls.json" '
        .server_scan_results | length == 1 and all(.[];
            .connectivity_status == "COMPLETED" and .scan_status == "COMPLETED" and
            ([.scan_result | to_entries[] | select(.key | test("^tls_.*_cipher_suites$")) |
              select(.value.result.accepted_cipher_suites | length > 0) |
              .value.result.tls_version_used] | sort) ==
            ([$policy[0].tls_versions[] | gsub("v|\\."; "_")] | sort))
    ' "$results/${profile##*/}.json" > /dev/null
    echo "${profile##*/} TLS profile passed"
    kubectl delete namespace "$namespace" --wait=false
    namespace=
done
