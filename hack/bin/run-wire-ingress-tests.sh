#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."

# Require an explicit choice: this must match the Envoy Gateway controller.
: "${WIRE_INGRESS_XDS_NAME_SCHEME_V2:?Set to true or false to match the controller}"
work=$(mktemp -d)
namespace=
cleanup() {
    if [[ $? != 0 && -n "$namespace" ]]; then
        kubectl -n "$namespace" logs job/tls-scan >&2 || true
        kubectl -n "$namespace" get pods >&2 || true
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

for profile in integration/ingress/*/values.yaml; do
    profile=${profile%/values.yaml}
    echo "Testing ${profile##*/} TLS profile"
    # generateName avoids collisions and cleanup only deletes namespaces we created.
    namespace=$(kubectl create -f - -o jsonpath='{.metadata.name}' <<'YAML'
apiVersion: v1
kind: Namespace
metadata:
  generateName: wire-ingress-tls-
YAML
    )
    kubectl -n "$namespace" create secret tls server-tls \
        --cert="$work/server.pem" --key="$work/server.key"
    helm install tls-test charts/wire-ingress -n "$namespace" \
        -f integration/ingress/values.yaml -f "$profile/values.yaml" \
        --set "gateway.className=${WIRE_INGRESS_GATEWAY_CLASS:-envoy}" \
        --set "gateway.patchPolicies.xdsNameSchemeV2=$WIRE_INGRESS_XDS_NAME_SCHEME_V2"
    kubectl -n "$namespace" wait gateway/tls-test listenerset/tls-test-wire-ingress \
        --for=condition=Programmed --timeout=180s
    address=$(kubectl -n "${ENVOY_GATEWAY_NAMESPACE:-envoy-gateway-system}" get service \
        -l "gateway.envoyproxy.io/owning-gateway-namespace=$namespace" \
        -o json | jq -er '.items | if length == 1 then .[0].spec.clusterIP else error("Expected one proxy service") end')
    kubectl -n "$namespace" create configmap tls-scan \
        --from-file=ca.pem="$work/cert.pem" --from-file=policy.json="$profile/tls.json" \
        --from-literal=target="ingress.example.test:443{$address}"
    kubectl -n "$namespace" create -f integration/ingress/scan-job.yaml
    # Stop promptly on either success or failure, including scheduling/image-pull timeouts.
    deadline=$((SECONDS + 330))
    state=Running
    while [[ "$state" == Running && $SECONDS -lt $deadline ]]; do
        state=$(kubectl -n "$namespace" get job tls-scan -o json | jq -r '
            [.status.conditions[]? | select(.status == "True" and (.type == "Complete" or .type == "Failed"))][0].type // "Running"')
        if [[ "$state" == Running ]]; then sleep 2; fi
    done
    [[ "$state" == Complete ]] || { echo "TLS scan job: $state" >&2; exit 1; }
    kubectl -n "$namespace" logs job/tls-scan > "$work/scan.json"
    # SSLyze allowlists forbid extras; also require a completed scan and every expected TLS version.
    jq -e --slurpfile policy "$profile/tls.json" '
        .server_scan_results | length == 1 and all(.[];
            .connectivity_status == "COMPLETED" and .scan_status == "COMPLETED" and
            ([.scan_result | to_entries[] | select(.key | test("^tls_.*_cipher_suites$")) |
              select(.value.result.accepted_cipher_suites | length > 0) |
              .value.result.tls_version_used] | sort) ==
            ([$policy[0].tls_versions[] | gsub("v|\\."; "_")] | sort))
    ' "$work/scan.json"
    echo "${profile##*/} TLS profile passed"
    kubectl delete namespace "$namespace" --wait=false
    namespace=
done
