#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."

# Require an explicit choice: this must match the Envoy Gateway controller.
: "${WIRE_INGRESS_XDS_NAME_SCHEME_V2:?Set to true or false to match the controller}"
work=$(mktemp -d)
namespace=
forward_pid=
cleanup() {
    if [[ -n "$forward_pid" ]]; then
        kill "$forward_pid" 2>/dev/null || true
        wait "$forward_pid" 2>/dev/null || true
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
    service=$(kubectl -n "${ENVOY_GATEWAY_NAMESPACE:-envoy-gateway-system}" get service \
        -l "gateway.envoyproxy.io/owning-gateway-namespace=$namespace" \
        -o json | jq -er '.items | if length == 1 then .[0].metadata.name else error("Expected one proxy service") end')
    kubectl -n "${ENVOY_GATEWAY_NAMESPACE:-envoy-gateway-system}" port-forward \
        "service/$service" :443 --address=127.0.0.1 > "$work/forward.log" 2>&1 &
    forward_pid=$!
    for ((attempt=0; attempt<30; attempt++)); do
        if grep -q 'Forwarding from' "$work/forward.log"; then break; fi
        kill -0 "$forward_pid"
        sleep 1
    done
    cat "$work/forward.log"
    port=$(sed -n 's/.*127.0.0.1:\([0-9]*\) ->.*/\1/p' "$work/forward.log")
    : "${port:?Port-forward did not start}"
    sslyze --custom_tls_config "$profile/tls.json" --certinfo_ca_file "$work/cert.pem" \
        --json_out "$work/scan.json" "ingress.example.test:$port{127.0.0.1}"
    # SSLyze allowlists forbid extras; also require a completed scan and every expected TLS version.
    jq -e --slurpfile policy "$profile/tls.json" '
        .server_scan_results | length == 1 and all(.[];
            .connectivity_status == "COMPLETED" and .scan_status == "COMPLETED" and
            ([.scan_result | to_entries[] | select(.key | test("^tls_.*_cipher_suites$")) |
              select(.value.result.accepted_cipher_suites | length > 0) |
              .value.result.tls_version_used] | sort) ==
            ([$policy[0].tls_versions[] | gsub("v|\\."; "_")] | sort))
    ' "$work/scan.json"
    kill "$forward_pid"
    wait "$forward_pid" 2>/dev/null || true
    forward_pid=
    kubectl delete namespace "$namespace" --wait=false
    namespace=
done
