#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
export NAMESPACE=${NAMESPACE:-test-integration}
# Available $HELMFILE_ENV profiles: default, default-ssl, kind, kind-ssl
HELMFILE_ENV=${HELMFILE_ENV:-default}
# This controls if integration tests run against ingress-nginx or envoy-gateway
WIRE_INGRESS_MODE=${WIRE_INGRESS_MODE:-nginx}
export WIRE_INGRESS_MODE
ENVOY_GATEWAY_NAMESPACE=${ENVOY_GATEWAY_NAMESPACE:-envoy-gateway-system}
export ENVOY_GATEWAY_NAMESPACE
CHARTS_DIR="${TOP_LEVEL}/.local/charts"
HELM_PARALLELISM=${HELM_PARALLELISM:-1}

changed_files=$(git --no-pager diff-tree --no-commit-id -r --name-only HEAD)

if [[ "$WIRE_INGRESS_MODE" != "nginx" ]] && echo "$changed_files" | grep -q "^charts/nginx-ingress-services"; then
  echo "ERROR: Changes detected in charts/nginx-ingress-services but WIRE_INGRESS_MODE is '${WIRE_INGRESS_MODE}'." >&2
echo "This failure is intentional: changes to nginx-ingress-services are not exercised by the" >&2
echo "integration test suite when running in envoy mode, and would be merged without any test coverage."  >&2
echo "To test these changes, change to WIRE_INGRESS_MODE=nginx. and merge the changes"  >&2
  echo "Then in a follow-up PR change WIRE_INGRESS_MODE=envoy and re-implement the changes also in charts/wire-ingress"
  echo "FUTUREWORK: Remove WIRE_INGRESS_MODE once ingress-nginx is no longer supported by wire-server"
  exit 1
fi

# shellcheck disable=SC1091
. "$DIR/helm_overrides.sh"
"${DIR}"/integration-cleanup.sh

# FUTUREWORK explore: have helmfile do the interpolation (and skip the "make charts" step) https://wearezeta.atlassian.net/browse/SQPIT-722
#
# FUTUREWORK: get rid of wrapper charts, use helmfile for pinning. Then we may not need the recursive update hack anymore: https://wearezeta.atlassian.net/browse/SQPIT-721
#
# Sadly, even with helmfile, we still need to use use this recursive update
# script beforehand on all relevant charts to download the nested dependencies
# (e.g. cassandra from underneath databases-ephemeral)
echo "updating recursive dependencies ..."
charts=(fake-aws databases-ephemeral rabbitmq wire-server ingress-nginx-controller nginx-ingress-services wire-ingress)
mkdir -p ~/.parallel && touch ~/.parallel/will-cite
printf '%s\n' "${charts[@]}" | parallel -P "${HELM_PARALLELISM}" "$DIR/update.sh" "$CHARTS_DIR/{}"

export NAMESPACE_1="$NAMESPACE"
export NAMESPACE_2="$NAMESPACE-fed2"

if [[ "$WIRE_INGRESS_MODE" == "nginx" ]]; then
  export FEDERATION_DOMAIN_BASE_1="${NAMESPACE_1}.svc.cluster.local"
  export FEDERATION_DOMAIN_1="federation-test-helper.${FEDERATION_DOMAIN_BASE_1}"
  export FEDERATION_DOMAIN_BASE_2="${NAMESPACE_2}.svc.cluster.local"
  export FEDERATION_DOMAIN_2="federation-test-helper.${FEDERATION_DOMAIN_BASE_2}"
else
  export FEDERATION_DOMAIN_BASE_1="${ENVOY_GATEWAY_NAMESPACE}.svc.cluster.local"
  export FEDERATION_DOMAIN_1="${NAMESPACE_1}-fed.${FEDERATION_DOMAIN_BASE_1}"
  export FEDERATION_DOMAIN_BASE_2="${ENVOY_GATEWAY_NAMESPACE}.svc.cluster.local"
  export FEDERATION_DOMAIN_2="${NAMESPACE_2}-fed.${FEDERATION_DOMAIN_BASE_2}"
fi

echo "Fetch federation-ca secret from cert-manager namespace"
FEDERATION_CA_CERTIFICATE=$(kubectl -n cert-manager get secrets federation-ca -o json -o jsonpath="{.data['tls\.crt']}" | base64 -d)
export FEDERATION_CA_CERTIFICATE

copy_federator_ca_secret() {
    local target_ns=$1
    local release_name=${2:-ingress-svc}
    local ca_b64
    ca_b64=$(kubectl -n wire-federation-v0 get secret federator-ca-secret -o jsonpath='{.data.ca\.crt}' 2>/dev/null || true)
    if [[ -z "${ca_b64}" ]]; then
        echo "federator-ca-secret not found in wire-federation-v0; aborting" >&2
        exit 1
    fi
    kubectl get ns "${target_ns}" >/dev/null 2>&1 || kubectl create ns "${target_ns}"
    kubectl -n "${target_ns}" apply -f - <<EOF
apiVersion: v1
kind: Secret
metadata:
  name: federator-ca-secret
  labels:
    app.kubernetes.io/managed-by: "Helm"
  annotations:
    meta.helm.sh/release-name: "${release_name}"
    meta.helm.sh/release-namespace: "${target_ns}"
type: Opaque
data:
  ca.crt: ${ca_b64}
EOF
}

echo "Ensure namespaces exist and seed federator CA secret before helmfile"
copy_federator_ca_secret "$NAMESPACE_1" "ingress-svc"
copy_federator_ca_secret "$NAMESPACE_2" "ingress-svc"

echo "Installing charts..."

set +e
helmfile --environment "$HELMFILE_ENV" --file "${TOP_LEVEL}/hack/helmfile.yaml.gotmpl" sync --concurrency 0
EXIT_CODE=$?

if (( EXIT_CODE > 0)); then
    echo "!! Helm install failed. Attempting to get some more information ..."

    kubectl -n "$NAMESPACE_1" get events | grep -v "Normal "
    kubectl -n "$NAMESPACE_2" get events | grep -v "Normal "
    "${DIR}/kubectl-get-debug-info.sh" "$NAMESPACE_1"
    "${DIR}/kubectl-get-debug-info.sh" "$NAMESPACE_2"
    exit $EXIT_CODE
fi
set -e

# Envoy Gateway creates the proxies asynchronously, outside the Helm releases,
# so helmfile does not wait for them. Wait until every Gateway is programmed,
# i.e. its proxy has available replicas.
if [[ "$WIRE_INGRESS_MODE" == "envoy" ]]; then
    for namespace in "$NAMESPACE_1" "$NAMESPACE_2"; do
        echo "Waiting for Gateways in '$namespace' to be programmed..."
        if ! kubectl -n "$namespace" wait gateways.gateway.networking.k8s.io --all --for=condition=Programmed --timeout=300s; then
            echo "!! Gateways in '$namespace' not programmed. Attempting to get some more information ..."
            kubectl -n "$namespace" get gateways.gateway.networking.k8s.io -o yaml
            kubectl -n "$ENVOY_GATEWAY_NAMESPACE" get pods -o wide -l "gateway.envoyproxy.io/owning-gateway-namespace=$namespace"
            exit 1
        fi
    done
fi

# wait for fakeSNS to create resources. TODO, cleaner: make initiate-fake-aws-sns a post hook. See cassandra-migrations chart for an example.
resourcesReady() {
    SNS_POD=$(kubectl -n "${NAMESPACE_1}" get pods | grep fake-aws-sns | grep Running | awk '{print $1}')
    kubectl -n "${NAMESPACE_1}" logs "$SNS_POD" -c initiate-fake-aws-sns | grep created

    SNS_POD=$(kubectl -n "${NAMESPACE_2}" get pods | grep fake-aws-sns | grep Running | awk '{print $1}')
    kubectl -n "${NAMESPACE_2}" logs "$SNS_POD" -c initiate-fake-aws-sns | grep created
}
until resourcesReady; do
    echo 'waiting for SNS resources'
    sleep 1
done

echo "done"
