#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
export NAMESPACE=${NAMESPACE:-test-integration}
# Available $HELMFILE_ENV profiles: default, default-ssl, kind, kind-ssl
HELMFILE_ENV=${HELMFILE_ENV:-default}
CHARTS_DIR="${TOP_LEVEL}/.local/charts"
HELM_PARALLELISM=${HELM_PARALLELISM:-1}

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
charts=(fake-aws databases-ephemeral rabbitmq wire-server ingress-nginx-controller nginx-ingress-services)
mkdir -p ~/.parallel && touch ~/.parallel/will-cite
printf '%s\n' "${charts[@]}" | parallel -P "${HELM_PARALLELISM}" "$DIR/update.sh" "$CHARTS_DIR/{}"

export NAMESPACE_1="$NAMESPACE"
export FEDERATION_DOMAIN_BASE_1="$NAMESPACE_1.svc.cluster.local"
export FEDERATION_DOMAIN_1="federation-test-helper.$FEDERATION_DOMAIN_BASE_1"

export NAMESPACE_2="$NAMESPACE-fed2"
export FEDERATION_DOMAIN_BASE_2="$NAMESPACE_2.svc.cluster.local"
export FEDERATION_DOMAIN_2="federation-test-helper.$FEDERATION_DOMAIN_BASE_2"

echo "Fetch federation-ca secret from cert-manager namespace"
FEDERATION_CA_CERTIFICATE=$(kubectl -n cert-manager get secret federation-ca -o json -o jsonpath="{.data['tls\.crt']}" | base64 -d)
export FEDERATION_CA_CERTIFICATE

copy_federator_ca_secret() {
    local target_ns=$1
    local release_name=${2:-ingress-svc}
    local ca_b64
    ca_b64=$(printf "%s" "${FEDERATION_CA_CERTIFICATE}" | base64 | tr -d '\n')
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
