#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
export NAMESPACE=${NAMESPACE:-test-integration}
# Available $HELMFILE_ENV profiles: default, default-ssl, kind, kind-ssl
HELMFILE_ENV=${HELMFILE_ENV:-default}
CHARTS_DIR="${TOP_LEVEL}/.local/charts"
HELM_PARALLELISM=${HELM_PARALLELISM:-1}

. "$DIR/helm_overrides.sh"
${DIR}/integration-cleanup.sh

# FUTUREWORK explore: have helmfile do the interpolation (and skip the "make charts" step) https://wearezeta.atlassian.net/browse/SQPIT-722
#
# FUTUREWORK: get rid of wrapper charts, use helmfile for pinning. Then we may not need the recursive update hack anymore: https://wearezeta.atlassian.net/browse/SQPIT-721
#
# Sadly, even with helmfile, we still need to use use this recursive update
# script beforehand on all relevant charts to download the nested dependencies
# (e.g. cassandra from underneath databases-ephemeral)
echo "updating recursive dependencies ..."
charts=(fake-aws databases-ephemeral redis-cluster rabbitmq wire-server ingress-nginx-controller nginx-ingress-controller nginx-ingress-services)
mkdir -p ~/.parallel && touch ~/.parallel/will-cite
printf '%s\n' "${charts[@]}" | parallel -P "${HELM_PARALLELISM}" "$DIR/update.sh" "$CHARTS_DIR/{}"

KUBERNETES_VERSION_MAJOR="$(kubectl version -o json | jq -r .serverVersion.major)"
KUBERNETES_VERSION_MINOR="$(kubectl version -o json | jq -r .serverVersion.minor)"
KUBERNETES_VERSION_MINOR="${KUBERNETES_VERSION_MINOR//[!0-9]/}" # some clusters report minor versions as a string like '27+'. Strip any non-digit characters.
export KUBERNETES_VERSION="$KUBERNETES_VERSION_MAJOR.$KUBERNETES_VERSION_MINOR"
if (( KUBERNETES_VERSION_MAJOR > 1 || KUBERNETES_VERSION_MAJOR == 1 && KUBERNETES_VERSION_MINOR >= 23 )); then
    export INGRESS_CHART="ingress-nginx-controller"
else
    export INGRESS_CHART="nginx-ingress-controller"
fi
echo "kubeVersion: $KUBERNETES_VERSION and ingress controller=$INGRESS_CHART"
export NAMESPACE_1="$NAMESPACE"
export FEDERATION_DOMAIN_BASE_1="$NAMESPACE_1.svc.cluster.local"
export FEDERATION_DOMAIN_1="federation-test-helper.$FEDERATION_DOMAIN_BASE_1"

export NAMESPACE_2="$NAMESPACE-fed2"
export FEDERATION_DOMAIN_BASE_2="$NAMESPACE_2.svc.cluster.local"
export FEDERATION_DOMAIN_2="federation-test-helper.$FEDERATION_DOMAIN_BASE_2"

echo "Fetch federation-ca secret from cert-manager namespace"
FEDERATION_CA_CERTIFICATE=$(kubectl -n cert-manager get secrets federation-ca -o json -o jsonpath="{.data['tls\.crt']}")
export FEDERATION_CA_CERTIFICATE

echo "Installing charts..."

set +e
helmfile --environment "$HELMFILE_ENV" --file "${TOP_LEVEL}/hack/helmfile.yaml" sync --skip-deps --concurrency 0
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
