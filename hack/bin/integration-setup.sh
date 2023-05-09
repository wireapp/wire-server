#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
export NAMESPACE=${NAMESPACE:-test-integration}
HELMFILE_ENV=${HELMFILE_ENV:-default}
CHARTS_DIR="${TOP_LEVEL}/.local/charts"
HELM_PARALLELISM=${HELM_PARALLELISM:-1}

. "$DIR/helm_overrides.sh"

"${DIR}/integration-cleanup.sh"

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
echo "Generating self-signed certificates..."
export FEDERATION_DOMAIN_BASE="$NAMESPACE.svc.cluster.local"
export FEDERATION_DOMAIN="federation-test-helper.$FEDERATION_DOMAIN_BASE"
"$DIR/selfsigned-kubernetes.sh" namespace1

echo "Installing charts..."

helmfile --environment "$HELMFILE_ENV" --file "${TOP_LEVEL}/hack/helmfile-single.yaml" sync

# wait for fakeSNS to create resources. TODO, cleaner: make initiate-fake-aws-sns a post hook. See cassandra-migrations chart for an example.
resourcesReady() {
    SNS_POD=$(kubectl -n "${NAMESPACE}" get pods | grep fake-aws-sns | grep Running | awk '{print $1}')
    kubectl -n "${NAMESPACE}" logs "$SNS_POD" -c initiate-fake-aws-sns | grep created
}
until resourcesReady; do
    echo 'waiting for SNS resources'
    sleep 1
done

kubectl -n "${NAMESPACE}" get pods

echo "done"
