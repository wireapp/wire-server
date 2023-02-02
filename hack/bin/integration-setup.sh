#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
export NAMESPACE=${NAMESPACE:-test-integration}
HELMFILE_ENV=${HELMFILE_ENV:-default}
CHARTS_DIR="${TOP_LEVEL}/.local/charts"

. "$DIR/helm_overrides.sh"

"${DIR}/integration-cleanup.sh"

echo "updating recursive dependencies ..."
charts=(fake-aws databases-ephemeral redis-cluster wire-server nginx-ingress-controller nginx-ingress-services)
touch ~/.parallel/will-cite
printf '%s\n' "${charts[@]}" | parallel "$DIR/update.sh" "$CHARTS_DIR/{}"

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
