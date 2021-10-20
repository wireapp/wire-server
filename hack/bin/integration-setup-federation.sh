#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
export NAMESPACE=${NAMESPACE:-test-integration}
HELMFILE_ENV=${HELMFILE_ENV:-default}
CHARTS_DIR="${TOP_LEVEL}/.local/charts"

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
charts=(fake-aws databases-ephemeral wire-server nginx-ingress-controller nginx-ingress-services)
for chart in "${charts[@]}"; do
    "$DIR/update.sh" "$CHARTS_DIR/$chart"
done

# FUTUREWORK: use helm functions instead, see https://wearezeta.atlassian.net/browse/SQPIT-723
echo "Generating self-signed certificates..."

export NAMESPACE_1="$NAMESPACE"
export FEDERATION_DOMAIN_BASE="$NAMESPACE_1.svc.cluster.local"
export FEDERATION_DOMAIN_1="federation-test-helper.$FEDERATION_DOMAIN_BASE"
"$DIR/selfsigned-kubernetes.sh" namespace1

export NAMESPACE_2="$NAMESPACE-fed2"
export FEDERATION_DOMAIN_BASE="$NAMESPACE_2.svc.cluster.local"
export FEDERATION_DOMAIN_2="federation-test-helper.$FEDERATION_DOMAIN_BASE"
"$DIR/selfsigned-kubernetes.sh" namespace2

echo "Installing charts..."

helmfile --environment "$HELMFILE_ENV" --file "${TOP_LEVEL}/hack/helmfile.yaml" sync

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
