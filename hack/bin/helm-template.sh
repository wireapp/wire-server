#!/usr/bin/env bash

# This script can be used to template a helm chart with values filled in from
# hack/helm_vars as overrrides, if available.  This allows debugging helm
# templating issues without actually installing anything, and without needing
# access to a kubernetes cluster
USAGE="Usage: $0"

set -x

set -e

release=${1:?$USAGE}

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
CHARTS_DIR="${TOP_LEVEL}/.local/charts"


# for f in "${DIR}/../helm_vars/${chart}"/*; do
#     options+=(-f "$f")
# done

# valuesfile="${DIR}/../helm_vars/${chart}/values.yaml"
# valuestmplfile="${DIR}/../helm_vars/${chart}/values.yaml.gotmpl"
# certificatesfile="${DIR}/../helm_vars/${chart}/certificates.yaml"
# if [ -f "$valuesfile" ]; then
#     options+=(-f "$valuesfile")
# fi
# if [ -f "$certificatesfile" ]; then
#     options+=(-f "$certificatesfile")
# fi

# "$DIR/update.sh" "$CHARTS_DIR/$chart"
# helm template "$chart" "$CHARTS_DIR/$chart" ${options[*]}

export NAMESPACE_1="test-template"
export FEDERATION_DOMAIN_BASE_1="$NAMESPACE_1.svc.cluster.local"
export FEDERATION_DOMAIN_1="federation-test-helper.$FEDERATION_DOMAIN_BASE_1"

export NAMESPACE_2="test-template-fed2"
export FEDERATION_DOMAIN_BASE_2="$NAMESPACE_2.svc.cluster.local"
export FEDERATION_DOMAIN_2="federation-test-helper.$FEDERATION_DOMAIN_BASE_2"

FEDERATION_CA_CERTIFICATE=$(cat "$TOP_LEVEL/deploy/dockerephemeral/federation-v0/integration-ca.pem")
export FEDERATION_CA_CERTIFICATE

export INGRESS_CHART="ingress-nginx-controller"

charts=(fake-aws databases-ephemeral redis-cluster rabbitmq wire-server ingress-nginx-controller nginx-ingress-controller nginx-ingress-services)

# for chart in "${charts[@]}"; do
#   "$DIR"/update.sh "$CHARTS_DIR/$chart"
# done

helmfile template --environment kind  --skip-deps -f "$TOP_LEVEL/hack/helmfile.yaml" "$release"
