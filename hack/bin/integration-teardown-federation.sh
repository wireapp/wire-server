#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

set -ex

HELMFILE_ENV=${HELMFILE_ENV:-default}
NAMESPACE=${NAMESPACE:-test-integration}
export NAMESPACE_1="$NAMESPACE"
export NAMESPACE_2="$NAMESPACE-fed2"
# these don't matter for destruction but have to be set.
export FEDERATION_DOMAIN_1="."
export FEDERATION_DOMAIN_2="."
export FEDERATION_DOMAIN_BASE_1="."
export FEDERATION_DOMAIN_BASE_2="."
export FEDERATION_CA_CERTIFICATE="."
export ENTERPRISE_IMAGE_PULL_SECRET="."
export INGRESS_CHART="ingress-nginx-controller"

# shellcheck disable=SC1091
. "$DIR/helm_overrides.sh"
helmfile --environment "$HELMFILE_ENV" --file "${TOP_LEVEL}/hack/helmfile.yaml.gotmpl" destroy --skip-deps --skip-charts --concurrency 0 || echo "Failed to delete helm deployments, ignoring this failure as next steps will the destroy namespaces anyway."

kubectl delete namespace "$NAMESPACE_1" "$NAMESPACE_2"
