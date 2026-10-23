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

# shellcheck disable=SC1091
. "$DIR/helm_overrides.sh"
helmfile --environment "$HELMFILE_ENV" --file "${TOP_LEVEL}/hack/helmfile.yaml.gotmpl" destroy --skip-deps --skip-charts --concurrency 0 || echo "Failed to delete helm deployments, ignoring this failure as next steps will the destroy namespaces anyway."

# Envoy mode creates objects outside the test namespaces, which deleting the
# namespaces does not remove. Delete any Gateway left over by a failed helmfile
# destroy first, so Envoy Gateway does not recreate the proxies.
if kubectl get crd gateways.gateway.networking.k8s.io >/dev/null 2>&1; then
    for namespace in "$NAMESPACE_1" "$NAMESPACE_2"; do
        kubectl -n "$namespace" delete gateways.gateway.networking.k8s.io --all --ignore-not-found --wait --timeout=60s || echo "Failed to delete Gateways in '$namespace', ignoring."
    done
fi
"$DIR/integration-cleanup-envoy.sh" "$NAMESPACE_1" "$NAMESPACE_2" || echo "Failed to delete Envoy Gateway leftovers, ignoring this failure as integration-cleanup.sh retries it."

kubectl delete namespace "$NAMESPACE_1" "$NAMESPACE_2" --wait=false
