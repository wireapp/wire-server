#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

set -ex

NAMESPACE=${NAMESPACE:-test-integration}
export NAMESPACE_1="$NAMESPACE"
export NAMESPACE_2="$NAMESPACE-fed2"
# these don't matter for destruction but have to be set.
export FEDERATION_DOMAIN_1="."
export FEDERATION_DOMAIN_2="."

KUBERNETES_VERSION_MAJOR="$(kubectl version -o json | jq -r .serverVersion.major)"
KUBERNETES_VERSION_MINOR="$(kubectl version -o json | jq -r .serverVersion.minor)"
KUBERNETES_VERSION_MINOR="${KUBERNETES_VERSION_MINOR//[!0-9]/}" # some clusters report minor versions as a string like '27+'. Strip any non-digit characters.
if (( KUBERNETES_VERSION_MAJOR > 1 || KUBERNETES_VERSION_MAJOR == 1 && KUBERNETES_VERSION_MINOR >= 23 )); then
    export INGRESS_CHART="ingress-nginx-controller"
else
    export INGRESS_CHART="nginx-ingress-controller"
fi

. "$DIR/helm_overrides.sh"
helmfile --file "${TOP_LEVEL}/hack/helmfile.yaml" destroy --skip-deps --skip-charts --concurrency 0

kubectl delete namespace "$NAMESPACE_1"
kubectl delete namespace "$NAMESPACE_2"
