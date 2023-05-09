#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."

NAMESPACE=${NAMESPACE:-test-integration}
# doesn't matter for destruction but needs to be set
export FEDERATION_DOMAIN="."
KUBERNETES_VERSION_MAJOR="$(kubectl version -o json | jq -r .serverVersion.major)"
KUBERNETES_VERSION_MINOR="$(kubectl version -o json | jq -r .serverVersion.minor)"
KUBERNETES_VERSION_MINOR="${KUBERNETES_VERSION_MINOR//[!0-9]/}" # some clusters report minor versions as a string like '27+'. Strip any non-digit characters.
if (( KUBERNETES_VERSION_MAJOR > 1 || KUBERNETES_VERSION_MAJOR == 1 && KUBERNETES_VERSION_MINOR >= 23 )); then
    export INGRESS_CHART="ingress-nginx-controller"
else
    export INGRESS_CHART="nginx-ingress-controller"
fi

set -ex

. "$DIR/helm_overrides.sh"
helmfile --file "${TOP_LEVEL}/hack/helmfile-single.yaml" destroy
