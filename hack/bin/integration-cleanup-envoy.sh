#!/usr/bin/env bash
#
# Delete integration test leftovers in the Envoy Gateway controller namespace.
#
# In envoy mode a test deployment owns objects outside its own namespaces, which
# deleting the test namespaces does not remove:
# - the proxy Deployment, Service, ServiceAccount, ... that Envoy Gateway creates
#   per Gateway (label gateway.envoyproxy.io/owning-gateway-namespace)
# - the <namespace>-fed and dynamic-backend-<n>-<namespace> Services from the
#   wire-ingress and integration charts (Helm annotation
#   meta.helm.sh/release-namespace)
#
# Usage:
#   integration-cleanup-envoy.sh NAMESPACE...  delete leftovers owned by these namespaces
#   integration-cleanup-envoy.sh               delete leftovers of test-* namespaces that no longer exist
#
# Set DRY_RUN=1 to only print what would be deleted.

set -euo pipefail

ENVOY_GATEWAY_NAMESPACE=${ENVOY_GATEWAY_NAMESPACE:-envoy-gateway-system}
OWNER_LABEL=gateway.envoyproxy.io/owning-gateway-namespace
KINDS=deployments,services,serviceaccounts,configmaps,horizontalpodautoscalers,poddisruptionbudgets

dry_run=()
if [[ "${DRY_RUN:-}" == "1" ]]; then
    dry_run=(--dry-run=server)
fi

if ! kubectl get namespace "$ENVOY_GATEWAY_NAMESPACE" >/dev/null 2>&1; then
    echo "Namespace '$ENVOY_GATEWAY_NAMESPACE' does not exist; nothing to clean up."
    exit 0
fi

kubectl_eg() {
    kubectl -n "$ENVOY_GATEWAY_NAMESPACE" "$@"
}

# Namespaces that own objects in $ENVOY_GATEWAY_NAMESPACE
owners() {
    {
        kubectl_eg get "$KINDS" -l "$OWNER_LABEL" -o json |
            jq -r --arg label "$OWNER_LABEL" '.items[].metadata.labels[$label]'
        kubectl_eg get services -o json |
            jq -r '.items[].metadata.annotations["meta.helm.sh/release-namespace"] // empty'
    } | sort -u
}

cleanup_namespace() {
    local namespace=$1
    echo "Deleting leftovers of namespace '$namespace' in '$ENVOY_GATEWAY_NAMESPACE'..."
    kubectl_eg delete "$KINDS" -l "$OWNER_LABEL=$namespace" --ignore-not-found --wait=false "${dry_run[@]}"
    kubectl_eg get services -o json |
        jq -r --arg ns "$namespace" '.items[] | select(.metadata.annotations["meta.helm.sh/release-namespace"] == $ns) | .metadata.name' |
        xargs -r kubectl -n "$ENVOY_GATEWAY_NAMESPACE" delete service --ignore-not-found --wait=false "${dry_run[@]}"
}

if (($# > 0)); then
    for namespace in "$@"; do
        cleanup_namespace "$namespace"
    done
    exit 0
fi

# Namespaces that still exist (including terminating ones) are skipped; their
# leftovers are deleted by a later run.
for namespace in $(owners | grep '^test-' || true); do
    if kubectl get namespace "$namespace" >/dev/null 2>&1; then
        continue
    fi
    cleanup_namespace "$namespace"
done
