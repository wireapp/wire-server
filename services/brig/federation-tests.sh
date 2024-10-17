#!/usr/bin/env bash

USAGE="$0 <NAMESPACE>"
NAMESPACE=${1:?$USAGE}

set -e

command -v telepresence >/dev/null 2>&1 || {
    echo >&2 "telepresence is not installed, aborting."
    exit 1
}

# This script assumes:
# * two wire-server backends under NAMEPACE and NAMESPACE-fed2 have been deployed with helm.
# * you have a locally compiled brig-integration executable
#
# It then downloads the configmaps, performs a hacky override for two configuration flags,
# and then uses telepresence to run a locally-compiled brig-integration executable against
# the brigs and federators inside kubernetes in the two NAMESPACES.

cd "$(dirname "${BASH_SOURCE[0]}")"

kubectl -n "$NAMESPACE" get configmap brig-integration -o jsonpath='{.data.integration\.yaml}' >i.yaml
kubectl -n "$NAMESPACE" get configmap brig -o jsonpath='{.data.brig\.yaml}' >b.yaml

# override some settings so the local brig-integration executable doesn't fail
sed -i "s=privateKeys: /etc/wire/brig/secrets/secretkey.txt=privateKeys: test/resources/zauth/privkeys.txt=g" b.yaml
sed -i "s=publicKeys: /etc/wire/brig/secrets/publickey.txt=publicKeys: test/resources/zauth/pubkeys.txt=g" b.yaml

# We need to pass --also-proxy to cannon pod IPs, as for some reason (maybe due
# to calico) the pod IPs in some clusters are not within the podCIDR range
# defined on the nodes and cannons need to be accessed directly (without using
# the kubernetes services)
declare -a alsoProxyOptions
while read -r ip; do
  alsoProxyOptions+=("--also-proxy=${ip}")
done < <(kubectl get pods -n "$NAMESPACE" -l app=cannon -o json | jq -r '.items[].status.podIPs[].ip')

AWS_ACCESS_KEY_ID="$(kubectl get secret -n "$NAMESPACE" brig -o json | jq -r '.data | map_values(@base64d) | .awsKeyId')"
export AWS_ACCESS_KEY_ID
AWS_SECRET_ACCESS_KEY="$(kubectl get secret -n "$NAMESPACE" brig -o json | jq -r '.data | map_values(@base64d) | .awsSecretKey')"
export AWS_SECRET_ACCESS_KEY
AWS_REGION="$(kubectl get deployment -n "$NAMESPACE" brig -o json | jq -r '.spec.template.spec.containers | map(.env | map(select(.name == "AWS_REGION").value))[0][0]')"
export AWS_REGION

# shellcheck disable=SC2086
telepresence --namespace "$NAMESPACE" --also-proxy=cassandra-ephemeral "${alsoProxyOptions[@]}" --run bash -c "./dist/brig-integration -p federation-end2end-user -i i.yaml -s b.yaml"
