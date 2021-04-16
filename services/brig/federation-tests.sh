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

telepresence --namespace "$NAMESPACE" --also-proxy cassandra-ephemeral --run bash -c "export INTEGRATION_FEDERATION_TESTS=1; ./dist/brig-integration -p brig-federation -i i.yaml -s b.yaml"
