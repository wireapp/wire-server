#!/usr/bin/env bash

USAGE="$0 <NAMESPACE>"
NAMESPACE=${1:?$USAGE}

kubectl -n "$NAMESPACE" get configmap brig-integration -o jsonpath='{.data.integration\.yaml}' > i.yaml
kubectl -n "$NAMESPACE" get configmap brig -o jsonpath='{.data.brig\.yaml}' > b.yaml

# override some settings so the local brig-integration executable doesn't fail
sed -i "s=privateKeys: /etc/wire/brig/secrets/secretkey.txt=privateKeys: test/resources/zauth/privkeys.txt=g" b.yaml
sed -i "s=publicKeys: /etc/wire/brig/secrets/publickey.txt=publicKeys: test/resources/zauth/pubkeys.txt=g" b.yaml

telepresence --namespace "$NAMESPACE" --also-proxy cassandra-ephemeral --run bash -c "./dist/brig-integration -p brig-federation -i i.yaml -s b.yaml"
