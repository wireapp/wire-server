#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."

NAMESPACE=${NAMESPACE:-test-integration}

set -ex

echo "NAMESPACE = $NAMESPACE"

helm ls --all --namespace ${NAMESPACE} | grep "test-" | awk '{print $1}' | xargs -n 1 helm -n "$NAMESPACE" delete

sleep 10

kubectl delete namespace ${NAMESPACE}
