#!/usr/bin/env bash

set -x

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
"${DIR}"/integration-teardown-ingress-classes.sh

IFS=$'\n'
for NAMESPACE in $(kubectl get namespaces | grep "^test-" | awk '{print $1}'); do
    echo "$NAMESPACE"
    kubectl delete namespace "$NAMESPACE" &
done
