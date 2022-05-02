#!/usr/bin/env bash

# shellcheck disable=SC2034
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."

set -x

IFS=$'\n'
for NAMESPACE in $(kubectl get namespaces | grep "^test-" | awk '{print $1}'); do

  echo "$NAMESPACE"
  kubectl delete namespace "$NAMESPACE"

done
