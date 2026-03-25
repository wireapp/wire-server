#!/usr/bin/env bash

# Script to delete any integration namespaces prefixed with `test-` older than 2 hours.
# Also deletes leftover nginx ingress classes.
#
# Motivation: cleanup of old test clusters that were not deleted (e.g. by the CI system, because it broke)

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

NOW=$(date +%s)
namespaces=$(kubectl get namespaces -o json | jq -r --argjson now "$NOW" '
  .items[]
  | .metadata as $meta
  | $meta.name as $name
  | select($name | startswith("test-"))
  | select($name | contains("-fed2") | not)
  | ($meta.creationTimestamp | fromdateiso8601) as $created
  | select(($now - $created) > (2 * 60 * 60))
  | $name
')

if [[ -z "$namespaces" ]]; then
    echo "Nothing to clean up."
else
    while read -r namespace; do
        echo "Test namespace '$namespace' older than 2 hours; tearing down..."
        if ! NAMESPACE="$namespace" "${DIR}/integration-teardown-federation.sh"; then
            echo "Failed to tear down namespace '$namespace'; continuing..."
        fi
    done <<<"$namespaces"
fi

"${DIR}/integration-teardown-ingress-classes.sh"
