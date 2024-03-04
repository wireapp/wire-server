#!/usr/bin/env bash

# Script to delete any helm releases prefixed with `test-` older than 20 minutes deemed inactive
#
# Motivation: cleanup of old test clusters that were not deleted (e.g. by the CI system, because it broke)

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

releases=$(helm list -A -f '^test-' -o json |
    jq -r -f "$DIR/filter-old-releases.jq")

if [ -n "$releases" ]; then
    while read -r line; do
        name=$(awk '{print $1}' <<<"$line")
        namespace=$(awk '{print $2}' <<<"$line")
        echo "test release '$name' older than 2 hours; deleting..."
        helm delete -n "$namespace" "$name"
    done <<<"$releases"
else
    echo "Nothing to clean up."
fi
