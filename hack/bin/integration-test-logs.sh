#!/usr/bin/env bash
set -euo pipefail

if [[ -z "$NAMESPACE" ]]; then
  echo "NAMESPACE not set"
  exit 1
fi

while IFS= read LINE; do
  if [[ "$LINE" =~ ^Pod\ (.*)\ running$ ]]; then
    kubectl -n "$NAMESPACE" logs "${BASH_REMATCH[1]}" -f
  fi
done
