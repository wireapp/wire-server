#!/usr/bin/env bash

set -euo pipefail

TOKEN="${1:-}"

if [[ -z "$TOKEN" ]]; then
  echo "Usage: $0 <token>" >&2
  echo "  token: BOM processing token from upload response" >&2
  echo "" >&2
  echo "Environment variables:" >&2
  echo "  DEPENDENCY_TRACK_API_KEY: API key for Dependency Track (required)" >&2
  exit 1
fi

if [[ -z "${DEPENDENCY_TRACK_API_KEY:-}" ]]; then
  echo "ERROR: DEPENDENCY_TRACK_API_KEY environment variable not set" >&2
  exit 1
fi

curl -s -X GET "https://deptrack.wire.link/api/v1/event/token/${TOKEN}" \
  -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY"
