#!/usr/bin/env bash

set -euo pipefail

SBOM_FILE="${1:-}"
PROJECT_NAME="${2:-sven-bom-test}"
PROJECT_VERSION="${3:-}"

if [[ -z "$SBOM_FILE" ]] || [[ -z "$PROJECT_VERSION" ]]; then
  echo "Usage: $0 <sbom-file> [project-name] <project-version>"
  echo "  sbom-file: Path to SBOM JSON file to upload"
  echo "  project-name: Dependency Track project name (default: sven-bom-test)"
  echo "  project-version: Project version (required)"
  echo ""
  echo "Environment variables:"
  echo "  DEPENDENCY_TRACK_API_KEY: API key for Dependency Track (required)"
  exit 1
fi

if [[ ! -f "$SBOM_FILE" ]]; then
  echo "ERROR: SBOM file not found: $SBOM_FILE" >&2
  exit 1
fi

if [[ -z "${DEPENDENCY_TRACK_API_KEY:-}" ]]; then
  echo "ERROR: DEPENDENCY_TRACK_API_KEY environment variable not set" >&2
  exit 1
fi

echo "Uploading SBOM: $SBOM_FILE"
echo "  Project: $PROJECT_NAME"
echo "  Version: $PROJECT_VERSION"

# Create temporary file for base64-encoded SBOM
tmpfile=$(mktemp)
trap "rm -f $tmpfile" EXIT

# Base64 encode the SBOM file
base64 -w 0 "$SBOM_FILE" > "$tmpfile"

# Build JSON payload and upload
jq -n \
  --rawfile bom "$tmpfile" \
  --arg projectName "$PROJECT_NAME" \
  --arg projectVersion "$PROJECT_VERSION" \
  '{bom: $bom, projectName: $projectName, projectVersion: $projectVersion, autoCreate: true}' \
  | curl -X PUT "https://deptrack.wire.link/api/v1/bom" \
    -H "X-Api-Key: $DEPENDENCY_TRACK_API_KEY" \
    -H "Content-Type: application/json" \
    --data-binary @-

echo ""
echo "Upload complete"
