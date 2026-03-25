#!/usr/bin/env bash

set -euo pipefail

# Script to upload all generated SBOMs to Dependency Track
#
# The Dependency Track API is described here (openapi/swagger):
# https://editor.swagger.io/?url=https://deptrack.wire.link/api/openapi.json

PROJECT_NAME="${1:-}"
VERSION="${2:-}"

if [[ -z "$PROJECT_NAME" ]] || [[ -z "$VERSION" ]]; then
  echo "Usage: $0 <project-name> <version>" >&2
  echo "  project-name: Dependency Track project name" >&2
  echo "  version: Project version" >&2
  exit 1
fi

if [[ -z "${DEPENDENCY_TRACK_API_KEY:-}" ]]; then
  echo "ERROR: DEPENDENCY_TRACK_API_KEY environment variable not set" >&2
  exit 1
fi

# Find git repository root and script directory
GIT_ROOT="$(git rev-parse --show-toplevel)"
SBOMS_DIR="$GIT_ROOT/tmp/sboms"

SCRIPT_DIR="$(dirname "$0")"

echo "Uploading Helm SBOMs..."
find "$SBOMS_DIR/helm" -name '*.json' -type f -not -path '*/.oci-cache/*' 2>/dev/null | while read -r sbom; do
  chart_purl=$(jq -r '.metadata.component.externalReferences[] | select(.comment == "Helm chart") | .url' "$sbom")
  chart_name=$(echo "$chart_purl" | sed 's|pkg:helm/||' | cut -d'@' -f1)
  "$SCRIPT_DIR/upload-sbom.sh" "$sbom" "Helm charts" "$PROJECT_NAME" "$VERSION" "$chart_name" || exit 1
done

echo "Uploading Helmfile SBOMs..."
find "$SBOMS_DIR/helmfile" -name '*.json' -type f -not -path '*/.oci-cache/*' 2>/dev/null | while read -r sbom; do
  "$SCRIPT_DIR/upload-sbom.sh" "$sbom" helmfile "$PROJECT_NAME" "$VERSION" || exit 1
done

echo "Uploading Docker Compose SBOMs..."
find "$SBOMS_DIR/docker-compose" -name '*.json' -type f -not -path '*/.oci-cache/*' 2>/dev/null | while read -r sbom; do
  "$SCRIPT_DIR/upload-sbom.sh" "$sbom" docker-compose "$PROJECT_NAME" "$VERSION" || exit 1
done

echo "✓ All SBOMs uploaded successfully"
