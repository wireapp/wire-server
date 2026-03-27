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

if [[ -d "$SBOMS_DIR/helm" ]]; then
  echo "Uploading Helm SBOMs..."
  find "$SBOMS_DIR/helm" -name '*.json' -type f -not -path '*/.oci-cache/*' 2>/dev/null | while read -r sbom; do
    chart_purl=$(jq -r '.metadata.component.externalReferences[] | select(.comment == "Helm chart") | .url' "$sbom")
    chart_name=$(echo "$chart_purl" | sed 's|pkg:helm/||' | cut -d'@' -f1)
    "$SCRIPT_DIR/upload-sbom.sh" "$sbom" "Helm charts" "$PROJECT_NAME" "$VERSION" "$chart_name" || exit 1
  done
else
  echo "Skipping Helm SBOMs (directory not found)"
fi

if [[ -d "$SBOMS_DIR/helmfile" ]]; then
  echo "Uploading Helmfile SBOMs..."
  find "$SBOMS_DIR/helmfile" -name '*.json' -type f -not -path '*/.oci-cache/*' 2>/dev/null | while read -r sbom; do
    "$SCRIPT_DIR/upload-sbom.sh" "$sbom" helmfile "$PROJECT_NAME" "$VERSION" || exit 1
  done
else
  echo "Skipping Helmfile SBOMs (directory not found)"
fi

if [[ -d "$SBOMS_DIR/docker-compose" ]]; then
  echo "Uploading Docker Compose SBOMs..."
  find "$SBOMS_DIR/docker-compose" -name '*.json' -type f -not -path '*/.oci-cache/*' 2>/dev/null | while read -r sbom; do
    "$SCRIPT_DIR/upload-sbom.sh" "$sbom" docker-compose "$PROJECT_NAME" "$VERSION" || exit 1
  done
else
  echo "Skipping Docker Compose SBOMs (directory not found)"
fi

if [[ -d "$SBOMS_DIR/nix-docker-images/runtime" ]]; then
  echo "Uploading Nix Docker Images SBOMs (runtime dependencies)..."
  find "$SBOMS_DIR/nix-docker-images/runtime" -name '*.json' -type f 2>/dev/null | while read -r sbom; do
    # Extract image name from filename: sbom-nix-docker-{name}.{version}.cyclonedx.json
    image_name=$(basename "$sbom" | sed 's/sbom-nix-docker-//' | sed 's/\.[0-9].*//' | sed 's/\.cyclonedx\.json$//')
    # Add [runtime] to make project name unique
    project_prefix="$image_name [runtime]"
    "$SCRIPT_DIR/upload-sbom.sh" "$sbom" "nix-docker-runtime" "$PROJECT_NAME" "$VERSION" "$project_prefix" || exit 1
  done
else
  echo "Skipping Nix Docker Images SBOMs (runtime) (directory not found)"
fi

if [[ -d "$SBOMS_DIR/nix-docker-images/buildtime" ]]; then
  echo "Uploading Nix Docker Images SBOMs (buildtime dependencies)..."
  find "$SBOMS_DIR/nix-docker-images/buildtime" -name '*.json' -type f 2>/dev/null | while read -r sbom; do
    # Extract image name from filename: sbom-nix-docker-{name}.{version}.cyclonedx.json
    image_name=$(basename "$sbom" | sed 's/sbom-nix-docker-//' | sed 's/\.[0-9].*//' | sed 's/\.cyclonedx\.json$//')
    # Add [buildtime] to make project name unique
    project_prefix="$image_name [buildtime]"
    "$SCRIPT_DIR/upload-sbom.sh" "$sbom" "nix-docker-buildtime" "$PROJECT_NAME" "$VERSION" "$project_prefix" || exit 1
  done
else
  echo "Skipping Nix Docker Images SBOMs (buildtime) (directory not found)"
fi

if [[ -d "$SBOMS_DIR/nix-devshell/runtime" ]]; then
  echo "Uploading Nix devShell SBOMs (runtime dependencies)..."
  find "$SBOMS_DIR/nix-devshell/runtime" -name '*.json' -type f 2>/dev/null | while read -r sbom; do
    # Extract devShell name from filename: sbom-nix-devshell-{name}.{version}.cyclonedx.json
    devshell_name=$(basename "$sbom" | sed 's/sbom-nix-devshell-//' | sed 's/\.[0-9].*//' | sed 's/\.cyclonedx\.json$//')
    # Add [runtime] to make project name unique
    project_prefix="devshell-$devshell_name [runtime]"
    "$SCRIPT_DIR/upload-sbom.sh" "$sbom" "nix-devshell-runtime" "$PROJECT_NAME" "$VERSION" "$project_prefix" || exit 1
  done
else
  echo "Skipping Nix devShell SBOMs (runtime) (directory not found)"
fi

if [[ -d "$SBOMS_DIR/nix-devshell/buildtime" ]]; then
  echo "Uploading Nix devShell SBOMs (buildtime dependencies)..."
  find "$SBOMS_DIR/nix-devshell/buildtime" -name '*.json' -type f 2>/dev/null | while read -r sbom; do
    # Extract devShell name from filename: sbom-nix-devshell-{name}.{version}.cyclonedx.json
    devshell_name=$(basename "$sbom" | sed 's/sbom-nix-devshell-//' | sed 's/\.[0-9].*//' | sed 's/\.cyclonedx\.json$//')
    # Add [buildtime] to make project name unique
    project_prefix="devshell-$devshell_name [buildtime]"
    "$SCRIPT_DIR/upload-sbom.sh" "$sbom" "nix-devshell-buildtime" "$PROJECT_NAME" "$VERSION" "$project_prefix" || exit 1
  done
else
  echo "Skipping Nix devShell SBOMs (buildtime) (directory not found)"
fi

echo "✓ All SBOMs uploaded successfully"
