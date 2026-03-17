#!/usr/bin/env bash

set -euo pipefail

# Find git repository root to ensure paths work regardless of where script is executed
GIT_ROOT="$(git rev-parse --show-toplevel)"

# Source common SBOM functions
# shellcheck source=hack/bin/sbom-common.sh
source "$(dirname "$0")/sbom-common.sh"

OUTPUT_DIR="${1:-.}"
VERSION_OVERRIDE="${2:-}"
HELMFILE_PATH="hack/helmfile.yaml.gotmpl"

if [[ -z "$VERSION_OVERRIDE" ]]; then
  echo "Usage: $0 <output-dir> <version>"
  echo "  output-dir: Directory to write SBOM files"
  echo "  version: Version to use for wire-server images (e.g., 5.28.9)"
  exit 1
fi

# Check if Docker is available and running
check_docker_running

mkdir -p "$OUTPUT_DIR"

# Get current git commit hash for linking to source
GIT_COMMIT=$(git rev-parse HEAD)

helmfile_url="https://github.com/wireapp/wire-server/blob/${GIT_COMMIT}/${HELMFILE_PATH}"

echo "Processing helmfile: $HELMFILE_PATH"

# Extract all images from helmfile template output
# We use helmfile template to render all releases and extract images
cd "$GIT_ROOT"

# Set dummy environment variables required by helmfile
export NAMESPACE_1="${NAMESPACE_1:-dummy-namespace-1}"
export NAMESPACE_2="${NAMESPACE_2:-dummy-namespace-2}"
export FEDERATION_DOMAIN_1="${FEDERATION_DOMAIN_1:-fed1.example.com}"
export FEDERATION_DOMAIN_2="${FEDERATION_DOMAIN_2:-fed2.example.com}"
export FEDERATION_DOMAIN_BASE_1="${FEDERATION_DOMAIN_BASE_1:-base1.example.com}"
export FEDERATION_DOMAIN_BASE_2="${FEDERATION_DOMAIN_BASE_2:-base2.example.com}"
export FEDERATION_CA_CERTIFICATE="${FEDERATION_CA_CERTIFICATE:-dummy-cert}"
export ENTERPRISE_IMAGE_PULL_SECRET="${ENTERPRISE_IMAGE_PULL_SECRET:-dummy-secret}"

# Extract images from helmfile
images=$(helmfile -f "$HELMFILE_PATH" template 2>/dev/null | yq -r '.. | objects | .image? // empty' - 2>/dev/null | sort -u || true)

if [[ -z "$images" ]]; then
  echo "No images found in helmfile" >&2
  exit 1
fi

# Track errors during processing
error_count=0

# Process each unique image
while IFS= read -r img; do
  # Skip empty lines
  [[ -z "$img" ]] && continue

  # Replace placeholder tags (do-not-use) with user-provided version
  if [[ "$img" == *":do-not-use" ]] || [[ "$img" == *":0.0.42" ]]; then
    img="${img%:*}:${VERSION_OVERRIDE}"
  fi

  canonical_img=$(canonicalize_image_name "$img")
  safe_name=$(echo "$canonical_img" | tr '/:' '-')
  filename="$OUTPUT_DIR/sbom-helmfile-${safe_name}.cyclonedx.json"
  temp_filename="${filename}.tmp"

  echo "  Creating SBOM for $img -> $canonical_img: $filename"

  # Scan image with syft (handles schema 1 conversion and validation)
  if ! scan_image_with_syft "$canonical_img" "$temp_filename" "$OUTPUT_DIR"; then
    ((error_count++))
    continue
  fi

  # Generate OCI purl
  oci_purl=$(generate_oci_purl "$canonical_img")

  # Create helmfile reference purl for context
  helmfile_purl="pkg:helm/helmfile@${VERSION_OVERRIDE}"

    # Add fields that are not provided by syft
  jq --arg oci_purl "$oci_purl" \
     --arg helmfile_purl "$helmfile_purl" \
     --arg helmfile_url "$helmfile_url" \
     '.metadata.component.name = $oci_purl |
      .metadata.component.purl = $oci_purl |
      .metadata.component.externalReferences += [
        {"type": "distribution", "url": $helmfile_purl, "comment": "Helmfile deployment"},
        {"type": "build-meta", "url": $helmfile_url, "comment": "Source Helmfile"}
      ]' \
     "$temp_filename" > "$filename"
  rm "$temp_filename"
done <<< "$images"

echo "SBOM generation complete. Output directory: $OUTPUT_DIR"

if [[ $error_count -gt 0 ]]; then
  echo "WARNING: $error_count error(s) occurred during SBOM generation" >&2
  exit 1
fi
