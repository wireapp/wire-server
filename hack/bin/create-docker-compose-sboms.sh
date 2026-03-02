#!/usr/bin/env bash

set -euo pipefail

# Find git repository root to ensure paths work regardless of where script is executed
GIT_ROOT="$(git rev-parse --show-toplevel)"

# Source common SBOM functions
# shellcheck source=hack/bin/sbom-common.sh
source "$(dirname "$0")/sbom-common.sh"

OUTPUT_DIR="${1:-.}"
COMPOSE_FILE_RELATIVE="deploy/dockerephemeral/docker-compose.yaml"
COMPOSE_FILE="$GIT_ROOT/$COMPOSE_FILE_RELATIVE"

# Check if Docker is available and running
check_docker_running

mkdir -p "$OUTPUT_DIR"

# Get current git commit hash for linking to source
GIT_COMMIT=$(git rev-parse HEAD)
COMPOSE_FILE_URL="https://github.com/wireapp/wire-server/blob/${GIT_COMMIT}/${COMPOSE_FILE_RELATIVE}"

# Track errors during processing
error_count=0

docker compose -f "$COMPOSE_FILE" config --images \
  | while read -r img; do
      canonical_img=$(canonicalize_image_name "$img")
      safe_name=$(echo "$canonical_img" | tr '/:' '-')
      filename="$OUTPUT_DIR/sbom-${safe_name}.cyclonedx.json"
      temp_filename="${filename}.tmp"

      echo "  Creating SBOM for $img -> $canonical_img: $filename"

      if ! scan_image_with_syft "$canonical_img" "$temp_filename" "$OUTPUT_DIR"; then
        ((error_count++))
        continue
      fi

      purl=$(generate_oci_purl "$canonical_img")

      # Add fields that are not provided by syft
      jq --arg purl "$purl" \
         --arg compose_url "$COMPOSE_FILE_URL" \
         '.metadata.component.name = $purl |
          .metadata.component.purl = $purl |
          .metadata.component.properties += [{"name": "scope", "value": "test"}] |
          .metadata.component.externalReferences += [{"type": "build-meta", "url": $compose_url, "comment": "Source docker-compose manifest"}]' \
         "$temp_filename" > "$filename"
      rm "$temp_filename"
    done

echo "SBOM generation complete. Output directory: $OUTPUT_DIR"

if [[ $error_count -gt 0 ]]; then
  echo "WARNING: $error_count error(s) occurred during SBOM generation" >&2
  exit 1
fi
