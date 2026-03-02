#!/usr/bin/env bash

set -euo pipefail

# Find git repository root to ensure paths work regardless of where script is executed
GIT_ROOT="$(git rev-parse --show-toplevel)"

OUTPUT_DIR="${1:-.}"
COMPOSE_FILE_RELATIVE="deploy/dockerephemeral/docker-compose.yaml"
COMPOSE_FILE="$GIT_ROOT/$COMPOSE_FILE_RELATIVE"

# Canonicalize image names to include explicit docker.io or existing registry
# prefix. Docker's default registry is docker.io, but it's often omitted in
# image references.
canonicalize_image_name() {
  local img="$1"

  if [[ "$img" != *"/"* ]]; then
    # No slash means it's an official Docker Hub image (e.g., nginx:latest)
    echo "docker.io/library/$img"
  elif [[ "${img%%/*}" != *"."* ]] && [[ "${img%%/*}" != *":"* ]] && [[ "${img%%/*}" != "localhost" ]]; then
    # Has slash but first part has no dot/colon and isn't localhost
    # This is a Docker Hub user/org image (e.g., mesosphere/aws-cli:1.14.5)
    echo "docker.io/$img"
  else
    # Already has a registry (e.g., quay.io/wire/brig:latest)
    echo "$img"
  fi
}

mkdir -p "$OUTPUT_DIR"

# Get current git commit hash for linking to source
GIT_COMMIT=$(git rev-parse HEAD)
COMPOSE_FILE_URL="https://github.com/wireapp/wire-server/blob/${GIT_COMMIT}/${COMPOSE_FILE_RELATIVE}"

docker compose -f "$COMPOSE_FILE" config --images \
  | while read -r img; do
      canonical_img=$(canonicalize_image_name "$img")
      safe_name=$(echo "$canonical_img" | tr '/:' '-')
      filename="$OUTPUT_DIR/sbom-${safe_name}.cyclonedx.json"
      temp_filename="${filename}.tmp"

      echo "Creating SBOM for $img -> $canonical_img: $filename"

      # Generate SBOM with syft
      SYFT_FORMAT_PRETTY=true \
      SYFT_ENRICH=all \
      SYFT_NIX_CAPTURE_OWNED_FILES=true \
      SYFT_SCOPE="all-layers" \
        syft -v "docker:$canonical_img" -o cyclonedx-json  > "$temp_filename"

      # Parse registry and image path for OCI purl
      # Format: pkg:oci/namespace/name@version?repository_url=registry
      registry="${canonical_img%%/*}"  # Extract registry (e.g., docker.io)
      image_path="${canonical_img#*/}" # Remove registry (e.g., mesosphere/aws-cli:1.14.5)
      image_name="${image_path%:*}"    # Remove tag (e.g., mesosphere/aws-cli)
      image_tag="${image_path##*:}"    # Extract tag (e.g., 1.14.5)

      purl="pkg:oci/${image_name}@${image_tag}?repository_url=${registry}"

      # Add purl, scope tag, and docker-compose source reference using jq
      jq --arg purl "$purl" \
         --arg compose_url "$COMPOSE_FILE_URL" \
         '.metadata.component.purl = $purl |
          .metadata.component.properties += [{"name": "scope", "value": "test"}] |
          .metadata.component.externalReferences += [{"type": "build-meta", "url": $compose_url, "comment": "Source docker-compose manifest"}]' \
         "$temp_filename" > "$filename"
      rm "$temp_filename"
    done
