#!/usr/bin/env bash

# Common functions and utilities for SBOM generation scripts

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
  return 0
}

# Generate OCI purl from canonical image name
# Args: canonical_img
# Output: OCI purl string
generate_oci_purl() {
  local canonical_img="$1"

  local registry="${canonical_img%%/*}"  # Extract registry (e.g., quay.io)
  local image_path="${canonical_img#*/}" # Remove registry (e.g., wire/brig:5.28.15)
  local image_name="${image_path%:*}"    # Remove tag (e.g., wire/brig)
  local image_tag="${image_path##*:}"    # Extract tag (e.g., 5.28.15)

  echo "pkg:oci/${image_name}@${image_tag}?repository_url=${registry}"
  return 0
}

# Scan image with syft, handling schema 1 manifests if needed
# Args: canonical_img, temp_filename, output_dir
# Returns: 0 on success, 1 on failure
scan_image_with_syft() {
  local canonical_img="$1"
  local temp_filename="$2"
  local output_dir="$3"

  # Locate run-syft.sh in the same directory as this script
  local run_syft
  run_syft="$(dirname "${BASH_SOURCE[0]}")/run-syft.sh"

  # Check manifest version with skopeo to determine if conversion is needed
  local manifest_info
  manifest_info=$(skopeo inspect --raw "docker://$canonical_img" 2>/dev/null || echo "")

  if echo "$manifest_info" | grep -q '"schemaVersion":\s*1'; then
    # Old schema 1 format - need to convert with skopeo
    echo "  Detected schema 1 manifest, converting to OCI format with skopeo..." >&2

    local oci_dir
    oci_dir="$output_dir/.oci-cache/$(echo "$canonical_img" | tr '/:' '-')"
    mkdir -p "$oci_dir"

    if ! skopeo copy --insecure-policy "docker://$canonical_img" "oci:$oci_dir"; then
      echo "  ERROR: Failed to convert image $canonical_img with skopeo" >&2
      rm -rf "$oci_dir"
      return 1
    fi

    # Scan the OCI format image
    if ! "$run_syft" "oci-dir:$oci_dir" "$temp_filename"; then
      echo "  ERROR: Failed to scan OCI image for $canonical_img" >&2
      rm -rf "$oci_dir"
      rm -f "$temp_filename"
      return 1
    fi
  else
    # Modern format - scan directly with syft
    if ! "$run_syft" "registry:$canonical_img" "$temp_filename"; then
      echo "  ERROR: Failed to generate SBOM for $canonical_img" >&2
      rm -f "$temp_filename"
      return 1
    fi
  fi

  # Validate the generated SBOM
  _validate_sbom_json "$temp_filename" "$canonical_img"
}

# Validate SBOM JSON file (internal helper)
# Args: temp_filename, canonical_img
# Returns: 0 if valid, 1 if invalid
_validate_sbom_json() {
  local temp_filename="$1"
  local canonical_img="$2"

  if [[ ! -s "$temp_filename" ]]; then
    echo "  ERROR: Empty SBOM output for $canonical_img" >&2
    rm -f "$temp_filename"
    return 1
  fi

  if ! jq empty "$temp_filename" 2>/dev/null; then
    echo "  ERROR: Invalid JSON in SBOM output for $canonical_img:" >&2
    head -5 "$temp_filename" >&2
    rm -f "$temp_filename"
    return 1
  fi

  return 0
}
