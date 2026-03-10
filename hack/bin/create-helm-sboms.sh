#!/usr/bin/env bash

set -euo pipefail

# Find git repository root to ensure paths work regardless of where script is executed
GIT_ROOT="$(git rev-parse --show-toplevel)"

OUTPUT_DIR="${1:-.}"
VERSION_OVERRIDE="${2:-}"
CHARTS_DIR="$GIT_ROOT/charts"

if [[ -z "$VERSION_OVERRIDE" ]]; then
  echo "Usage: $0 <output-dir> <version>"
  echo "  output-dir: Directory to write SBOM files"
  echo "  version: Version to use for images (e.g., 5.28.9)"
  exit 1
fi

# Check if Docker is available and running
if ! docker info >/dev/null 2>&1; then
  echo "ERROR: Docker is not running or not accessible." >&2
  echo "Please start Docker and try again." >&2
  exit 1
fi

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

# Extract images from a Helm chart using helm template
# This properly resolves images from subcharts and dependencies
extract_images_from_chart() {
  local chart_path="$1"
  local chart_name="$2"

  # First, try to build dependencies if requirements.yaml or Chart.yaml has dependencies
  if [[ -f "$chart_path/requirements.yaml" ]] || grep -q "^dependencies:" "$chart_path/Chart.yaml" 2>/dev/null; then
    echo "  Building dependencies for $chart_name..." >&2
    (cd "$chart_path" && helm dependency build > /dev/null 2>&1) || true
  fi

  # Template the chart and extract image references
  # We use a dummy release name and set a global placeholder to be more lenient
  # (we don't want to check the Helm chart, only extract its images)
  local output
  output=$(helm template test-release "$chart_path" --set-string 'global.placeholder=placeholder' 2>/dev/null) || true

  # Extract image values from the output using yq (jq wrapper)
  # Recursively find all .image fields in objects and output unique values
  echo "$output" | yq -r '.. | objects | .image? // empty' - 2>/dev/null | sort -u || true
}

# Generate SBOM for an image using syft
generate_sbom() {
  local canonical_img="$1"
  local temp_filename="$2"

  SYFT_FORMAT_PRETTY=true \
  SYFT_ENRICH=all \
  SYFT_NIX_CAPTURE_OWNED_FILES=true \
  SYFT_SCOPE="all-layers" \
    syft -v "docker:$canonical_img" -o cyclonedx-json > "$temp_filename"
}

mkdir -p "$OUTPUT_DIR"

# Get current git commit hash for linking to source
GIT_COMMIT=$(git rev-parse HEAD)

# Track errors during processing
error_count=0

# Loop through each chart directory
for chart_dir in "$CHARTS_DIR"/*/; do
  chart_name=$(basename "$chart_dir")

  # Skip if not a directory or doesn't have Chart.yaml
  if [[ ! -f "$chart_dir/Chart.yaml" ]]; then
    continue
  fi

  # Skip charts that will be deleted or have issues
  if [[ "$chart_name" == "metallb" ]] || [[ "$chart_name" == "mlsstats" ]]; then
    echo "Skipping chart: $chart_name (excluded)"
    continue
  fi

  echo "Processing chart: $chart_name"

  # Get chart version from Chart.yaml
  chart_version=$(yq -r '.version // "unknown"' "$chart_dir/Chart.yaml")

  # Extract images using helm template
  images=$(extract_images_from_chart "$chart_dir" "$chart_name")

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
    filename="$OUTPUT_DIR/sbom-helm-${chart_name}-${safe_name}.cyclonedx.json"
    temp_filename="${filename}.tmp"

    echo "  Creating SBOM for $img -> $canonical_img: $filename"

    # Check manifest version with skopeo to determine if conversion is needed
    manifest_info=$(skopeo inspect --raw "docker://$canonical_img" 2>/dev/null || echo "")

    if echo "$manifest_info" | grep -q '"schemaVersion":\s*1'; then
      # Old schema 1 format - need to convert with skopeo
      echo "  Detected schema 1 manifest, converting to OCI format with skopeo..." >&2

      oci_dir="$OUTPUT_DIR/.oci-cache/$(echo "$canonical_img" | tr '/:' '-')"
      mkdir -p "$oci_dir"

      if ! skopeo copy --insecure-policy "docker://$canonical_img" "oci:$oci_dir"; then
        echo "  ERROR: Failed to convert image $canonical_img with skopeo" >&2
        rm -rf "$oci_dir"
        ((error_count++))
        continue
      fi

      # Scan the OCI format image
      if ! SYFT_FORMAT_PRETTY=true \
           SYFT_ENRICH=all \
           SYFT_NIX_CAPTURE_OWNED_FILES=true \
           SYFT_SCOPE="all-layers" \
             syft -v "oci-dir:$oci_dir" -o cyclonedx-json > "$temp_filename"; then
        echo "  ERROR: Failed to scan OCI image for $canonical_img" >&2
        rm -rf "$oci_dir"
        rm -f "$temp_filename"
        ((error_count++))
        continue
      fi

      # Clean up OCI cache
      rm -rf "$oci_dir"
    else
      # Modern format - scan directly with syft
      if ! generate_sbom "$canonical_img" "$temp_filename"; then
        echo "  ERROR: Failed to generate SBOM for $canonical_img" >&2
        rm -f "$temp_filename"
        ((error_count++))
        continue
      fi
    fi

    # Validate that we have a valid JSON file
    if [[ ! -s "$temp_filename" ]]; then
      echo "  ERROR: Empty SBOM output for $canonical_img" >&2
      rm -f "$temp_filename"
      ((error_count++))
      continue
    fi

    if ! jq empty "$temp_filename" 2>/dev/null; then
      echo "  ERROR: Invalid JSON in SBOM output for $canonical_img:" >&2
      head -5 "$temp_filename" >&2
      rm -f "$temp_filename"
      ((error_count++))
      continue
    fi

    # Parse registry and image path for OCI purl
    # Format: pkg:oci/namespace/name@version?repository_url=registry
    registry="${canonical_img%%/*}"  # Extract registry (e.g., quay.io)
    image_path="${canonical_img#*/}" # Remove registry (e.g., wire/brig:do-not-use)
    image_name="${image_path%:*}"    # Remove tag (e.g., wire/brig)
    image_tag="${image_path##*:}"    # Extract tag (e.g., do-not-use)

    oci_purl="pkg:oci/${image_name}@${image_tag}?repository_url=${registry}"

    # Create helm purl
    # Format: pkg:helm/name@version
    helm_purl="pkg:helm/${chart_name}@${chart_version}"

    # Relative path for GitHub URL
    chart_relative_path="charts/$chart_name"
    chart_url="https://github.com/wireapp/wire-server/tree/${GIT_COMMIT}/${chart_relative_path}"

    # Set helm purl as component purl and add references in externalReferences
    jq --arg helm_purl "$helm_purl" \
       --arg oci_purl "$oci_purl" \
       --arg chart_url "$chart_url" \
       '.metadata.component.purl = $helm_purl |
        .metadata.component.externalReferences += [
          {"type": "distribution", "url": $helm_purl, "comment": "Helm chart"},
          {"type": "build-meta", "url": $chart_url, "comment": "Source Helm chart"}
        ]' \
       "$temp_filename" > "$filename"
    rm "$temp_filename"
  done <<< "$images"
done

echo "SBOM generation complete. Output directory: $OUTPUT_DIR"

if [[ $error_count -gt 0 ]]; then
  echo "WARNING: $error_count error(s) occurred during SBOM generation" >&2
  exit 1
fi
