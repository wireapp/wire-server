#!/usr/bin/env bash

set -euo pipefail

# Find git repository root to ensure paths work regardless of where script is executed
GIT_ROOT="$(git rev-parse --show-toplevel)"

# Source common SBOM functions
# shellcheck source=hack/bin/sbom-common.sh
source "$(dirname "$0")/sbom-common.sh"

OUTPUT_DIR="${1:-.}"
VERSION_OVERRIDE="${2:-}"
CHARTS_DIR="$GIT_ROOT/.local/charts"

if [[ -z "$VERSION_OVERRIDE" ]]; then
  echo "Usage: $0 <output-dir> <version>"
  echo "  output-dir: Directory to write SBOM files"
  echo "  version: Version to use for packaged charts (e.g., 5.28.22)"
  exit 1
fi

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
  return 0
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

  # `mlsstats`'s image is not publically available
  if [[ "$chart_name" == "mlsstats" ]]; then
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

    canonical_img=$(canonicalize_image_name "$img")
    safe_name=$(echo "$canonical_img" | tr '/:' '-')
    filename="$OUTPUT_DIR/sbom-helm-${chart_name}-${safe_name}.cyclonedx.json"
    temp_filename="${filename}.tmp"

    echo "  Creating SBOM for $img -> $canonical_img: $filename"

    # Scan image with syft (handles schema 1 conversion and validation)
    if ! scan_image_with_syft "$canonical_img" "$temp_filename" "$OUTPUT_DIR"; then
      ((error_count++))
      continue
    fi

    oci_purl=$(generate_oci_purl "$canonical_img")

    # Create helm purl
    # Format: pkg:helm/name@version
    helm_purl="pkg:helm/${chart_name}@${chart_version}"

    # Relative path for GitHub URL
    chart_relative_path="charts/$chart_name"
    chart_url="https://github.com/wireapp/wire-server/tree/${GIT_COMMIT}/${chart_relative_path}"

    # Add fields that are not provided by syft
    jq --arg oci_purl "$oci_purl" \
       --arg helm_purl "$helm_purl" \
       --arg chart_url "$chart_url" \
       '.metadata.component.name = $oci_purl |
        .metadata.component.purl = $oci_purl |
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
