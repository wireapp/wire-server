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
  # TODO: This should be a more recent version
  echo "  version: Version to use for images (e.g., 2.125.0)"
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

  # Extract image values from the output
  # TODO: This should be done with yq or jq. A regex is too fragile.
  echo "$output" | grep -oP '^\s*image:\s*["\x27]?\K[^"\x27[:space:]]+(?=["\x27]?\s*$)' || true
}

mkdir -p "$OUTPUT_DIR"

# Get current git commit hash for linking to source
GIT_COMMIT=$(git rev-parse HEAD)

# Track images we've already processed to avoid duplicates
# TODO: We still want to have a relation of Helm chart to Image (Container). Thus, even if an image shows up in multiple Helm charts, we want multiple result files with metadata referring to the originating chart.
declare -A processed_images

# Loop through each chart directory
for chart_dir in "$CHARTS_DIR"/*/; do
  chart_name=$(basename "$chart_dir")

  # Skip if not a directory or doesn't have Chart.yaml
  if [[ ! -f "$chart_dir/Chart.yaml" ]]; then
    continue
  fi

  echo "Processing chart: $chart_name"

  # Get chart version from Chart.yaml
  chart_version=$(yq -r '.version // "unknown"' "$chart_dir/Chart.yaml")

  # Relative path for GitHub URL
  # TODO: Move this close its first usage
  chart_relative_path="charts/$chart_name"
  chart_url="https://github.com/wireapp/wire-server/tree/${GIT_COMMIT}/${chart_relative_path}"

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

    # Skip if we've already processed this image
    [[ -n "${processed_images[$img]:-}" ]] && continue
    processed_images[$img]=1

    canonical_img=$(canonicalize_image_name "$img")
    safe_name=$(echo "$canonical_img" | tr '/:' '-')
    filename="$OUTPUT_DIR/sbom-helm-${chart_name}-${safe_name}.cyclonedx.json"
    temp_filename="${filename}.tmp"

    echo "  Creating SBOM for $img -> $canonical_img: $filename"

    # Try to generate SBOM with syft directly first
    # Let stderr show to user, only capture stdout (JSON) to temp file
    # TODO: This is duplicated code: The syft call should be extracted to a function
    if ! SYFT_FORMAT_PRETTY=true \
         SYFT_ENRICH=all \
         SYFT_NIX_CAPTURE_OWNED_FILES=true \
         SYFT_SCOPE="all-layers" \
           syft -v "docker:$canonical_img" -o cyclonedx-json > "$temp_filename"; then

      echo "  Initial scan failed, pulling image with Docker to handle old format..." >&2
      rm -f "$temp_filename"

      # Pull the image with Docker to convert old image formats
      if ! docker pull "$canonical_img"; then
        echo "  WARNING: Failed to pull image $canonical_img, skipping..." >&2
        continue
      fi

      # Retry syft scan on the locally pulled image
      if ! SYFT_FORMAT_PRETTY=true \
           SYFT_ENRICH=all \
           SYFT_NIX_CAPTURE_OWNED_FILES=true \
           SYFT_SCOPE="all-layers" \
             syft -v "docker:$canonical_img" -o cyclonedx-json > "$temp_filename"; then
        echo "  WARNING: Failed to generate SBOM for $canonical_img even after Docker pull, skipping..." >&2
        rm -f "$temp_filename"
        # TODO: We want to fail (exit 1) in case things go south
        continue
      fi
    fi

    # Validate that we have a valid JSON file
    if [[ ! -s "$temp_filename" ]]; then
      echo "  WARNING: Empty SBOM output for $canonical_img, skipping..." >&2
      rm -f "$temp_filename"
        # TODO: We want to fail (exit 1) in case things go south
      continue
    fi

    if ! jq empty "$temp_filename" 2>/dev/null; then
      echo "  WARNING: Invalid JSON in SBOM output for $canonical_img:" >&2
      head -5 "$temp_filename" >&2
      rm -f "$temp_filename"
        # TODO: We want to fail (exit 1) in case things go south
      continue
    fi

    # Parse registry and image path for OCI purl
    # Format: pkg:oci/namespace/name@version?repository_url=registry
    registry="${canonical_img%%/*}"  # Extract registry (e.g., quay.io)
    image_path="${canonical_img#*/}" # Remove registry (e.g., wire/brig:do-not-use)
    image_name="${image_path%:*}"    # Remove tag (e.g., wire/brig)
    image_tag="${image_path##*:}"    # Extract tag (e.g., do-not-use)

    oci_purl="pkg:oci/${image_name}@${image_tag}?repository_url=${registry}"

    # Create helm purl to indicate this image is used by a helm chart
    # Format: pkg:helm/chart-name@chart-version
    helm_purl="pkg:helm/${chart_name}@${chart_version}"

    # Add purl, scope tag, and helm chart source reference using jq
    # We add both the OCI purl (for the container image) and helm purl (for the chart context)
    # TODO: `metadata.component.properties` add nothing - some information is even wrong. Remove it and its variables.
    jq --arg oci_purl "$oci_purl" \
       --arg helm_purl "$helm_purl" \
       --arg chart_url "$chart_url" \
       --arg chart_name "$chart_name" \
       '.metadata.component.purl = $oci_purl |
        .metadata.component.properties += [
          {"name": "scope", "value": "test"},
          {"name": "helm.chart.name", "value": $chart_name},
          {"name": "helm.chart.purl", "value": $helm_purl}
        ] |
        .metadata.component.externalReferences += [
          {"type": "build-meta", "url": $chart_url, "comment": "Source Helm chart"}
        ]' \
       "$temp_filename" > "$filename"
    rm "$temp_filename"
  done <<< "$images"
done

echo "SBOM generation complete. Output directory: $OUTPUT_DIR"
