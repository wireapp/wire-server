#!/usr/bin/env bash

set -euo pipefail

# Find git repository root to ensure paths work regardless of where script is executed
GIT_ROOT="$(git rev-parse --show-toplevel)"

OUTPUT_DIR_BASE="${1:-.}"
VERSION="${2:-}"

if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 <output-dir-base> <version>"
  echo "  output-dir-base: Base directory to write SBOM files"
  echo "                   Will create subdirectories: runtime/ and buildtime/"
  echo "  version:         Version to use for SBOMs (e.g., 5.28.22)"
  exit 1
fi

# Create separate directories for runtime and buildtime SBOMs
OUTPUT_DIR_RUNTIME="$OUTPUT_DIR_BASE/runtime"
OUTPUT_DIR_BUILDTIME="$OUTPUT_DIR_BASE/buildtime"
mkdir -p "$OUTPUT_DIR_RUNTIME" "$OUTPUT_DIR_BUILDTIME"

# Get current git commit hash for linking to source
GIT_COMMIT=$(git rev-parse HEAD)

# Get system architecture for the flake reference
SYSTEM=$(nix eval --impure --raw --expr 'builtins.currentSystem')

# Track errors during processing
error_count=0

# Function to generate SBOM with metadata and convert it
# Parameters: flake_ref, output_file, component_name, purl, sbom_type
#   sbom_type must be either "runtime" or "buildtime"
# Uses global variables: VERSION, GIT_COMMIT
generate_sbom() {
  local flake_ref="$1"
  local output_file="$2"
  local component_name="$3"
  local purl="$4"
  local sbom_type="$5"

  # Validate sbom_type parameter
  if [[ "$sbom_type" != "runtime" && "$sbom_type" != "buildtime" ]]; then
    echo "  ERROR: Invalid sbom_type '$sbom_type'. Must be 'runtime' or 'buildtime'" >&2
    return 1
  fi

  echo "  Generating $sbom_type SBOM..."

  # Build sbomnix command with buildtime flag if needed
  local sbomnix_cmd="sbomnix \"$flake_ref\" --verbose 1"
  [[ "$sbom_type" == "buildtime" ]] && sbomnix_cmd="$sbomnix_cmd --buildtime"
  sbomnix_cmd="$sbomnix_cmd --cdx=\"$output_file\""

  if ! eval "$sbomnix_cmd" 2>&1; then
    echo "  ERROR: sbomnix ($sbom_type) failed" >&2
    return 1
  fi

  # Add metadata and convert to v1.6
  if [[ -f "$output_file" ]]; then
    local temp_file="${output_file}.tmp"
    local source_url="https://github.com/wireapp/wire-server/tree/${GIT_COMMIT}"

    # Remove invalid CPEs (those with empty version fields) and add our metadata
    jq --arg component_name "$component_name" \
       --arg purl "$purl" \
       --arg source_url "$source_url" \
       --arg version "$VERSION" \
       '# Remove CPE from metadata.component if it has empty version (::)
        if .metadata.component.cpe and (.metadata.component.cpe | contains("::")) then
          .metadata.component |= del(.cpe)
        else . end |
        # Add our metadata
        .metadata.component.name = $component_name |
        .metadata.component.version = $version |
        .metadata.component.purl = $purl |
        .metadata.component.externalReferences += [
          {"type": "vcs", "url": $source_url, "comment": "Source repository"}
        ]' \
       "$output_file" > "$temp_file"
    mv "$temp_file" "$output_file"

    # Convert to CycloneDX 1.6
    if cyclonedx convert --input-file "$output_file" --output-file "$temp_file" --output-format json --output-version v1_6; then
      mv "$temp_file" "$output_file"
      echo "  ✓ $sbom_type SBOM created: $output_file"
      return 0
    else
      echo "  ERROR: CycloneDX conversion failed for $sbom_type SBOM" >&2
      return 1
    fi
  fi

  return 1
}

echo "Generating SBOMs for Nix devShells..."
echo "Version: $VERSION"
echo "System: $SYSTEM"
echo "Output directory (runtime): $OUTPUT_DIR_RUNTIME"
echo "Output directory (buildtime): $OUTPUT_DIR_BUILDTIME"
echo ""

# Get list of devShell names from the devShells attrset
echo "Discovering devShells..."
mapfile -t devshell_names < <(nix --extra-experimental-features 'nix-command flakes' eval "$GIT_ROOT#devShells.${SYSTEM}" --apply 'shells: builtins.concatStringsSep "\n" (builtins.attrNames shells)' --raw 2>&1 | grep -v warning)

echo "Found ${#devshell_names[@]} devShells to process"
echo ""

# Process each devShell
for devshell_name in "${devshell_names[@]}"; do
  # Skip empty lines
  [[ -z "$devshell_name" ]] && continue

  echo "Processing devShell: $devshell_name"

  # Create flake reference for the devShell
  flake_ref="$GIT_ROOT#devShells.${SYSTEM}.${devshell_name}"
  echo "  Flake reference: $flake_ref"

  # Create component metadata
  component_name="wire-server-devshell-${devshell_name}"
  purl="pkg:nix/wire-server/${devshell_name}@${VERSION}"

  # Generate runtime-only SBOM
  runtime_output_file="$OUTPUT_DIR_RUNTIME/sbom-nix-devshell-${devshell_name}.${VERSION}.cyclonedx.json"
  if ! generate_sbom "$flake_ref" "$runtime_output_file" "$component_name" "$purl" "runtime"; then
    ((error_count++))
  fi

  # Generate buildtime SBOM (includes build dependencies)
  buildtime_output_file="$OUTPUT_DIR_BUILDTIME/sbom-nix-devshell-${devshell_name}.${VERSION}.cyclonedx.json"
  if ! generate_sbom "$flake_ref" "$buildtime_output_file" "$component_name" "$purl" "buildtime"; then
    ((error_count++))
  fi

  echo ""
done

echo "SBOM generation complete."
echo "Runtime SBOMs:   $OUTPUT_DIR_RUNTIME"
echo "Buildtime SBOMs: $OUTPUT_DIR_BUILDTIME"
echo "Total devShells processed: ${#devshell_names[@]}"

if [[ $error_count -gt 0 ]]; then
  echo "WARNING: $error_count error(s) occurred during SBOM generation" >&2
  exit 1
fi
