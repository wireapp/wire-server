#!/usr/bin/env bash

set -euo pipefail

# Centralized syft invocation with all standard Wire server settings
# This ensures consistent syft configuration across all SBOM generation

SOURCE="${1:-}"
OUTPUT_FILE="${2:-}"

if [[ -z "$SOURCE" ]] || [[ -z "$OUTPUT_FILE" ]]; then
  echo "Usage: $0 <source> <output-file>" >&2
  echo "  source: syft source (e.g., 'docker:image:tag', 'oci-dir:/path/to/oci')" >&2
  echo "  output-file: path to write CycloneDX JSON output" >&2
  exit 1
fi

# Standard syft configuration for Wire server SBOMs
SYFT_FORMAT_PRETTY=true \
SYFT_ENRICH=all \
SYFT_NIX_CAPTURE_OWNED_FILES=true \
SYFT_SCOPE="all-layers" \
  syft -v "$SOURCE" -o cyclonedx-json > "$OUTPUT_FILE"
