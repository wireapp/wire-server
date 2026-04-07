#!/usr/bin/env bash
# scan-all-binaries-in-image.sh
# Scans all x86-64 ELF binaries in a Docker image for forbidden instructions

set -euo pipefail

if [ $# -eq 0 ]; then
    echo "Usage: $0 <docker-image>"
    echo "Example: $0 quay.io/wire/brig:latest"
    exit 1
fi

IMAGE="$1"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CAPSTONE_SCRIPT="$SCRIPT_DIR/capstone-scan.py"

if [ ! -f "$CAPSTONE_SCRIPT" ]; then
    echo "Error: capstone-scan.py not found at $CAPSTONE_SCRIPT"
    exit 1
fi

echo "========================================="
echo "Scanning all binaries in: $IMAGE"
echo "========================================="

# Pull the image
echo "Pulling image..."
docker pull "$IMAGE"

# Create container and extract filesystem
CONTAINER=$(docker create "$IMAGE")
TEMP_DIR=$(mktemp -d)
trap 'docker rm $CONTAINER > /dev/null 2>&1; chmod -R +w $TEMP_DIR 2>/dev/null || true; rm -rf $TEMP_DIR' EXIT

echo "Extracting filesystem..."
docker export "$CONTAINER" | tar -C "$TEMP_DIR" -x

# Find all ELF binaries
echo "Finding ELF binaries..."
BINARIES=$(find "$TEMP_DIR" -type f -exec file {} \; | grep -i 'ELF.*x86-64' | cut -d: -f1)
BINARY_COUNT=$(echo "$BINARIES" | wc -l)

echo "Found $BINARY_COUNT x86-64 ELF binaries to scan"
echo ""

# Scan each binary
FAILED_COUNT=0
PASSED_COUNT=0
CURRENT=0
FAILED_BINARIES=()

while IFS= read -r binary; do
    CURRENT=$((CURRENT + 1))
    rel_path="${binary#"$TEMP_DIR"}"
    echo "[$CURRENT/$BINARY_COUNT] Scanning: $rel_path"

    if "$CAPSTONE_SCRIPT" "$binary" 2>&1; then
        echo "  ✓ OK"
        PASSED_COUNT=$((PASSED_COUNT + 1))
    else
        echo "  ✗ FAILED - contains forbidden instructions!"
        FAILED_COUNT=$((FAILED_COUNT + 1))
        FAILED_BINARIES+=("$rel_path")
    fi
    echo ""
done <<< "$BINARIES"

# Summary
echo "========================================="
echo "Summary for: $IMAGE"
echo "========================================="
echo "Total binaries scanned: $BINARY_COUNT"
echo "Passed: $PASSED_COUNT"
echo "Failed: $FAILED_COUNT"

if [ $FAILED_COUNT -gt 0 ]; then
    echo ""
    echo "Failed binaries:"
    for failed_binary in "${FAILED_BINARIES[@]}"; do
        echo "  - $failed_binary"
    done
    echo ""
    echo "❌ Image contains binaries with forbidden instructions!"
    exit 1
else
    echo ""
    echo "✅ All binaries are compliant!"
    exit 0
fi
