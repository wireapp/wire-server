#!/usr/bin/env bash
# Extract memchr-test binary from docker image

set -euo pipefail

if [ $# -eq 0 ]; then
    echo "Usage: $0 <image-name>"
    echo "Example: $0 quay.io/wire/memchr-test:latest"
    exit 1
fi

IMAGE="$1"
VERSION="0.1.0"

echo "Extracting memchr-test binary from ${IMAGE}..."

# Get the entrypoint to find the memchr-test path
ENTRYPOINT=$(docker inspect "${IMAGE}" | jq -r '.[0].Config.Entrypoint | .[-1]')
echo "memchr-test binary location: ${ENTRYPOINT}"

# Create a container from the image
CONTAINER=$(docker create "${IMAGE}")

# Export the container filesystem and extract just the memchr-test binary
echo "Extracting from container..."
TEMP_DIR=$(mktemp -d)
docker export "${CONTAINER}" | tar -x -C "${TEMP_DIR}" "${ENTRYPOINT#/}"
cp "${TEMP_DIR}${ENTRYPOINT}" "./memchr-test-${VERSION}"
rm -rf "${TEMP_DIR}"

# Clean up the container
docker rm "${CONTAINER}"

echo "Extracted memchr-test binary to: ./memchr-test-${VERSION}"
chmod +x "./memchr-test-${VERSION}"
