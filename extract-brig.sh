#!/usr/bin/env bash
# Extract brig binary from docker image

set -euo pipefail

VERSION="5.25.0"
IMAGE="quay.io/wire/brig:${VERSION}"

echo "Extracting brig binary from ${IMAGE}..."

# Pull the image
docker pull "${IMAGE}"

# Get the entrypoint to find the brig path
ENTRYPOINT=$(docker inspect "${IMAGE}" | jq -r '.[0].Config.Entrypoint | .[-1]')
echo "Brig binary location: ${ENTRYPOINT}"

# Create a container from the image
CONTAINER=$(docker create "${IMAGE}")

# Export the container filesystem and extract just the brig binary
echo "Extracting from container..."
TEMP_DIR=$(mktemp -d)
docker export "${CONTAINER}" | tar -x -C "${TEMP_DIR}" "${ENTRYPOINT#/}"
cp "${TEMP_DIR}${ENTRYPOINT}" "./brig-${VERSION}"
rm -rf "${TEMP_DIR}"

# Clean up the container
docker rm "${CONTAINER}"

echo "Extracted brig binary to: ./brig-${VERSION}"
