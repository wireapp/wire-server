#!/usr/bin/env bash

# Pulls nginz and nginz_disco images from quay.io into buildah store, and loads
# them into the kind cluster

set -ex

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

DOCKER_DOWNLOAD_TAG=latest
DOCKER_TAG=${DOCKER_TAG:-$USER}
EXECUTABLES=${EXECUTABLES:-"nginz nginz_disco"}

for EX in $EXECUTABLES; do
    CONTAINER_NAME=$EX
    buildah containers | awk '{print $5}' | grep "$CONTAINER_NAME" ||
        buildah from --name "$CONTAINER_NAME" -v "${TOP_LEVEL}":/src --pull "quay.io/wire/$CONTAINER_NAME:$DOCKER_DOWNLOAD_TAG"
    buildah tag "quay.io/wire/$CONTAINER_NAME:$DOCKER_DOWNLOAD_TAG" "quay.io/wire/$CONTAINER_NAME:$DOCKER_TAG"
    if [[ "$BUILDAH_KIND_LOAD" -eq "1" ]]; then
        archiveDir=$(mktemp -d)
        imgPath="$archiveDir/${EX}_${DOCKER_TAG}.tar"
        imgName="quay.io/wire/$EX:$DOCKER_TAG"
        buildah push "$imgName" "docker-archive:$imgPath:$imgName"
        kind load image-archive --name "$KIND_CLUSTER_NAME" "$imgPath"
        rm -rf "$archiveDir"
    fi
done

if [[ "$BUILDAH_PUSH" -eq "1" ]]; then
    for EX in $EXECUTABLES; do
        buildah push "quay.io/wire/$EX:$DOCKER_TAG"
    done
fi

# general cleanup
"$DIR/buildah-purge-untagged.sh"
