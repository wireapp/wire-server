#!/usr/bin/env bash

set -ex

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

EXECUTABLES=${EXECUTABLES:-"cannon brig cargohold galley gundeck federator brig-index brig-schema galley-schema galley-migrate-data gundeck-schema proxy spar spar-schema spar-migrate-data brig-integration galley-integration spar-integration gundeck-integration cargohold-integration federator-integration"}
CONTAINER_NAME="output"
DOCKER_TAG=${DOCKER_TAG:-$USER}

buildah containers | awk '{print $5}' | grep "$CONTAINER_NAME" ||
    buildah from --name "$CONTAINER_NAME" -v "${TOP_LEVEL}":/src --pull quay.io/wire/alpine-deps:develop

# Only brig needs these templates, but for simplicity we add them to all resulting images (optimization FUTUREWORK)
buildah run "$CONTAINER_NAME" -- sh -c 'mkdir -p /usr/share/wire/ && cp -r "/src/services/brig/deb/opt/brig/templates/." "/usr/share/wire/templates"'

for EX in $EXECUTABLES; do
    # Copy the main executable into the PATH on the container
    buildah run "$CONTAINER_NAME" -- cp "/src/dist-buildah/$EX" "/usr/bin/$EX"

    # Start that executable by default when launching the docker image
    buildah config --entrypoint "[ \"/usr/bin/dumb-init\", \"--\", \"/usr/bin/$EX\" ]" "$CONTAINER_NAME"
    buildah config --cmd null "$CONTAINER_NAME"

    # Bake an image
    buildah commit "$CONTAINER_NAME" quay.io/wire/"$EX":"$DOCKER_TAG"

    # remove executable from the image in preparation for the next iteration
    buildah run "$CONTAINER_NAME" -- rm "/usr/bin/$EX"
done

if [[ "$BUILDAH_PUSH" -eq "1" ]]; then
    for EX in $EXECUTABLES; do
        buildah push "quay.io/wire/$EX:$DOCKER_TAG"
    done
fi

if [[ "$BUILDAH_KIND_LOAD" -eq "1" ]]; then
    archiveDir=$(mktemp -d)
    for EX in $EXECUTABLES; do
        imgPath="$archiveDir/${EX}_${DOCKER_TAG}.tar"
        imgName="quay.io/wire/$EX:$DOCKER_TAG"
        buildah push "$imgName" "docker-archive:$imgPath:$imgName"
        kind load image-archive --name "$KIND_CLUSTER_NAME" "$imgPath"
    done
    rm -rf "$archiveDir"
fi
