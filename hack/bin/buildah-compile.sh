#!/usr/bin/env bash

# This compiles wire-server inside an alpine-based container based on quay.io/wire/alpine-builder.
# the tool 'buildah' is used to mount some folders in, and to
# keep the stack caches of .stack and .stack-work (renamed to avoid conflicts) for the next compilation

# After compilation, ./buildah-make-images.sh can be used
# to bake individual executables into their respective docker images used by kubernetes.

set -ex

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

# Note: keep the following names and paths in sync with the other buildah-* scripts.
mkdir -p "$TOP_LEVEL"/.stack-root-buildah
mkdir -p "$TOP_LEVEL"/.stack-work-buildah
mkdir -p "$TOP_LEVEL"/dist-buildah
CONTAINER_NAME=wire-server-dev

# check for the existence of; or create a working container
buildah containers | awk '{print $5}' | grep "$CONTAINER_NAME" \
    || buildah from --name "$CONTAINER_NAME" -v "${TOP_LEVEL}":/src --pull quay.io/wire/alpine-builder:develop

# The first time round, we want to copy the .stack folder from the alpine-builder for future use. Afterwards, we want to re-use the "dirty" stack root folder.
# Current check hinges on the existence of a config file, and hardcodes some paths
ls "$TOP_LEVEL/.stack-root-buildah/config.yaml" 2> /dev/null \
    || buildah run "$CONTAINER_NAME" -- cp -a /root/.stack/. /src/.stack-root-buildah/

buildah run "$CONTAINER_NAME" -- /src/hack/bin/buildah-inside.sh "$@"
