#!/usr/bin/env bash

# This compiles wire-server inside an ubuntu-based container based on quay.io/wire/ubuntu20-builder.
# the tool 'buildah' is used to mount some folders in, and to
# keep the stack caches of /.root/.cabal and dist-newstyle (renamed to avoid conflicts) for the next compilation

# After compilation, ./buildah-make-images.sh can be used
# to bake individual executables into their respective docker images used by kubernetes.

set -ex

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

# Note: keep the following names and paths in sync with the other buildah-* scripts.
mkdir -p "$TOP_LEVEL"/buildah/dot-cabal
mkdir -p "$TOP_LEVEL"/buildah/dist-newstyle
mkdir -p "$TOP_LEVEL"/buildah/dist

CONTAINER_NAME=wire-server-dev

# check for the existence of; or create a working container
buildah containers | awk '{print $5}' | grep "$CONTAINER_NAME" \
    || buildah from --name "$CONTAINER_NAME" -v "${TOP_LEVEL}":/src --pull quay.io/wire/ubuntu20-builder:develop

# copy /root/.cabal out of the container
ls "$TOP_LEVEL"/buildah/dot-cabal/store 2> /dev/null \
    || buildah run "$CONTAINER_NAME" -- cp -a /root/.cabal/. /src/buildah/dot-cabal

buildah run "$CONTAINER_NAME" -- /src/hack/bin/buildah-inside.sh "$@"
