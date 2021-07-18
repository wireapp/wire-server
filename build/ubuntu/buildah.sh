#!/usr/bin/env bash


# This is based of the hack/bin/buildah_* set of scripts, with the aim of reproducing the Docker based
# set of Dockerfiles used to create images for development, ci and deployment using Ubuntu 20.04 as 
# a base image and nix.

set -ex

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"
# Use layers and cache
BUILDAH_LAYERS=true

# check for the existence of; or create a working container
# buildah containers | awk '{print $5}' | grep "ubuntu-nix-builder" \
#     || buildah from --name "ubuntu-nix-builder" -v "${TOP_LEVEL}":/src --pull quay.io/wire/ubuntu-nix-builder:develop

buildah bud -v "${TOP_LEVEL}":/src -t "base" -f "${DIR}/Dockerfile.base" .
echo "built base"