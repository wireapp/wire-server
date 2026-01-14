#!/usr/bin/env bash

# This script builds all the images in wireServer.images attribute of the flake
# and loads into the docker daemon of kind using the repository name specified
# in the image derivation and tag specified by environment variable
# "$DOCKER_TAG".

set -euo pipefail

set -x

# nix attribute under wireServer containing all the images
readonly IMAGES_ATTR="imagesUnoptimizedNoDocs"

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &>/dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

tmp_link_store=$(mktemp -d)
image_list_file="$tmp_link_store/image-list"
nix -v --show-trace -L build "$ROOT_DIR#wireServer.imagesList" -o "$image_list_file"

xargs -I {} -P 10 "$SCRIPT_DIR/kind-upload-image.sh" "wireServer.$IMAGES_ATTR.{}" < "$image_list_file"

printf '*** Unploading image %s\n' nginz
"$SCRIPT_DIR/kind-upload-image.sh" nginz
