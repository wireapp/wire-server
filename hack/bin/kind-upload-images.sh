#!/usr/bin/env bash

# This script builds all the images in wireServer.images attribute of
# $ROOT_DIR/nix/default.nix and uploads them to the docker registry using the
# repository name specified in the image derivation and tag specified by
# environment variable "$DOCKER_TAG".
#
# If $DOCKER_USER and $DOCKER_PASSWORD are provided, the script will use them to
# upload the images.
#
# This script is intended to be run by CI/CD pipelines.

set -euo pipefail

set -x

# nix attribute under wireServer from "$ROOT_DIR/nix" containing all the images
readonly IMAGES_ATTR="imagesUnoptimizedNoDocs"

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &>/dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

tmp_link_store=$(mktemp -d)
image_list_file="$tmp_link_store/image-list"
nix -v --show-trace -L build -f "$ROOT_DIR/nix" wireServer.imagesList -o "$image_list_file"

xargs -I {} -P 10 "$SCRIPT_DIR/kind-upload-image.sh" "wireServer.$IMAGES_ATTR.{}" < "$image_list_file"

printf '*** Unploading image %s\n' nginz
"$SCRIPT_DIR/kind-upload-image.sh" nginz
