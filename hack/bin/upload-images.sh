#!/usr/bin/env bash

# This script builds all the images in wireServer.images attribute of the flake
# and uploads them to the docker registry using the repository name specified in
# the image derivation and tag specified by environment variable "$DOCKER_TAG".
#
# If $DOCKER_USER and $DOCKER_PASSWORD are provided, the script will use them to
# upload the images.
#
# This script is intended to be run by CI/CD pipelines.

set -euo pipefail

readonly usage="USAGE: $0 <images_attr>"

# nix attribute under wireServer containing all the images
readonly IMAGES_ATTR=${1:?$usage}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &>/dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

tmp_link_store=$(mktemp -d)
image_list_file="$tmp_link_store/image-list"
nix -v --show-trace -L build "$ROOT_DIR#wireServer.imagesList" -o "$image_list_file" --fallback

# Build everything first so we can benefit the most from having many cores.
nix -v --show-trace -L build "$ROOT_DIR#wireServer.$IMAGES_ATTR.all" --no-link --fallback

xargs -I {} -P 10 "$SCRIPT_DIR/upload-image.sh" "wireServer.$IMAGES_ATTR.{}" < "$image_list_file"

printf '*** Uploading image %s\n' nginz
"$SCRIPT_DIR/upload-image.sh" nginz
