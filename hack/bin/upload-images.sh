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

readonly usage="USAGE: $0 <images_attr>"

# nix attribute under wireServer from "$ROOT_DIR/nix" containing all the images
readonly IMAGES_ATTR=${1:?$usage}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

tmp_link_store=$(mktemp -d)
image_list_file="$tmp_link_store/image-list"
nix -v --show-trace -L build -f "$ROOT_DIR/nix" wireServer.imagesList -o "$image_list_file"

# Build everything first so we can benefit the most from having many cores.
nix -v --show-trace -L build -f "$ROOT_DIR/nix" "wireServer.$IMAGES_ATTR" --no-link

while IFS="" read -r image_name || [ -n "$image_name" ]
do
    printf '*** Uploading image %s\n' "$image_name"
    "$SCRIPT_DIR/upload-image.sh" "wireServer.$IMAGES_ATTR.$image_name"
done < "$image_list_file"
