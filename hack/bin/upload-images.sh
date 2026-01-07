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

# Build everything first so we can benefit the most from having many cores.
result=$(mktemp -d -t stream-images.XXXXXX)
nix -v --show-trace -L build "$ROOT_DIR#wireServer.$IMAGES_ATTR.all" --out-link "$result/images" --fallback

find "$result/images/" -type l -print0 | xargs -0 -I {} -P 10 "$SCRIPT_DIR/upload-image.sh" {}

printf '*** Uploading image %s\n' nginz
nginz_image=$(mktemp -d -t stream-nginz-image.XXXXXX)
nix -v --show-trace -L build "$ROOT_DIR#nginz" --out-link "$nginz_image/image" --fallback
"$SCRIPT_DIR/upload-image.sh" "$nginz_image/image"
