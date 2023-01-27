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
readonly IMAGE_ATTR=${1:?$usage}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &>/dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

tmp_link_store=$(mktemp -d)

image_stream_file="$tmp_link_store/image-stream"
nix -v --show-trace -L build -f "$ROOT_DIR/nix" "$IMAGE_ATTR" -o "$image_stream_file"
image_file="$tmp_link_store/image"
image_file_tagged="$tmp_link_store/image-tagged"
"$image_stream_file" > "$image_file"
repo=$(skopeo list-tags "docker-archive://$image_file" | jq -r '.Tags[0] | split(":") | .[0]')
skopeo copy --insecure-policy --additional-tag "$repo:$DOCKER_TAG" "docker-archive://$image_file" "docker-archive://$image_file_tagged"
kind load image-archive "$image_file_tagged" --name "$KIND_CLUSTER_NAME"
