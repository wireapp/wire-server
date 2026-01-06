#!/usr/bin/env bash

# This script builds all the images in wireServer.images attribute of the flake
# and loads them into the docker daemon of kind using the repository name
# specified in the image derivation and tag specified by environment variable
# "$DOCKER_TAG".

set -euo pipefail

set -x

# nix attribute under wireServer containing all the images
readonly IMAGE_ATTR=${1:?$usage}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &>/dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

tmp_link_store=$(mktemp -d)

image_stream_file="$tmp_link_store/image-stream"
nix -v --show-trace -L build -f "$ROOT_DIR#$IMAGE_ATTR" -o "$image_stream_file"
image_file="$tmp_link_store/image"
image_file_tagged="$tmp_link_store/image-tagged"
"$image_stream_file" > "$image_file"
repo=$(skopeo list-tags "docker-archive://$image_file" | jq -r '.Tags[0] | split(":") | .[0]')
skopeo copy --insecure-policy --additional-tag "$repo:$DOCKER_TAG" "docker-archive://$image_file" "docker-archive://$image_file_tagged"
kind load image-archive "$image_file_tagged" --name "$KIND_CLUSTER_NAME"
