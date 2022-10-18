#!/usr/bin/env bash

# This script builds an from the attribute provided at $1, which must be present
# in $ROOT_DIR/nix/default.nix, and uploads it to the docker registry using the
# repository name specified in the image derivation and tag specified by
# environment variable "$DOCKER_TAG".
#
# If $DOCKER_USER and $DOCKER_PASSWORD are provided, the script will use them to
# upload the images.
#
# This script is intended to be run by CI/CD pipelines.

set -euo pipefail

readonly DOCKER_TAG=${DOCKER_TAG:?"Please set the DOCKER_TAG env variable"}

readonly usage="USAGE: $0 <image_attr>"
readonly IMAGE_ATTR=${1:?$usage}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

credsArgs=""
if [[ "${DOCKER_USER+x}" != "" ]]; then
    DOCKER_PASSWORD=${DOCKER_PASSWORD:?"DOCKER_PASSWORD must be provided when DOCKER_USER is provided"}
    credsArgs="--dest-creds=$DOCKER_USER:$DOCKER_PASSWORD"
fi

tmp_link_store=$(mktemp -d)
# Using dockerTools.streamLayeredImage outputs an executable which prints the
# image tar on stdout when executed. This is done so we don't store large images
# in nix-store and hence in the cache. This also reduces the things we have to
# download from the cache when uploading multiple images as the image is a
# product of other store paths which should already be cached and a lot of our
# images should have a few common layers. More information:
# https://nixos.org/manual/nixpkgs/unstable/#ssec-pkgs-dockerTools-streamLayeredImage
image_stream_file="$tmp_link_store/image_stream"
nix -v --show-trace -L build -f "$ROOT_DIR/nix" "$IMAGE_ATTR" -o "$image_stream_file"
image_file="$tmp_link_store/image"
"$image_stream_file" > "$image_file"
repo=$(skopeo list-tags "docker-archive://$image_file" | jq -r '.Tags[0] | split(":") | .[0]')
printf "*** Uploading $image_file to %s:%s" "$repo" "$DOCKER_TAG"
# shellcheck disable=SC2086
skopeo --insecure-policy copy $credsArgs "docker-archive://$image_file" "docker://$repo:$DOCKER_TAG"
