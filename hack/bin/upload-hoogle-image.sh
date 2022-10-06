#!/usr/bin/env bash

# This script builds the hoogle image from the wireServer.hoogleImage attribute
# of $ROOT_DIR/nix/default.nix and uploads it to the docker registry using the
# repository name specified in the image derivation and tag specified by
# environment variable "$DOCKER_TAG".
#
# If $DOCKER_USER and $DOCKER_PASSWORD are provided, the script will use them to
# upload the images.
#
# This script is intended to be run by CI/CD pipelines.

set -euo pipefail

readonly DOCKER_TAG=${DOCKER_TAG:?"Please set the DOCKER_TAG env variable"}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

credsArgs=""
if [[ "${DOCKER_USER+x}" != "" ]]; then
    DOCKER_PASSWORD=${DOCKER_PASSWORD:?"DOCKER_PASSWORD must be provided when DOCKER_USER is provided"}
    credsArgs="--dest-creds=$DOCKER_USER:$DOCKER_PASSWORD"
fi

tmp_link_store=$(mktemp -d)
# wireServer.hoogleImage outputs an executable which prints the image on stdout
# when executed. This is done so we don't store large images in nix-store and
# hence in the cache. The image is a product of other store paths which should
# already be cached. More information:
# https://nixos.org/manual/nixpkgs/unstable/#ssec-pkgs-dockerTools-streamLayeredImage
image_stream_file="$tmp_link_store/image_stream"
nix -v --show-trace -L build -f "$ROOT_DIR/nix" "wireServer.hoogleImage" -o "$image_stream_file"
image_file="$tmp_link_store/image"
"$image_stream_file" | gzip > "$image_file"
repo=$(skopeo list-tags "docker-archive://$image_file" | jq -r '.Tags[0] | split(":") | .[0]')
echo "Uploading $image_file to $repo:$DOCKER_TAG"
# shellcheck disable=SC2086
skopeo --insecure-policy copy $credsArgs "docker-archive://$image_file" "docker://$repo:$DOCKER_TAG"
