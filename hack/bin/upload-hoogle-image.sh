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
image_file="$tmp_link_store/image"
nix -v --show-trace -L build -f "$ROOT_DIR/nix" "wireServer.hoogleImage" -o "$image_file"
repo=$(skopeo list-tags "docker-archive://$image_file" | jq -r '.Tags[0] | split(":") | .[0]')
echo "Uploading $image_file to $repo:$DOCKER_TAG"
# shellcheck disable=SC2086
skopeo --insecure-policy copy $credsArgs "docker-archive://$image_file" "docker://$repo:$DOCKER_TAG"
