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

readonly DOCKER_TAG=${DOCKER_TAG:?"Please set the DOCKER_TAG env variable"}
# nix attribute under wireServer from "$ROOT_DIR/nix" containing all the images
readonly IMAGES_ATTR=${1:?$usage}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

credsArgs=""
if [[ "${DOCKER_USER+x}" != "" ]]; then
    DOCKER_PASSWORD=${DOCKER_PASSWORD:?"DOCKER_PASSWORD must be provided when DOCKER_USER is provided"}
    credsArgs="--dest-creds=$DOCKER_USER:$DOCKER_PASSWORD"
fi

images_list="$(nix -v --show-trace -L build -f "$ROOT_DIR/nix" wireServer.imagesList)"

# Build everything first so we can benefit the most from having many cores.
nix --show-trace -L build -f "$ROOT_DIR/nix" "wireServer.$IMAGES_ATTR"

while IFS="" read -r image_name || [ -n "$image_name" ]
do
    printf '*** Uploading image %s\n' "$image_name"
    image=$(nix --show-trace -L build -f "$ROOT_DIR/nix" "wireServer.$IMAGES_ATTR.$image_name")
    repo=$(skopeo list-tags "docker-archive://$image" | jq -r '.Tags[0] | split(":") | .[0]')
    echo "Uploading $image to $repo:$DOCKER_TAG"
    # shellcheck disable=SC2086
    skopeo --insecure-policy copy $credsArgs "docker-archive://$image" "docker://$repo:$DOCKER_TAG"
done < "$images_list"
