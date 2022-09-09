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

readonly DOCKER_TAG=${DOCKER_TAG:?"Please set the DOCKER_TAG env variable"}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

if [[ "${DOCKER_USER+x}" != "" ]]; then
    DOCKER_SERVER=${DOCKER_SERVER:-"quay.io"}
    docker_cred=$(echo -n "$DOCKER_USER:$DOCKER_PASSWORD" | base64)
    authfile=$(mktemp)
    jq -n > "$authfile" \
       '{auths: { "\($docker_server)": { auth: "\($docker_cred)"  }}}' \
       --arg docker_server "$DOCKER_SERVER" \
       --arg docker_cred "$docker_cred"
    export REGISTRY_AUTH_FILE=$authfile
    echo "setting REGISTRY_AUTH_FILE"
fi

images_list="$(nix-build "$ROOT_DIR/nix" -A wireServer.imagesList)"

while IFS="" read -r image_name || [ -n "$image_name" ]
do
    printf '*** Building image %s\n' "$image_name"
    image=$(nix-build "$ROOT_DIR/nix" -A "wireServer.images.$image_name")
    repo=$(skopeo list-tags "docker-archive://$image" | jq -r '.Tags[0] | split(":") | .[0]')
    # shellcheck disable=SC2086
    skopeo --insecure-policy copy "docker-archive://$image" "docker://$repo:$DOCKER_TAG"
done < "$images_list"
