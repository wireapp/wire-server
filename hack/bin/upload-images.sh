#!/usr/bin/env bash

# This script builds all the images in wireServer.images attribute of
# $ROOT_DIR/nix/default.nix and uploads them to the docker registry using the
# repository name specified in the image derivation and tag specified by
# environment variable "$DOCKER_TAG".
#
# This script is intended to be run by CI/CD pipelines.

set -euo pipefail

readonly DOCKER_TAG=${DOCKER_TAG:?"Please set the DOCKER_TAG env variable"}

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
readonly SCRIPT_DIR ROOT_DIR

if [[ "${DOCKER_USER+x}" != "" ]]; then

    DOCKER_PASSWORD=${DOCKER_PASSWORD:?"DOCKER_PASSWORD must be provided when DOCKER_USER is provided"}
    DOCKER_SERVER=${DOCKER_SERVER:-"quay.io"}

    docker_cred=$(echo -n "$DOCKER_USER:$DOCKER_PASSWORD" | base64)
    authfile=$(mktemp)
    jq -n > "$authfile" \
       '{auths: { "\($docker_server)": { auth: "\($docker_cred)"  }}}' \
       --arg docker_server "$DOCKER_SERVER" \
       --arg docker_cred "$docker_cred"
    export REGISTRY_AUTH_FILE=$authfile
fi

while IFS='' read -r image; do
    repo=$(skopeo list-tags "docker-archive://$image" | jq -r '.Tags[0] | split(":") | .[0]')
    skopeo copy "docker-archive://$image" "docker://$repo:$DOCKER_TAG"
done < <(nix-build "$ROOT_DIR/nix" -A wireServer.images)
