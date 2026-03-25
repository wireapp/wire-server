#!/usr/bin/env bash

# This script builds an from the attribute provided at $1, which must be present
# in the flake, and uploads it to the docker registry using the repository name
# specified in the image derivation and tag specified by environment variable
# "$DOCKER_TAG".
#
# If $DOCKER_USER and $DOCKER_PASSWORD are provided, the script will use them to
# upload the images.
#
# This script is intended to be run by CI/CD pipelines.

set -euo pipefail

readonly DOCKER_TAG=${DOCKER_TAG:?"Please set the DOCKER_TAG env variable"}

readonly usage="USAGE: $0 <image_stream_file>"
readonly IMAGE_STREAM_FILE=${1:?$usage}

credsArgs=""
if [[ "${DOCKER_USER+x}" != "" ]]; then
    DOCKER_PASSWORD=${DOCKER_PASSWORD:?"DOCKER_PASSWORD must be provided when DOCKER_USER is provided"}
    credsArgs="--dest-creds=$DOCKER_USER:$DOCKER_PASSWORD"
fi

# Retry a command with exponential backoff
# quay.io sometimes rate-limits us, so try again.
# Also, skopeo's retry logic doesn't properly work, look here if you want to see very badly written go code:
# https://github.com/containers/skopeo/blob/869d496f185cc086f22d6bbb79bb57ac3a415617/vendor/github.com/containers/common/pkg/retry/retry.go#L52-L113
function retry {
    local max_attempts=$1
    local seconds_delay=1
    local attempt_count=1
    shift 1

    while [[ $attempt_count -le "$max_attempts" ]]; do
        if "$@"; then
            break
        else
            local status=$?
            if [[ $attempt_count -lt "$max_attempts" ]]; then
                echo "Command [$*] failed after attempt $attempt_count of $max_attempts. Retrying in $seconds_delay second(s)." >&2
                sleep $seconds_delay
            elif [[ $attempt_count -eq "$max_attempts" ]]; then
                echo "Command [$*] failed after $attempt_count attempt(s)" >&2
                return $status
            fi
        fi
        attempt_count=$((attempt_count + 1))
        seconds_delay=$((seconds_delay * 2))
    done
}

tmp_link_store=$(mktemp -d)
# Using dockerTools.streamLayeredImage outputs an executable which prints the
# image tar on stdout when executed. This is done so we don't store large images
# in nix-store and hence in the cache. This also reduces the things we have to
# download from the cache when uploading multiple images as the image is a
# product of other store paths which should already be cached and a lot of our
# images should have a few common layers. More information:
# https://nixos.org/manual/nixpkgs/unstable/#ssec-pkgs-dockerTools-streamLayeredImage
image_file="$tmp_link_store/image"
"$IMAGE_STREAM_FILE" >"$image_file"
repo=$(skopeo list-tags "docker-archive://$image_file" | jq -r '.Tags[0] | split(":") | .[0]')
printf "*** Uploading $image_file to %s:%s\n" "$repo" "$DOCKER_TAG"
# shellcheck disable=SC2086
retry 5 skopeo --insecure-policy copy --retry-times 5 $credsArgs "docker-archive://$image_file" "docker://$repo:$DOCKER_TAG"
