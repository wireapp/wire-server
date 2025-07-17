#!/usr/bin/env bash

set -euo pipefail

if [[ -z "${DOCKER_HUB_USERNAME+x}" ]]; then
    echo "Not logging in to docker hub as there are no credentials provided."
else
    helm registry login registry-1.docker.io --username "${DOCKER_HUB_USERNAME}" --password "${DOCKER_HUB_PASSWORD}"
fi
