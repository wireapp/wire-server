#!/usr/bin/env bash

set -x

# run.sh should work no matter what is the current directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"

docker-compose --file "$DOCKER_FILE" --file "$SCRIPT_DIR/federation-v0.yaml" up
docker-compose --file "$DOCKER_FILE" --file "$SCRIPT_DIR/federation-v0.yaml" down
