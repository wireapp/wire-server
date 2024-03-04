#!/usr/bin/env bash

set -xe

# run.sh should work no matter what is the current directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"

cleanup () {
   docker-compose --file "$DOCKER_FILE" --file "$SCRIPT_DIR/federation-v0.yaml" down
}

docker-compose --file "$DOCKER_FILE" --file "$SCRIPT_DIR/federation-v0.yaml" up -d
trap cleanup EXIT
echo "All Services started successfully, press Ctrl+C to stop them"
# Wait for something to kill this
while true; do sleep 100000000; done
