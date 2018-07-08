#!/usr/bin/env bash

# run.sh should work no matter what is the current directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"

# If there are schema changes and you don't force pull the docker
# migrations, you may run out of sync and you would get this error
# message (or a similar one):
#
#     brig: Schema Version too old! Expecting at least: 49, but got: 48
#
# So we always pull the migrations image first.
docker pull quay.io/wire/migrations

docker-compose --file "$DOCKER_FILE" up
