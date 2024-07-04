#!/usr/bin/env bash

set -e

# run.sh should work no matter what is the current directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"
FED_VERSIONS=(0 1)

opts=( "--file" "$DOCKER_FILE" )
for v in "${FED_VERSIONS[@]}"; do
  var="ENABLE_FEDERATION_V$v"
  if [[ "${!var}" == 1 ]]; then
    opts+=( "--file" "$SCRIPT_DIR/federation-v$v.yaml" )
  fi
done

dc() {
  docker-compose "${opts[@]}" "$@"
}

cleanup () {
  dc down
}

if [ -z "$1" ]; then
  dc up -d
  trap cleanup EXIT
  echo "All Services started successfully, press Ctrl+C to stop them"
  # Wait for something to kill this
  sleep infinity
else
  dc "$@"
fi
