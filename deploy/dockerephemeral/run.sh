#!/usr/bin/env bash

# To start the federation v0, v1, v2 backends, set ENABLE_FEDERATION_V0=1, ENABLE_FEDERATION_V1=1, ENABLE_FEDERATION_V2=1
# in the env where this script is run.
# Set WIRE_S3_MODE=minio to use the legacy MinIO-backed fake S3 service.

set -e

# run.sh should work no matter what is the current directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"
S3_MODE="${WIRE_S3_MODE:-rustfs}"
FED_VERSIONS=(0 1 2)

if [[ -e "${SCRIPT_DIR}/run.before.hook.local" ]]; then
  # shellcheck disable=SC1091
  . "${SCRIPT_DIR}/run.before.hook.local"
fi

opts=("--file" "$DOCKER_FILE")
case "$S3_MODE" in
  rustfs)
    ;;
  minio)
    opts+=("--file" "$SCRIPT_DIR/docker-compose.legacy-minio.yaml")
    ;;
  *)
    echo "Unsupported WIRE_S3_MODE: $S3_MODE" >&2
    exit 1
    ;;
esac
for v in "${FED_VERSIONS[@]}"; do
  var="ENABLE_FEDERATION_V$v"
  if [[ "${!var}" == 1 ]]; then
    opts+=("--file" "$SCRIPT_DIR/federation-v$v.yaml")
  fi
done

dc() {
  docker-compose "${opts[@]}" "$@"
  return 0
}

cleanup() {
  dc down
  return 0
}

if [[ -z "$1" ]]; then
  dc up -d
  if [[ -e "${SCRIPT_DIR}/run.after.hook.local" ]]; then
    # shellcheck disable=SC1091
    . "${SCRIPT_DIR}/run.after.hook.local"
  fi
  trap cleanup EXIT
  echo "All Services started successfully, press Ctrl+C to stop them"
  # Wait for something to kill this
  sleep infinity
else
  dc "$@"
fi
