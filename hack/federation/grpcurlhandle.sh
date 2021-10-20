#!/usr/bin/env bash

# Usage:
#
# First, run services (including nginz) locally:
#
#   export INTEGRATION_USE_NGINZ=1; ./services/start-services-only.sh
#
# Then, run this command for user search (ideally on a handle that exists)
#
#
#
handle="pyaewrqxggbtbvzdsubkl" # change this to a valid handle in your local database

command -v grpcurl >/dev/null 2>&1 || {
    echo >&2 "grpcurl is not installed, aborting. Maybe try ' nix-env -iA nixpkgs.grpcurl '?"
    exit 1
}

path=$(echo -n federation/get-user-by-handle | base64)
body=$(echo -n "\"$handle\"" | base64)

TOP_LEVEL="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

function getHandle() {
    echo ""
    echo "===> getHandle: $1"
    if [ -z "$SERVERNAME" ]; then
        AUTHORITY=""
    else
        AUTHORITY="-authority $SERVERNAME"
    fi
    set -x
    grpcurl -d @ -format json $AUTHORITY $MODE -import-path "${TOP_LEVEL}/libs/wire-api-federation/proto/" -proto router.proto "$HOST:$PORT" wire.federator.Inward/call <<EOM
{"component": "Brig", "path": "$path", "body": "$body", "originDomain": "grpcurl.example.com"}
EOM
    { set +x; } 2>/dev/null # stop outputting commands and don't print the set +x line
    echo "===|"
    echo
}

HOST="localhost"
SERVERNAME="federator.integration.example.com"
PORT=8443
MODE="-cacert ${TOP_LEVEL}/services/nginz/integration-test/conf/nginz/integration-ca.pem"
getHandle "local"
