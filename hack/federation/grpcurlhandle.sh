#!/usr/bin/env bash

command -v grpcurl >/dev/null 2>&1 || { echo >&2 "grpcurl is not installed, aborting. Maybe try ' nix-env -iA nixpkgs.grpcurl '?"; exit 1; }

path=$(echo -n users/by-handle | base64)
queryK=$(echo -n handle | base64)
queryV=$(echo -n alice | base64)

function getHandle() {
    echo ""
    echo "===> getHandle: $1"
    if [ -z "$SERVERNAME" ]; then
        AUTHORITY=""
    else
        AUTHORITY="-authority $SERVERNAME"
    fi
    set -x
    grpcurl -d @ -format json $AUTHORITY $MODE -proto ../../libs/wire-api-federation/proto/router.proto "$HOST:$PORT" wire.federator.Inward/call <<EOM
{"method": "GET", "component": "Brig", "path": "$path", "query": [{"key":"$queryK", "value":"$queryV"}]}
EOM
    { set +x; } 2>/dev/null # stop outputting commands and don't print the set +x line
    echo "===|"
    echo
}

HOST=localhost
PORT=8443
MODE="-cacert ../../services/nginz/integration-test/conf/nginz/integration-ca.pem"
SERVERNAME="federator.integration.example.com"
getHandle "local nginz on port 8443 using self-signed cert"

# HOST=
# PORT=443 # tls port of currently-deployed ingress in 'grpc' namespace
# MODE="-insecure"
# SERVERNAME="federator.integration.example.com"
# # making an insecure/ignore-certificates connection over TLS works:
# getHandle "description"
