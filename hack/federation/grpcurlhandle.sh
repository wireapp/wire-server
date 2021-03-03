#!/usr/bin/env bash

# Example request for handle lookup using grpcurl command line tool.
# expected output:
#
# requesting user lookup via directly calling federator...
# {
#   "httpResponse": {
#     "responseStatus": 404,
#     "responseBody": "SGFuZGxlIG5vdCBmb3VuZC4="
#   }
# }
# requesting user lookup via nginz forwarding to federator...
# {
#   "httpResponse": {
#     "responseStatus": 404,
#     "responseBody": "SGFuZGxlIG5vdCBmb3VuZC4="
#   }
# }
#

command -v grpcurl >/dev/null 2>&1 || { echo >&2 "grpcurl is not installed, aborting. Maybe try ' nix-env -iA nixpkgs.grpcurl '?"; exit 1; }

path=$(echo -n users/by-handle | base64)
queryK=$(echo -n handle | base64)
queryV=$(echo -n alice | base64)

# DIRECT=8098 # federator external port
# NGINZ=8090

function getHandle() {
    set -x
    grpcurl -d @ -format json -authority "$SERVERNAME" "$MODE" -proto ../../libs/wire-api-federation/proto/router.proto "$HOST:$VIA" wire.federator.Inward/call <<EOM
{"method": "GET", "component": "Brig", "path": "$path", "query": [{"key":"$queryK", "value":"$queryV"}]}
EOM
    set +x
}

HOST=88.99.188.44 # one 'anta' node
VIA=31063 # tls port of currently-deployed ingress in 'test-user' namespace
MODE="-insecure"
SERVERNAME="federator.integration.example.com"
# making an insecure/ignore-certificates connection over TLS works:
getHandle

# current certificate isn't trusted so this doesn't work
MODE=""
getHandle

# plaintext forwarding doesn't work as the controller only has one port for plain http and that is already taken and it's just an nginx, not magic, so it cannot distinguish between normal http traffic and grpc traffic on the same port so it strangely hangs and times out here:
VIA=31403
MODE="-plaintext"
SERVERNAME="federator.integration.example.com"
getHandle
