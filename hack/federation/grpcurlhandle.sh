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

DIRECT=8098 # federator external port
NGINZ=8090

function getHandle() {
    grpcurl -d @ -format json -plaintext -proto ../../libs/wire-api-federation/proto/router.proto "localhost:$VIA" wire.federator.Inward/call <<EOM
{"method": "GET", "component": "Brig", "path": "$path", "query": [{"key":"$queryK", "value":"$queryV"}]}
EOM
}

VIA=$DIRECT

echo "requesting user lookup via directly calling federator..."
getHandle

VIA=$NGINZ

echo "requesting user lookup via nginz forwarding to federator..."
getHandle
