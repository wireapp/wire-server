#!/usr/bin/env bash

set -e

#
# This bash script can be used to register an idp service with a team
# over the public backend API.
#
# Usage:
#   register_idp.sh idp-metadata.xml
#   WIRE_TRACE=1 WIRE_BACKEND=our-wire-backend.example.com register_idp.sh idp-metadata.xml
#
# the script will prompt you for your wire login and password.  your
# user needs to be a team admin, and the idp will be registered with
# that team.
#

metadata_file=$1
if [ ! -e "$metadata_file" ]; then
    echo "*** no metadata: '$1'"
    exit 80
fi

if [ -n "$WIRE_BACKEND" ]; then
    backend="$WIRE_BACKEND"
else
    backend="localhost:8080"
fi

if [ "$WIRE_TRACE" == "1" ]; then
    trace="1"
fi

command -v curl >/dev/null || ( echo "*** please install https://curl.haxx.se/ in your path."; exit 81 )
curl_exe=$(command -v curl)

command -v jq >/dev/null || ( echo "*** please install https://stedolan.github.io/jq/ in your path."; exit 82 )
jq_exe=$(command -v jq)

# login
if [ -n "$WIRE_LOGIN" ]; then
    login="$WIRE_LOGIN"
else
    echo -n "login email: "
    read -r login
fi

if [ -n "$WIRE_PASSWORD" ]; then
    password="$WIRE_PASSWORD"
else
    echo -n "password: "
    stty -echo; read -r password; stty echo; echo
fi

payload="{\"email\":\"$login\",\"password\":\"$password\"}"
test -n "$trace" && echo "$curl_exe -is --show-error -XPOST \"https://$backend/login\" -H'Content-type: application/json' -d\"$payload\""
access_token=$($curl_exe -s --show-error -XPOST "https://$backend/login" -H'Content-type: application/json' -d"$payload" | $jq_exe -r .access_token)

# register idp
test -n "$trace" && echo "$curl_exe -is --show-error -XPOST \"https://$backend/identity-providers\" -H\"Authorization: Bearer $access_token\" -H'Content-type: application/xml' -d@\"$metadata_file\""
$curl_exe -is --show-error -XPOST "https://$backend/identity-providers" -H"Authorization: Bearer $access_token" -H'Content-type: application/xml' -d@"$metadata_file"
