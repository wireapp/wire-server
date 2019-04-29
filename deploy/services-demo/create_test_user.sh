#!/usr/bin/env bash

set -e

#
# This bash script can be used to create an active user by using an internal
# brig endpoint. Note that this is not exposed over nginz and can only be used
# if you have direct access to brig
#

USAGE="USAGE: $0 <brig-host e.g. http://localhost:8082> <num-users to create e.g. 100>
Outputs a headerless CSV of:
User-Id,Email,Password"

if [[ $# -ne 2 ]]; then
    echo "$USAGE" 1>&2
    exit 1
fi

BRIG_HOST="$1"
COUNT="$2"

# Generate users

for i in `seq 1 $COUNT`
do
    EMAIL=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)"@example.com"
    PASSWORD=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)

    CURL_OUT=$(curl -i -s --show-error \
        -XPOST "$BRIG_HOST/i/users" \
        -H'Content-type: application/json' \
        -d'{"email":"'$EMAIL'","password":"'$PASSWORD'","name":"demo"}')

    UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')

    echo -e "$UUID,$EMAIL,$PASSWORD"
done
