#!/usr/bin/env bash

set -e

# server-side variant of ./register_idp.sh; use if you have ssh access to one of your spar instances.
# usage: ./register_idp_internal.sh <admin id> <metadata file>

backend="http://localhost:8080"

metadata_file=$1
if [ ! -e "${metadata_file}" ]; then
    echo "*** no metadata: '$1'"
    exit 80
fi

z_user=$2
if [ -z "${z_user}" ]; then
    echo "*** no z_user uuid"
    exit 80
fi

which curl >/dev/null || ( echo "*** please install https://curl.haxx.se/ in your path."; exit 81 )
curl_exe=$(which curl)

${curl_exe} -is -v -XPOST ${backend}/identity-providers -H"Z-User: ${z_user}" -H'Content-type: application/xml' -d@"${metadata_file}"
