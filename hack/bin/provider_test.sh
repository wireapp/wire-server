#!/usr/bin/env bash
#
# consider using create_team.py

set -e

#
# This bash script can be used to create an active user by using an internal
# brig endpoint. Note that this is not exposed over nginz and can only be used
# if you have direct access to brig
#

USAGE="USAGE: $0
    -h <host>: Base URI of nginz. default: http://localhost:8080
    -b <host>: Base URI of brig. default: http://localhost:8082
"

NGINZ_HOST="https://staging-nginz-https.zinfra.io"
BRIG_HOST="http://localhost:18080"
CARGOHOLD_HOST="http://localhost:8084"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":h:b" opt; do
  case ${opt} in
  h)
    NGINZ_HOST="$OPTARG"
    ;;
  b)
    BRIG_HOST="$OPTARG"
    ;;
  :)
    echo "-$OPTARG" requires an argument 1>&2
    exit 1
    ;;
  \?)
    echo "$USAGE" 1>&2
    exit 1
    ;;
  esac
done
shift $((OPTIND - 1))

if [ "$#" -ne 0 ]; then
  echo "$USAGE" 1>&2
  exit 1
fi

# Generate users

EMAIL=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)"@example.com"
PASSWORD=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)

CURL_OUT=$(curl -i -s --show-error \
  -XPOST "$BRIG_HOST/v5/provider/register" \
  -H'Content-type: application/json' \
  -H'Z-USER: 8e57852e-8fb0-47b6-835a-7175c525f176' \
  -d'{"email":"'"$EMAIL"'","password":"'"$PASSWORD"'","name":"test_provider","url":"https://example.com","description":"test provider description"}')

UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')

echo "$CURL_OUT"

ACTIVATION_CODE=$(curl -s --show-error \
  "$BRIG_HOST/i/provider/activation-code?email=$EMAIL" \
  -H'Content-type: application/json')

echo "$ACTIVATION_CODE"

# extract key and code from the response
KEY=$(echo "$ACTIVATION_CODE" | jq -r '.key')
CODE=$(echo "$ACTIVATION_CODE" | jq -r '.code')

echo "KEY: $KEY"
echo "CODE: $CODE"

curl -i --show-error \
  "$BRIG_HOST/v5/provider/activate?key=$KEY&code=$CODE"

LOGIN_RESPONSE=$(curl -i --show-error \
  -XPOST "$BRIG_HOST/v5/provider/login" \
  -H'Content-type: application/json' \
  -d'{"email":"'"$EMAIL"'","password":"'"$PASSWORD"'"}')

COOKIE=$(echo "$LOGIN_RESPONSE" | grep -i 'set-cookie' | sed 's/Set-Cookie: \(.*\);.*/\1/')

echo "COOKIE: $COOKIE"
echo "UUID: $UUID"
curl -i --show-error \
  -XPOST "$NGINZ_HOST/v5/provider/assets" \
  -H'Content-Type: multipart/mixed; boundary=frontier' \
  -H'Cookie: '"$COOKIE"'' \
  --data-binary @- <<EOF
--frontier
Content-Type: application/json
Content-Length: 32

{"public":true,"retention":null}
--frontier
Content-Type: text/plain
Content-Length: 11

profile pic

--frontier--
EOF
