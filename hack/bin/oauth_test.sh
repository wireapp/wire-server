#!/usr/bin/env bash

set -e

USAGE="This script tests the OAuth2 flow by creating a client, requesting an authorization code, and
then requesting an access token. It then uses the access token to make a request to /self.

Create a user first with './create_test_user.sh -n 1 -c'. Then use the user ID to call this script.

USAGE: $0
    -u <user_id>: User ID
"

unset -v USER

while getopts ":u:" opt; do
  case ${opt} in
  u)
    USER="$OPTARG"
    ;;
  \?)
    echo "$USAGE" 1>&2
    exit 1
    ;;
  :)
    echo "-$OPTARG" requires an argument 1>&2
    exit 1
    ;;
  esac
done
shift $((OPTIND - 1))

if [ -z "$USER" ]; then
  echo 'missing option -u <user_id>' 1>&2
  echo "$USAGE" 1>&2
  exit 1
fi

SCOPE="self:read"

CLIENT=$(
  curl -s -X POST localhost:8082/i/oauth/clients \
    -H "Content-Type: application/json" \
    -d '{
      "applicationName":"foobar",
      "redirectUrl":"https://example.com"
    }'
)

CLIENT_ID=$(echo "$CLIENT" | jq -r '.clientId')
CLIENT_SECRET=$(echo "$CLIENT" | jq -r '.clientSecret')

AUTH_CODE=$(
  curl -i -s -X POST localhost:8082/oauth/authorization/codes \
    -H 'Z-User: '"$USER" \
    -H "Content-Type: application/json" \
    -d '{
      "clientId": "'"$CLIENT_ID"'",
      "scope": "'"$SCOPE"'",
      "responseType": "code",
      "redirectUri": "https://example.com",
      "state": "foobar"
    }' |
    awk -F ': ' '/^Location/ {print $2}' | awk -F'[=&]' '{print $2}'
)

ACCESS_TOKEN=$(
  curl -s -X POST localhost:8082/oauth/token \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d 'code='"$AUTH_CODE"'&client_id='"$CLIENT_ID"'&grant_type=authorization_code&redirect_uri=https://example.com&client_secret='"$CLIENT_SECRET" |
    jq -r '.accessToken'
)

echo "client id    : $CLIENT_ID"
echo "client secret: $CLIENT_SECRET"
echo "scope        : $SCOPE"
echo "auth code    : $AUTH_CODE"
echo "access token : $ACCESS_TOKEN"

echo ""
echo "making a request to /self..."
curl -s -H 'Z-OAUTH: Bearer '"$ACCESS_TOKEN" -H "Content-Type: application/json" localhost:8082/self | jq .
