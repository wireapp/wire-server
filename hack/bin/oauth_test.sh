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

SCOPE="read:self"

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

ACCESS_TOKEN_RESPONSE=$(
  curl -s -X POST localhost:8080/oauth/token \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d 'code='"$AUTH_CODE"'&client_id='"$CLIENT_ID"'&grant_type=authorization_code&redirect_uri=https://example.com&client_secret='"$CLIENT_SECRET"
)

echo "$ACCESS_TOKEN_RESPONSE" | jq

ACCESS_TOKEN=$(echo "$ACCESS_TOKEN_RESPONSE" | jq -r '.accessToken')
REFRESH_TOKEN=$(echo "$ACCESS_TOKEN_RESPONSE" | jq -r '.refreshToken')

echo "client id    : $CLIENT_ID"
echo "client secret: $CLIENT_SECRET"
echo "scope        : $SCOPE"
echo "auth code    : $AUTH_CODE"
echo "access token : $ACCESS_TOKEN"

echo ""
echo "making a request to /self..."
curl -s -H 'Authorization: Bearer '"$ACCESS_TOKEN" -H "Content-Type: application/json" localhost:8080/self | jq

REFRESH_ACCESS_TOKEN_RESPONSE=$(
  curl -s -X POST localhost:8080/oauth/token \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d 'refresh_token='"$REFRESH_TOKEN"'&client_id='"$CLIENT_ID"'&grant_type=refresh_token&client_secret='"$CLIENT_SECRET"
)

NEW_ACCESS_TOKEN=$(echo "$REFRESH_ACCESS_TOKEN_RESPONSE" | jq -r '.accessToken')

echo ""
echo "making a request to /self with a new access token ..."
curl -s -H 'Authorization: Bearer '"$NEW_ACCESS_TOKEN" -H "Content-Type: application/json" localhost:8080/self | jq

echo ""
echo "Getting list of OAuth apps with account access..."

curl -s -H 'Z-User: '"$USER" localhost:8082/oauth/applications | jq
