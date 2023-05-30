#!/usr/bin/env bash

set -e

USAGE="This script tests the OAuth2 flow by creating a client, requesting an authorization code, and
then requesting an access token. It then uses the access token to make a request to /self.

Create a user first with './create_team.py 8082 -n 1'. Then use the user ID to call this script.

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
# Beware to not use hard coded values like these, anywhere but in test environments
CODE_VERIFIER="0LgRJptQI--6vQjlQfoXEM1GG4oSeN6ttESXVZRL6SEQAS6GuXW4X_FNkfp72BS9W157xZQKoJqXksj8C6UzO0.MQfgV3sdeOlg1XNSVlR50gYHfVM0A~qNyfZDmWFE8"
CODE_CHALLENGE="dc8qty_fbGDz4wfFPnvApmLfaP14uYDVkJ2tE8N0Xgk"

CLIENT=$(
  curl -s -X POST localhost:8082/i/oauth/clients \
    -H "Content-Type: application/json" \
    -d '{
      "application_name":"foobar",
      "redirect_url":"https://example.com"
    }'
)

CLIENT_ID=$(echo "$CLIENT" | jq -r '.client_id')

AUTH_CODE_RESPONSE=$(
  curl -i -s -X POST localhost:8082/oauth/authorization/codes \
    -H 'Z-User: '"$USER" \
    -H "Content-Type: application/json" \
    -d '{
      "client_id": "'"$CLIENT_ID"'",
      "scope": "'"$SCOPE"'",
      "response_type": "code",
      "redirect_uri": "https://example.com",
      "state": "foobar",
      "code_challenge": "'"$CODE_CHALLENGE"'",
      "code_challenge_method": "S256"
    }'
)

AUTH_CODE=$(echo "$AUTH_CODE_RESPONSE" | awk -F ': ' '/^Location/ {print $2}' | awk -F'[=&]' '{print $2}')

ACCESS_TOKEN_RESPONSE=$(
  curl -s -X POST localhost:8080/oauth/token \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d 'code='"$AUTH_CODE"'&client_id='"$CLIENT_ID"'&grant_type=authorization_code&redirect_uri=https://example.com&code_verifier='"$CODE_VERIFIER"
)

ACCESS_TOKEN=$(echo "$ACCESS_TOKEN_RESPONSE" | jq -r '.access_token')
REFRESH_TOKEN=$(echo "$ACCESS_TOKEN_RESPONSE" | jq -r '.refresh_token')

echo "client id    : $CLIENT_ID"
echo "scope        : $SCOPE"
echo "auth code    : $AUTH_CODE"
echo "access token : $ACCESS_TOKEN"

echo ""
echo "making a request to /self..."
curl -s -H 'Authorization: Bearer '"$ACCESS_TOKEN" -H "Content-Type: application/json" localhost:8080/self | jq

REFRESH_ACCESS_TOKEN_RESPONSE=$(
  curl -s -X POST localhost:8080/oauth/token \
    -H "Content-Type: application/x-www-form-urlencoded" \
    -d 'refresh_token='"$REFRESH_TOKEN"'&client_id='"$CLIENT_ID"'&grant_type=refresh_token'
)

NEW_ACCESS_TOKEN=$(echo "$REFRESH_ACCESS_TOKEN_RESPONSE" | jq -r '.access_token')

echo ""
echo "making a request to /self with a new access token ..."
curl -s -H 'Authorization: Bearer '"$NEW_ACCESS_TOKEN" -H "Content-Type: application/json" localhost:8080/self | jq

echo ""
echo "Getting list of OAuth apps with account access..."

curl -s -H 'Z-User: '"$USER" localhost:8082/oauth/applications | jq
