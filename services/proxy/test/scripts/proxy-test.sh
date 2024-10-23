#!/bin/bash

set -o pipefail
set -o errexit

cd "$(dirname "${BASH_SOURCE[0]}")"

echo "
run this script to test proxy on any running wire-server
instance.  this replaces more thorough integration tests, since
integration tests for just proxy without the proxied services
installed is hard and inadequate.

WIRE_BACKEND: $WIRE_BACKEND  (do not append a / to host:port!)

WIRE_ADMIN: $WIRE_ADMIN
WIRE_PASSWD: <not shown>
"

set -x

fail() {
  printf "\e[31;1m%s\e[0m\n" "$*" >&2
  exit 1
}

check_login() {
  echo "checking login..."
  status_code=$(curl --write-out '%{http_code}' --silent --output /dev/null -I -X GET --header "Authorization: Bearer $BEARER" "$WIRE_BACKEND"/self)

  if [[ "$status_code" == 200 ]]; then
    echo "login: OK"
  else
    echo "status code: $status_code"
    echo "this may be because your password contains special characters that would need to be quoted better in this script."
    fail "login: FAIL"
  fi
}

check_url() {
  export testnum=$1
  export verb=$2
  export uri=$3
  export status_want=$4

  status_have=$(curl --write-out '%{http_code}' --silent --output "./proxy-test/$testnum.txt" -I -X "$verb" \
                     --header "Authorization: Bearer $BEARER" \
                     --header "Content-Type: application/json" \
                     "$uri")

  curl -X "$verb" \
       --header "Authorization: Bearer $BEARER" \
       --header "Content-Type: application/json" \
       "$uri" > ./proxy-test/"$testnum".json

  if [[ "$status_have" == "$status_want" ]]; then
    echo "proxy $uri: OK"
    file ./proxy-test/"$testnum".json | grep -q '\(JSON\|PNG\)' || ( echo "received something weird!"; exit 1 )
  else
    echo "expected status code: $status_want, but got $status_have"
    fail "proxy $uri: FAIL (check ./proxy-test/$testnum.json for details)"
  fi
}

get_access_token() {
    BEARER=$(curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' \
                  -d '{"email":"'"$WIRE_ADMIN"'","password":"'"$WIRE_PASSWD"'"}' \
                  "$WIRE_BACKEND"/login'?persist=false' \
                 | jq -r .access_token)
}


mkdir -p ./proxy-test

get_access_token
check_login

check_url "1" "GET" "$WIRE_BACKEND"/api/swagger.json 200
check_url "2" "GET" "$WIRE_BACKEND"'/v2/proxy/giphy/v1/gifs/search?limit=100&offset=0&q=kitty' 200
check_url "3" "GET" "$WIRE_BACKEND"'/v2/proxy/youtube/v3/search' 200
check_url "4" "GET" "$WIRE_BACKEND"'/v2/proxy/googlemaps/api/staticmap?center=Berlin&zoom=14&size=400x400' 200
check_url "5" "GET" "$WIRE_BACKEND"'/v2/proxy/googlemaps/maps/api/geocode/json?place_id=ChIJeRpOeF67j4AR9ydy_PIzPuM' 200

# manually:
# curl -XGET http://localhost:8080/i/status  # from proxy pod
# curl -XHEAD http://localhost:8080/i/status  # from proxy pod
