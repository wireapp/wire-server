#!/bin/bash

set -o pipefail
set -o errexit

cd "$(dirname "${BASH_SOURCE[0]}")"

echo "WIRE_BACKEND: $WIRE_BACKEND"
echo "WIRE_ADMIN: $WIRE_ADMIN"
echo "WIRE_PASSWD: <not shown>"

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
    fail "login: FAIL"
  fi
}

check_url() {
  export testnum=$1
  export verb=$2
  export uri=$3
  export status=$4

  status_code=$(curl --write-out '%{http_code}' --silent --output "./proxy-test/$testnum.txt" -I -X "$verb" \
                     --header "Authorization: Bearer $BEARER" \
                     --header "Content-Type: application/json" \
                     "$uri")
  if [[ "$status_code" == "$status" ]]; then
    echo "proxy $uri: OK"
  else
    echo "expected status code: $status, but got $status_code"
    fail "proxy $uri: FAIL"
  fi

  git diff "./proxy-test/$testnum.txt"  # TODO: fail if diff is non-empty
}

get_access_token() {
    BEARER=$(curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' \
                  -d '{"email":"'"$WIRE_ADMIN"'","password":"'"$WIRE_PASSWD"'"}' \
                  "$WIRE_BACKEND"/login'?persist=false' \
                 | jq -r .access_token)
}

get_access_token
check_login

check_url "1" "GET" "$WIRE_BACKEND"/api/swagger.json 200
check_url "2" "GET" "$WIRE_BACKEND"'/v2/proxy/youtube/v3/search' 200
check_url "3" "GET" "$WIRE_BACKEND"'/v2/proxy/googlemaps/api/staticmap?center=Berlin&zoom=14&size=400x400' 200
check_url "4" "GET" "$WIRE_BACKEND"'/v2/proxy/googlemaps/maps/api/geocode/json?place_id=ChIJeRpOeF67j4AR9ydy_PIzPuM' 200
check_url "5" "GET" "$WIRE_BACKEND"'/v2/proxy/giphy/v1/gifs/search?limit=100&offset=0&q=kitty' 200
check_url "6" "POST" "$WIRE_BACKEND"'/v2/proxy/spotify/api/token' 415
check_url "7" "GET" "$WIRE_BACKEND"'/v2/proxy/soundcloud/resolve' 400
check_url "8" "GET" "$WIRE_BACKEND"'/v2/proxy/soundcloud/stream' 400

# manually:
# curl -XGET http://localhost:8080/i/status  # from proxy pod
# curl -XHEAD http://localhost:8080/i/status  # from proxy pod
