#!/bin/bash

echo "WIRE_BACKEND: $WIRE_BACKEND"
echo "WIRE_ADMIN: $WIRE_ADMIN"

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
  status_code=$(curl --write-out '%{http_code}' --silent --output /dev/null -I -X "$1" --header "Authorization: Bearer $BEARER" "$2")
  if [[ "$status_code" == "$3" ]]; then
    echo "proxy $2: OK"
  else
    echo "expected status code: $3, but got $status_code"
    fail "proxy $2: FAIL"
  fi
}

get_access_token() {
  BEARER=$(curl -X POST --header 'Content-Type: application/json' --header 'Accept: application/json' -d '{"email":"'"$WIRE_ADMIN"'","password":"'"$WIRE_PASSWD"'"}' "$WIRE_BACKEND"/login'?persist=false' | jq -r .access_token)
}

get_access_token
check_login
check_url "GET" "$WIRE_BACKEND"'/v2/proxy/youtube/v3/search' 200
check_url "GET" "$WIRE_BACKEND"'/v2/proxy/googlemaps/api/staticmap?center=Berlin&zoom=14&size=400x400' 200
check_url "GET" "$WIRE_BACKEND"'/v2/proxy/googlemaps/maps/api/geocode/json?place_id=ChIJeRpOeF67j4AR9ydy_PIzPuM' 200
check_url "GET" "$WIRE_BACKEND"'/v2/proxy/giphy/v1/gifs/search?limit=100&offset=0&q=kitty' 403
check_url "POST" "$WIRE_BACKEND"'/v2/proxy/spotify/api/token' 400
check_url "GET" "$WIRE_BACKEND"'/v2/proxy/soundcloud/resolve' 400
check_url "GET" "$WIRE_BACKEND"'/v2/proxy/soundcloud/stream' 400
