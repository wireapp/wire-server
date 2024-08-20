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
    -n <N>:  Create <N> users. default: 1
    -h <host>: Base URI of brig. default: http://localhost:8082
    -c: Output as headerless CSV in format 'User-Id,Email,Password'. default: false
"

BRIG_HOST="http://localhost:8082"
COUNT="1"
CSV="false"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":n:h:c" opt; do
  case ${opt} in
    n ) COUNT="$OPTARG"
      ;;
    h ) BRIG_HOST="$OPTARG"
      ;;
    c ) CSV="true"
      ;;
    : ) echo "-$OPTARG" requires an argument 1>&2
        exit 1
      ;;
    \? ) echo "$USAGE" 1>&2
         exit 1
      ;;
  esac
done
shift $((OPTIND -1))

if [ "$#" -ne 0 ]; then
  echo "$USAGE" 1>&2
  exit 1
fi;

# Generate users

# shellcheck disable=SC2034
for i in $(seq 1 "$COUNT")
do
    EMAIL=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)"@example.com"
    PASSWORD=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)

    CURL_OUT=$(curl -i -s --show-error \
        -XPOST "$BRIG_HOST/i/users" \
        -H'Content-type: application/json' \
        -d'{"email":"'"$EMAIL"'","password":"'"$PASSWORD"'","name":"demo"}')

    UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')

    if [ "$CSV" == "false" ]
        then echo -e "Succesfully created a user with email: ""$EMAIL"" and password: ""$PASSWORD"
        else echo -e "$UUID"",""$EMAIL"",""$PASSWORD"
    fi
done
