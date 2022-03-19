#!/usr/bin/env bash

set -o pipefail
set -o errexit

BRIG_HOST="http://localhost:8080"
OWNER_NAME="owner name n/a"
OWNER_EMAIL="owner email n/a"
OWNER_PASSWORD="owner pass n/a"
EMAIL_CODE="email code n/a"
TEAM_NAME="team name n/a"
TEAM_CURRENCY="USD"

USAGE="
tbd.
"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":o:e:p:v:t:c:h:" opt; do
  case ${opt} in
    o ) OWNER_NAME="$OPTARG"
      ;;
    e ) OWNER_EMAIL="$OPTARG"
      ;;
    p ) OWNER_PASSWORD="$OPTARG"
      ;;
    v ) EMAIL_CODE="$OPTARG"
      ;;
    t ) TEAM_NAME="$OPTARG"
      ;;
    c ) TEAM_CURRENCY="$OPTARG"
      ;;
    h ) BRIG_HOST="$OPTARG"
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
fi

curl -i -s --show-error \
     -XPOST "$BRIG_HOST/register" \
        -H'Content-type: application/json' \
        -d'{"name":"'"$OWNER_NAME"'","email":"'"$OWNER_EMAIL"'","password":"'"$OWNER_PASSWORD"'","email_code":"'"$EMAIL_CODE"'","team":{"currency":"'"$TEAM_CURRENCY"'","icon":"default","name":"'"$TEAM_NAME"'"}}'
