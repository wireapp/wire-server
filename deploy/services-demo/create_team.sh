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
Request a code to create a team.  Call ./create_test_team_members.sh
first, then use the code you will receive by email to call this script.

USAGE: $0 -h <host> -o <owner_name> -e <owner_email> -p <owner_password> -v <email_code> -t <team_name> -c <team_currency>
    -h <host>: Base URI of brig. default: ${BRIG_HOST}
    -o <owner_name>: user display name of the owner of the team to be created.  default: ${OWNER_NAME}
    -e <owner_email>: email address of the owner of the team to be created.  default: ${OWNER_EMAIL}
    -p <owner_password>: owner password.  default: ${OWNER_PASSWORD}
    -v <email_code>: validation code received by email.  default: ${EMAIL_CODE}
    -t <team_name>: default: ${TEAM_NAME}
    -c <team_currency>: default: ${TEAM_CURRENCY}

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
