#!/usr/bin/env bash

set -e

ADMIN_UUID="n/a"
TEAM_UUID="n/a"
BRIG_HOST="http://localhost:8080"
CSV_FILE="n/a"

USAGE="
This bash script can be used to invite members to a given team.  Input
is a csv file with email addresses and suggested user names.

Note that this uses internal brig endpoints.  It is not exposed over
nginz and can only be used if you have direct access to brig.

USAGE: $0
    -a <admin uuid>: User ID of the inviting admin.  default: ${ADMIN_UUID}
    -t <team uuid>: ID of the inviting team.  default: ${TEAM_UUID}
    -h <host>: Base URI of brig. default: ${BRIG_HOST}
    -c <input file>: file containing info on the invitees in format 'Email,UserName'.  default: ${CSV_FILE}
"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":a:t:h:c:" opt; do
  case ${opt} in
    a ) ADMIN_UUID="$OPTARG"
      ;;
    t ) TEAM_UUID="$OPTARG"
      ;;
    h ) BRIG_HOST="$OPTARG"
      ;;
    c ) CSV_FILE="$OPTARG"
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

if [ ! -e "$CSV_FILE" ]; then
    echo -e "\n\n*** I need the name of an existing csv file.\n\n"
    echo "$USAGE" 1>&2
    exit 1
fi

# Generate users
while IFS=, read -r EMAIL USER_NAME
do
    echo "inviting $USER_NAME <$EMAIL>..." 1>&2

    # Generate the invitation
    CURL_OUT_INVITATION=$(curl -i -s --show-error \
        -XPOST "$BRIG_HOST/teams/$TEAM_UUID/invitations" \
        -H'Content-type: application/json' \
        -H'Z-User: '"$ADMIN_UUID"'' \
        -d'{"email":"'"$EMAIL"'","name":"'"$USER_NAME"'","inviter_name":"Team admin"}')

    INVITATION_ID=$(echo "$CURL_OUT_INVITATION" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')

    echo "Created the invitation, sleeping 1 second..." 1>&2
    sleep 1

    if ( ( echo "$INVITATION_ID" | grep -q '"code"' ) &&
         ( echo "$INVITATION_ID" | grep -q '"label"' ) ) ; then
      echo "Got an error, aborting: $INVITATION_ID"
      exit 1
    fi

    echo "Sleeping 1 second..." 1>&2
    sleep 1
done < "$CSV_FILE"
