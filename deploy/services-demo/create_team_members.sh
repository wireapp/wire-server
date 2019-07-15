#!/usr/bin/env bash

set -ex

ADMIN_UUID="a09e9521-e14e-4285-ad71-47caa97f4a16"
TEAM_UUID="9e57a378-0dca-468f-9661-7872f5f1c910"
BRIG_HOST="http://localhost:8082"

USAGE="
This bash script can be used to invite members to a given team.  Input
is a csv file with email addresses and suggested user names.

Note that this uses internal brig endpoints.  It is not exposed over
nginz and can only be used if you have direct access to brig.

USAGE: $0
    -a <admin uuid>: User ID of the inviting admin.  default: ${ADMIN_UUID}
    -t <team uuid>: ID of the inviting team.  default: ${TEAM_UUID}
    -h <host>: Base URI of brig. default: ${BRIG_HOST}
    -c: input file containing info on the invitees in format 'Email,UserName'.
"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":a:t:h:c" opt; do
  case ${opt} in
    a ) ADMIN_UUID="$OPTARG"
      ;;
    t ) TEAM_UUID="$OPTARG"
      ;;
    h ) BRIG_HOST="$OPTARG"
      ;;
    c ) if [ ! -e "$OPTARG" ]; then
            echo -e "\n\n*** I need the name of an existing cvs file.\n\n"
            echo "$USAGE" 1>&2
            exit 1
        else
            CSV_FILE="$OPTARG"
        fi
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

    ERR='{"code":409,"message":"The given e-mail address is in use.","label":"email-exists"}'
    if [[ "$INVITATION_ID" == "$ERR" ]]; then
      echo "User with the email $EMAIL already exists, aborting"
      exit 1
    fi

    echo "Sleeping 1 second..." 1>&2
    sleep 1
done < "$CSV_FILE"
