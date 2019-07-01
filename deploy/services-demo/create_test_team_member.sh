#!/usr/bin/env bash

set -ex

#
# This bash script can be used to create an active in a team by using an internal
# brig endpoint. Note that this is not exposed over nginz and can only be used
# if you have direct access to brig
#

USAGE="USAGE: $0
    -s <S>:  Start at offset. default: 1
    -n <N>:  Create <N> users. default: 1
    -h <host>: Base URI of brig. default: http://localhost:8082
    -c: Output as headerless CSV in format 'User-Id,Email,Password'. default: false
"

ADMIN_UUID="a09e9521-e14e-4285-ad71-47caa97f4a16"
TEAM_UUID="9e57a378-0dca-468f-9661-7872f5f1c910"
BRIG_HOST="http://localhost:8082"
START="1"
COUNT="1"
CSV="false"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":s:n:h:c" opt; do
  case ${opt} in
    s ) START="$OPTARG"
      ;;
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
END=`expr $COUNT + $START - 1`
for i in `seq $START $END`
do
    # EMAIL=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)"@example.com"
    EMAIL='w'$(printf "%03d" $i)"@example.com"
    PASSWORD=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)

    # Generate the invitation

    CURL_OUT_INVITATION=$(curl -i -s --show-error \
        -XPOST "$BRIG_HOST/teams/$TEAM_UUID/invitations" \
        -H'Content-type: application/json' \
        -H'Z-User: '"$ADMIN_UUID"'' \
        -d'{"email":"'$EMAIL'","name":"Replace with name","inviter_name":"Team admin"}')

    INVITATION_ID=$(echo "$CURL_OUT_INVITATION" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')

    sleep 1;
    echo "Created the invitation, sleeping 1 second...";

    ERR='{"code":409,"message":"The given e-mail address is in use.","label":"email-exists"}'
    if [[ "$INVITATION_ID" == "$ERR" ]]; then
      echo "User with the email $EMAIL already exists, aborting"
      exit 1
    fi;

    # Get the code
    CURL_OUT_INVITATION_CODE=$(curl -i -s --show-error \
        -XGET "$BRIG_HOST/i/teams/invitation-code?team=$TEAM_UUID&invitation_id=$INVITATION_ID")

    INVITATION_CODE=$(echo "$CURL_OUT_INVITATION_CODE" | tail -1 | sed -n -e '/"code":/ s/^.*"\(.*\)".*/\1/p')

    sleep 1;
    echo "Got the code, sleeping 1 second...";

    # Create the user using that code
    CURL_OUT=$(curl -i -s --show-error \
            -XPOST "$BRIG_HOST/i/users" \
            -H'Content-type: application/json' \
            -d'{"email":"'$EMAIL'","password":"'$PASSWORD'","name":"demo","team_code":"'$INVITATION_CODE'"}')

    TEAM_MEMBER_UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')
    TEAM=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"team\":\"\([a-z0-9-]*\)\".*/\1/')

    if [ "$CSV" == "false" ]
        then echo -e "Succesfully created a team member: "$TEAM_MEMBER_UUID" on team: "$TEAM_UUID" with email: "$EMAIL" and password: "$PASSWORD
        else echo -e $UUID","$EMAIL","$PASSWORD
    fi
    sleep 1;
    echo "Sleeping 1 second...";
done
