#!/usr/bin/env bash
#
# consider using create_team.py (you'll have to evolve it a little further to cover this use case, though)

set -e

ADMIN_UUID="a09e9521-e14e-4285-ad71-47caa97f4a16"
TEAM_UUID="9e57a378-0dca-468f-9661-7872f5f1c910"
BRIG_HOST="http://localhost:8082"
START="1"
COUNT="1"
CSV="false"
TARGET_EMAIL_DOMAIN=""

USAGE="This bash script can be used to create active members in a
given team.  Every member will have an email address of the form
'w<number>@${TARGET_EMAIL_DOMAIN}', and will have to change that
(after logging in with the password provided to the user from the
output of this script).

Note that this uses internal brig endpoints.  It is not exposed over
nginz and can only be used if you have direct access to brig.

USAGE: $0 -d <email domain> [OPTIONS...]
    -d <email domain>: Domain part of the emails that the bogus
                       invitations are sent to.  No default, you need
                       to provide that.  Consider 'example.com', or an
                       internal domain you control.

                       WARNING: This may boost your reputation as a
                       spammer.  Use with care!

    -a <admin uuid>: User ID of the inviting admin.  default: ${ADMIN_UUID}
    -t <team uuid>: ID of the inviting team.  default: ${TEAM_UUID}
    -s <S>: Start at offset. default: ${START}
    -n <N>: Create <N> users. default: ${COUNT}
    -h <host>: Base URI of brig. default: ${BRIG_HOST}
    -c: Output as headerless CSV in format 'User-Id,Email,Password'. default: ${CSV}
"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":a:t:s:n:h:d:c" opt; do
  case ${opt} in
    a ) ADMIN_UUID="$OPTARG"
      ;;
    t ) TEAM_UUID="$OPTARG"
      ;;
    s ) START="$OPTARG"
      ;;
    n ) COUNT="$OPTARG"
      ;;
    h ) BRIG_HOST="$OPTARG"
      ;;
    d ) TARGET_EMAIL_DOMAIN="$OPTARG"
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
fi

# Warn about sending emails

if [ "$TARGET_EMAIL_DOMAIN" == "" ]; then
    echo -e "\n\n*** Please provide an email domain if you want to run this script.\n\n"
    echo "$USAGE" 1>&2
    exit 1
fi

# Generate users
END=$((COUNT + START - 1))
for i in $(seq "$START" "$END")
do
    EMAIL='w'$(printf "%03d" "$i")"@$TARGET_EMAIL_DOMAIN"
    PASSWORD=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)

    # Generate the invitation

    CURL_OUT_INVITATION=$(curl -i -s --show-error \
        -XPOST "$BRIG_HOST/teams/$TEAM_UUID/invitations" \
        -H'Content-type: application/json' \
        -H'Z-User: '"$ADMIN_UUID"'' \
        -d'{"email":"'"$EMAIL"'","name":"Replace with name","inviter_name":"Team admin"}')

    INVITATION_ID=$(echo "$CURL_OUT_INVITATION" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')

    echo "Created the invitation, sleeping 1 second..." 1>&2
    sleep 1

    if ( ( echo "$INVITATION_ID" | grep -q '"code"' ) &&
         ( echo "$INVITATION_ID" | grep -q '"label"' ) ) ; then
      echo "Got an error while creating $EMAIL, aborting: $INVITATION_ID"
      exit 1
    fi

    # Get the code
    CURL_OUT_INVITATION_CODE=$(curl -i -s --show-error \
        -XGET "$BRIG_HOST/i/teams/invitation-code?team=$TEAM_UUID&invitation_id=$INVITATION_ID")

    INVITATION_CODE=$(echo "$CURL_OUT_INVITATION_CODE" | tail -1 | sed -n -e '/"code":/ s/^.*"\(.*\)".*/\1/p')

    echo "Got the code, sleeping 1 second..." 1>&2
    sleep 1

    # Create the user using that code
    CURL_OUT=$(curl -i -s --show-error \
            -XPOST "$BRIG_HOST/i/users" \
            -H'Content-type: application/json' \
            -d'{"email":"'"$EMAIL"'","password":"'"$PASSWORD"'","name":"demo","team_code":"'"$INVITATION_CODE"'"}')

    TEAM_MEMBER_UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')
    TEAM=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"team\":\"\([a-z0-9-]*\)\".*/\1/')

    if [ "$TEAM" != "$TEAM_UUID" ]; then
        echo "unexpected error: user got assigned to no / the wrong team?!"
        echo "${CURL_OUT}"
        exit 1
    fi

    if [ "$CSV" == "false" ]
        then echo -e "Succesfully created a team member: $TEAM_MEMBER_UUID on team: $TEAM_UUID with email: $EMAIL and password: $PASSWORD"
        else echo -e "$UUID,$EMAIL,$PASSWORD"
    fi

    echo "Sleeping 1 second..." 1>&2
    sleep 1
done
