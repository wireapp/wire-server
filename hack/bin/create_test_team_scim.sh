#!/usr/bin/env bash

set -e

BRIG_HOST="http://localhost:8082"
SPAR_HOST="http://localhost:8088"

USAGE="
This bash script creates
1) team
2) team admin
3) scim token
4) a regular user via team invitation
5) a scim-managed user (without IDP)

Note that this uses internal brig and spar endpoints. It is not exposed over
nginz and can only be used if you have direct access to brig and spar simultaneously.

USAGE: $0
    -h <host>: Base URI of brig. default: ${BRIG_HOST}
    -s <host>: Base URI of spar. default: ${SPAR_HOST}
"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":h:s:" opt; do
  case ${opt} in
    h ) BRIG_HOST="$OPTARG"
      ;;
    s ) SPAR_HOST="$OPTARG"
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


ADMIN_EMAIL=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)"@example.com"
ADMIN_PASSWORD=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)

CURL_OUT=$(curl -i -s --show-error \
    -XPOST "$BRIG_HOST/i/users" \
    -H'Content-type: application/json' \
    -d'{"email":"'"$ADMIN_EMAIL"'","password":"'"$ADMIN_PASSWORD"'","name":"demo","team":{"name":"Wire team","icon":"default"}}')

ADMIN_UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')
TEAM_UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"team\":\"\([a-z0-9-]*\)\".*/\1/')


BEARER=$(curl -X POST \
              --header 'Content-Type: application/json' \
              --header 'Accept: application/json' \
              -d '{"email":"'"$ADMIN_EMAIL"'","password":"'"$ADMIN_PASSWORD"'"}' \
              "$BRIG_HOST"/login'?persist=false' | jq -r .access_token)

SCIM_TOKEN_FULL=$(curl -X POST \
                       --header "Authorization: Bearer $BEARER" \
                       --header 'Content-Type: application/json;charset=utf-8' \
                       --header 'Z-User: '"$ADMIN_UUID" \
                       -d '{ "description": "test '"$(date)"'", "password": "'"$ADMIN_PASSWORD"'" }' \
                       "$SPAR_HOST/scim/auth-tokens")

SCIM_TOKEN=$(echo "$SCIM_TOKEN_FULL" | jq -r .token)
SCIM_TOKEN_ID=$(echo "$SCIM_TOKEN_FULL" | jq -r .info.id)


# Create regular user via team invitation

REGULAR_USER_EMAIL=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)"@example.com"
REGULAR_USER_PASSWORD=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)
CURL_OUT_INVITATION=$(curl -i -s --show-error \
                            -XPOST "$BRIG_HOST/teams/$TEAM_UUID/invitations" \
                            -H'Content-type: application/json' \
                            -H'Z-User: '"$ADMIN_UUID"'' \
                            -d'{"email":"'"$REGULAR_USER_EMAIL"'","name":"Replace with name","inviter_name":"Team admin"}')

INVITATION_ID=$(echo "$CURL_OUT_INVITATION" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')

sleep 1

if ( ( echo "$INVITATION_ID" | grep -q '"code"' ) &&
         ( echo "$INVITATION_ID" | grep -q '"label"' ) ) ; then
    echo "Got an error while creating $REGULAR_USER_EMAIL, aborting: $INVITATION_ID"
    exit 1
fi

sleep 1

if ( ( echo "$INVITATION_ID" | grep -q '"code"' ) &&
            ( echo "$INVITATION_ID" | grep -q '"label"' ) ) ; then
    echo "Got an error while creating $REGULAR_USER_EMAIL, aborting: $INVITATION_ID"
    exit 1
fi

# Get the code
CURL_OUT_INVITATION_CODE=$(curl -i -s --show-error \
                                -XGET "$BRIG_HOST/i/teams/invitation-code?team=$TEAM_UUID&invitation_id=$INVITATION_ID")

INVITATION_CODE=$(echo "$CURL_OUT_INVITATION_CODE" | tail -1 | sed -n -e '/"code":/ s/^.*"\(.*\)".*/\1/p')

sleep 1

# Create the user using that code
CURL_OUT=$(curl -i -s --show-error \
                -XPOST "$BRIG_HOST/i/users" \
                -H'Content-type: application/json' \
                -d'{"email":"'"$REGULAR_USER_EMAIL"'","password":"'"$REGULAR_USER_PASSWORD"'","name":"demo","team_code":"'"$INVITATION_CODE"'"}')

REGULAR_TEAM_MEMBER_UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*\"id\":\"\([a-z0-9-]*\)\".*/\1/')


# Create user via SCIM invitation


scimUserName=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)
scimUserDisplayName="Display of $scimUserName"
scimUserEmail="$scimUserName@example.com"
scimUserExternalId="$scimUserEmail"

SCIM_USER=$(cat <<EOF
{
    "UserName": "$scimUserName",
    "Active": true,
    "DisplayName": "$scimUserDisplayName",
    "schemas": [
        "urn:ietf:params:scim:schemas:core:2.0:User"
    ],
    "externalId": "$scimUserExternalId",
    "name": {
        "formatted": "Name of username $scimUserName",
        "familyName": "Family name of username $scimUserName"
    },
    "emails": [
        {
            "Primary": true,
            "type": "work",
            "value": "$scimUserEmail"
        }
    ]
}
EOF
)

CURL_OUT_SCIM_POST=$(curl --location --request POST "$SPAR_HOST/scim/v2/Users" \
    --header 'Content-Type: application/json' \
    --header "Authorization: Bearer $SCIM_TOKEN" \
    -d "$SCIM_USER")

SCIM_USER_UUID=$(echo "$CURL_OUT_SCIM_POST" | jq -r .id)

SCIM_USER_INVITATION_ID=$(curl --location -G "$BRIG_HOST/i/teams/invitations/by-email?" \
    --header 'Content-Type: application/json' \
    --header "Authorization: Bearer $SCIM_TOKEN" \
    -d "email=$scimUserEmail" | jq -r .id)

sleep 1

SCIM_USER_INVITATION_CODE=$(curl --silent --show-error \
                            --header 'Content-Type: application/json' \
                            -XGET "$BRIG_HOST/i/teams/invitation-code?team=$TEAM_UUID&invitation_id=$SCIM_USER_INVITATION_ID" | jq -r .code
                            )

scimUserPassword=$(env LC_CTYPE=C tr -dc a-zA-Z0-9 < /dev/urandom | head -c 8)

# Create the user using that code
CURL_OUT=$(curl \
            -XPOST "$BRIG_HOST/i/users" \
            -H'Content-type: application/json' \
            -d'{"email":"'"$scimUserEmail"'","password":"'"$scimUserPassword"'","name":"'"$scimUserDisplayName"'","team_code":"'"$SCIM_USER_INVITATION_CODE"'"}')

SCIM_USER_REGISTER_TEAM=$(echo "$CURL_OUT" | jq -r .team)

if [ "$SCIM_USER_REGISTER_TEAM" != "$TEAM_UUID" ]; then
    echo "unexpected error: user got assigned to no / the wrong team?!"
    echo "${CURL_OUT}"
    exit 1
fi

echo "Succesfully created:"
echo ""
echo "team: $TEAM_UUID"
echo ""
echo "admin: $ADMIN_UUID"
echo "admin email: $ADMIN_EMAIL"
echo "admin password: $ADMIN_PASSWORD"
echo ""
echo "scim token: $SCIM_TOKEN"
echo "scim token uuid: $SCIM_TOKEN_ID"
echo ""
echo "user 1 (via team invite)"
echo "user 1: $REGULAR_TEAM_MEMBER_UUID"
echo "user 1 email: $REGULAR_USER_EMAIL"
echo "user 1 password: $REGULAR_USER_PASSWORD"
echo ""
echo "user 2 (via SCIM + invite)"
echo "user 2: $SCIM_USER_UUID"
echo "user 2 handle: $scimUserName"
echo "user 2 email: $scimUserEmail"
echo "user 2 password: $scimUserPassword"
echo "user 2 externalId: $scimUserExternalId"
