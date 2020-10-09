#!/usr/bin/env bash

set -eu

function create_team_and_scim_token {
    TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../.." && pwd )"

    IFS=',' read -r -a creds <<< $($TOP_LEVEL/deploy/services-demo/create_test_team_admins.sh -c)

    BRIG_HOST="http://$SCIM_TEST_SUITE_BRIG_HOST:$SCIM_TEST_SUITE_BRIG_PORT"
    WIRE_ADMIN_UUID=${creds[0]}
    WIRE_ADMIN=${creds[1]}
    WIRE_PASSWD=${creds[2]}

    export BEARER=$(curl -X POST \
                        --header 'Content-Type: application/json' \
                        --header 'Accept: application/json' \
                        -d '{"email":"'"$WIRE_ADMIN"'","password":"'"$WIRE_PASSWD"'"}' \
                        $BRIG_HOST/login'?persist=false' | jq -r .access_token)

    SPAR_HOST="http://$SCIM_TEST_SUITE_SPAR_HOST:$SCIM_TEST_SUITE_SPAR_PORT"

    export SCIM_TOKEN_FULL=$(curl -X POST \
        --header "Authorization: Bearer $BEARER" \
        --header 'Content-Type: application/json;charset=utf-8' \
        --header 'Z-User: '"$WIRE_ADMIN_UUID" \
        -d '{ "description": "test '"`date`"'", "password": "'"$WIRE_PASSWD"'" }' \
        $SPAR_HOST/scim/auth-tokens)

    export SCIM_TOKEN=$(echo $SCIM_TOKEN_FULL | jq -r .token)
    export SCIM_TOKEN_ID=$(echo $SCIM_TOKEN_FULL | jq -r .info.id)

    echo $SCIM_TOKEN
}

function create_env_file {
    token=$(create_team_and_scim_token)
    cat > /tmp/scim_test_suite_env.json <<EOF
{
  "id": "8b570933-354b-4be9-8b83-c6483a251909",
  "name": "Environment for SCIM Tests",
  "values":
    [
        {
            "key": "Server",
            "value": "$SCIM_TEST_SUITE_SPAR_HOST",
            "enabled": true
        },
        {
            "key": "Port",
            "value": ":$SCIM_TEST_SUITE_SPAR_PORT",
            "enabled": true
        },
        {
            "key": "Api",
            "value": "scim/v2",
            "enabled": true
        },
        {
            "key": "token",
            "value": "$token",
            "enabled": true
        }
    ]
}
EOF
}

create_env_file
newman run --environment /tmp/scim_test_suite_env.json "$@"
