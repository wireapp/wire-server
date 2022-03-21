#!/usr/bin/env bash

set -o pipefail
set -o errexit

BRIG_HOST="http://localhost:8080"
OWNER_EMAIL="owner email n/a"

USAGE="
Request a code to create a team.  Call this script first, then use the
code you will receive by email to call ./create_team.sh

USAGE: $0 -h <host> -e <email>
    -h <host>: Base URI of brig. default: ${BRIG_HOST}
    -e <email>: email address of the owner of the team to be created.  default: ${OWNER_EMAIL}

"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":e:h:" opt; do
  case ${opt} in
    e ) OWNER_EMAIL="$OPTARG"
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
     -XPOST "$BRIG_HOST/activate/send" \
        -H'Content-type: application/json' \
        -d'{"email":"'"$OWNER_EMAIL"'"}'
