#!/usr/bin/env bash

set -e

#
# This bash script can be used to create an active user by using an internal 
# brig endpoint. Note that this is not exposed over nginz and can only be used
# if you have direct access to brig
#

# Usage:
#   --csv			Output users in CSV format
#   --count=INT		Generate several users (by default it's just one)

CSV=false
COUNT=1

# Parse CLI options

if ! [ $# -eq 0 ]; then
    TEMP=`getopt -o "" -l csv,count: -n 'create_test_user.sh' -- "$@"`
    eval set -- "$TEMP"

    while true ; do
        case "$1" in
            --csv) CSV=true ; shift ;;
            --count)
                case "$2" in
                    "") shift 2 ;;
                    *) COUNT=$2 ; shift 2 ;;
                esac ;;
            --) shift ; break ;;
            *) echo "Unrecognized option $1" ; exit 1 ;;
        esac
    done
fi

# Generate users

for i in `seq 1 $COUNT`
do
    EMAIL=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)
    PASSWORD=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)

    CURL_OUT=$(curl -i -s --show-error \
        -XPOST "http://localhost:8082/i/users" \
        -H'Content-type: application/json' \
        -d'{"email":"'$EMAIL'@example.com","password":"'$PASSWORD'","name":"demo"}')

    UUID=$(echo "$CURL_OUT" | tail -1 | sed 's/.*"id":"\([0-9a-z-]\+\)".*/\1/')

    if [ "$CSV" == "false" ]
        then echo -e "Succesfully created a user with email: "$EMAIL"@example.com and password: "$PASSWORD
        else echo -e $UUID","$EMAIL","$PASSWORD
    fi
done
