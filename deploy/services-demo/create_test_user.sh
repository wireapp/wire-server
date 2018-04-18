#!/usr/bin/env bash

#
# This bash script can be used to create an active user by using an internal 
# brig endpoint. Note that this is not exposed over nginz and can only be used
# if you have direct access to brig
#

EMAIL=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)
PASSWORD=$(cat /dev/urandom | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 8)

curl -i -XPOST "http://localhost:8082/i/users" \
		-H'Content-type: application/json' \
		-d'{"email":"'$EMAIL'@example.com","password":"'$PASSWORD'","name":"demo"}' 

echo -e "\n\nSuccesfully created a user with email: "$EMAIL"@example.com and password: "$PASSWORD
