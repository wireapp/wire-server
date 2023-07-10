#! /usr/bin/env bash

# This can be used to run the script from within the wire-server repo, at the
# git root directory.
# source .envrc

RABBIT_HOST="172.24.0.10" # This is the IP of your running rabbit host. Get it from docker
RABBIT_USER="guest"
RABBIT_PASS="alpaca-grapefruit"

# These need to match one of the domains in the local testing environment.
OWN_DOMAIN="b.example.com"
TARGET_DOMAIN="example.com"

# This is a successful payload where background worker doesn't need to retry delivery to federator
# payload='{ "ownDomain":"'"$OWN_DOMAIN"'", "targetComponent":"brig", "path":"/search-users", "body":"{ \"term\": \"foo\" }"}'
#
# This is an unsuccessful payload where federator will never return a successful acknowledgement.
# payload='{"ownDomain":"'"$OWN_DOMAIN"'", "targetComponent":"brig", "path":"/on-user-deleted-connections", "body":"{\"claimant\": \"e84785b6-58f8-4e61-939f-87d2513e6970\", \"target\": \"1130a19c-9d12-4997-9efc-040c38d8e186\"}"}'

while true
do
  rabbitmqadmin \
    --host="$RABBIT_HOST" \
    --user="$RABBIT_USER" \
    --password="$RABBIT_PASS" \
    publish \
    routing_key="backend-notifications.$TARGET_DOMAIN" \
    payload='{ "ownDomain":"'"$OWN_DOMAIN"'", "targetComponent":"brig", "path":"/search-users", "body":"{ \"term\": \"foo\" }"}'
  # Gives a rough idea of how fast messages are being sent.
  date -Ins
done