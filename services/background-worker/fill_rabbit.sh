#! /usr/bin/env bash

# This script is will fill a local dockerised RabbitMQ instance with messages for background worker.
# It can be modified to send any message, and two messages are provided. One will succeed in local testing
# and the other will fail when being delivered from background-worker.

# Automagically figure out where RabbitMQ is available.
# The RabbitMQ management interface is available on the same address at port 15672, with the same credentials.
# This interface can be used to purge message queues. This is useful when testing valid and invalid messages.
RABBIT_HOST="$(docker inspect rabbitmq | grep IPAddress | grep -Po "(\d{1,3}\.){3}(\d{1,3})")" # This is the IP of your running rabbit host. Get it from docker
# These values come from .envrc. Since they are already
# exposed in the repo I'm not worried about repeating it here.
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
  # Gives a rough idea of how fast messages are being sent and that it isn't hanging
  date -Ins
done