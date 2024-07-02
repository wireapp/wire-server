#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1
DOMAIN=$2

echo 'Creating RabbitMQ resources'

curl --cacert /certs/rabbitmq-ca/ca.pem -u "$RABBITMQ_USERNAME:$RABBITMQ_PASSWORD" -X PUT "$ENDPOINT_URL/$DOMAIN"

echo "RabbitMQ vhost created successfully for $DOMAIN"
