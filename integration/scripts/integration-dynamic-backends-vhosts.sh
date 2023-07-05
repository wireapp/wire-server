#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1

echo 'Creating RabbitMQ resources'

curl -u "$RABBITMQ_USERNAME":"$RABBITMQ_PASSWORD" -X PUT "$ENDPOINT_URL"/d1.default.domain
curl -u "$RABBITMQ_USERNAME":"$RABBITMQ_PASSWORD" -X PUT "$ENDPOINT_URL"/d2.default.domain
curl -u "$RABBITMQ_USERNAME":"$RABBITMQ_PASSWORD" -X PUT "$ENDPOINT_URL"/d3.default.domain

echo 'RabbitMQ resources created successfully!'

