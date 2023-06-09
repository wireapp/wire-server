#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1

echo 'Creating RabbitMQ resources'

curl -u "$RABBITMQ_USERNAME":"$RABBITMQ_PASSWORD" -X PUT "$ENDPOINT_URL"/c.example.com
curl -u "$RABBITMQ_USERNAME":"$RABBITMQ_PASSWORD" -X PUT "$ENDPOINT_URL"/d.example.com
curl -u "$RABBITMQ_USERNAME":"$RABBITMQ_PASSWORD" -X PUT "$ENDPOINT_URL"/e.example.com

echo 'RabbitMQ resources created successfully!'

