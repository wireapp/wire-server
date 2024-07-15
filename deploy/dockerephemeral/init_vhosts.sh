#!/usr/bin/env bash

set -euo pipefail

exec_until_ready() {
        until $1; do echo 'service not ready yet'; sleep 1; done
}

create_vhost() {
        exec_until_ready "curl -u $RABBITMQ_USERNAME:$RABBITMQ_PASSWORD -X PUT http://rabbitmq:15672/api/vhosts/$1"
}

echo 'Creating RabbitMQ resources'

create_vhost backendA
create_vhost backendB
create_vhost d1.example.com
create_vhost d2.example.com
create_vhost d3.example.com
create_vhost federation-v0
create_vhost federation-v1

echo 'RabbitMQ resources created successfully!'
