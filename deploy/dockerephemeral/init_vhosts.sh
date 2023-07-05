#!/usr/bin/env sh

exec_until_ready() {
        until $1; do echo 'service not ready yet'; sleep 1; done
}

echo 'Creating RabbitMQ resources'

exec_until_ready "curl -u $RABBITMQ_USERNAME:$RABBITMQ_PASSWORD -X PUT http://rabbitmq:15672/api/vhosts/d1.default.domain"
exec_until_ready "curl -u $RABBITMQ_USERNAME:$RABBITMQ_PASSWORD -X PUT http://rabbitmq:15672/api/vhosts/d2.default.domain"
exec_until_ready "curl -u $RABBITMQ_USERNAME:$RABBITMQ_PASSWORD -X PUT http://rabbitmq:15672/api/vhosts/d3.default.domain"

echo 'RabbitMQ resources created successfully!'
