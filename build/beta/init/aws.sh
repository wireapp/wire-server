#!/bin/sh
echo 'Creating AWS resources'
# TODO: If dynamodb is not stopped, this will remain in an endless loop...
until aws --no-verify --endpoint-url=https://sqs sqs create-queue --queue-name local-brig-events; do echo 'sqs not ready yet'; sleep 1; done
until aws --no-verify --endpoint-url=https://sqs sqs create-queue --queue-name local-brig-events-internal; do echo "sqs not ready yet"; sleep 1; done
until aws --no-verify --endpoint-url=https://sqs sqs create-queue --queue-name local-gundeck-events; do echo "sqs not ready yet"; sleep 1; done
until aws --no-verify --endpoint-url=https://dynamodb dynamodb create-table --table-name local-brig-userkey-blacklist --attribute-definitions AttributeName=key,AttributeType=S --key-schema AttributeName=key,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5; do echo 'dynamodb not ready yet'; sleep 1; done
until aws --no-verify --endpoint-url=https://dynamodb dynamodb create-table --table-name local-brig-prekeys --attribute-definitions AttributeName=client,AttributeType=S --key-schema AttributeName=client,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5; do echo 'dynamodb not ready yet'; sleep 1; done
until aws --no-verify --endpoint-url=https://s3 s3api create-bucket --bucket local-cargohold; do echo 's3 not ready yet'; sleep 1; done
echo 'AWS resources created!'
