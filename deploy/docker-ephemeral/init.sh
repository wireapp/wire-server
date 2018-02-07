#!/bin/sh

# Assumes this to be run in an environment with `aws` installed
echo 'Creating AWS resources'
aws configure set aws_access_key_id dummy
aws configure set aws_secret_access_key dummy
aws configure set region eu-west-1

# Potentially delete pre-existing tables
aws --endpoint-url=http://dynamodb:8000 dynamodb delete-table --table-name integration-brig-userkey-blacklist || true
aws --endpoint-url=http://dynamodb:8000 dynamodb delete-table --table-name integration-brig-prekeys || true

# Create resources
until aws --endpoint-url=http://dynamodb:8000 dynamodb create-table --table-name integration-brig-userkey-blacklist --attribute-definitions AttributeName=key,AttributeType=S --key-schema AttributeName=key,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5; do echo 'dynamodb not ready yet'; sleep 1; done
until aws --endpoint-url=http://dynamodb:8000 dynamodb create-table --table-name integration-brig-prekeys --attribute-definitions AttributeName=client,AttributeType=S --key-schema AttributeName=client,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5; do echo 'dynamodb not ready yet'; sleep 1; done
until aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-brig-events; do echo 'sqs not ready yet'; sleep 1; done
until aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-brig-events-internal; do echo "sqs not ready yet"; sleep 1; done
until aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-gundeck-events; do echo "sqs not ready yet"; sleep 1; done
until aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-team-events.fifo; do echo "sqs not ready yet"; sleep 1; done
echo 'AWS resources created successfully!'
