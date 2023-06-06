#!/usr/bin/env bash
# shellcheck disable=SC2004

set -eo pipefail

ENDPOINT_URL=$1

# Assumes this to be run in an environment with `aws` installed
echo 'Creating AWS resources'
aws configure set aws_access_key_id dummykey
aws configure set aws_secret_access_key dummysecret
aws configure set region eu-west-1

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    suffix=$((i + 2))
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-brig-events$suffix"
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-brig-events$suffix --attributes VisibilityTimeout=1"
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-brig-events-internal$suffix"
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-brig-events-internal$suffix --attributes VisibilityTimeout=1"
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-user-events.fifo$suffix"
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-user-events.fifo$suffix --attributes VisibilityTimeout=1"

    # Gundeck's feedback queue
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-gundeck-events$suffix"
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-gundeck-events$suffix --attributes VisibilityTimeout=1"

    # Galley's team event queue
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-team-events.fifo$suffix"
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-team-events.fifo$suffix --attributes VisibilityTimeout=1"
done

echo 'AWS sqs queues created successfully!'

