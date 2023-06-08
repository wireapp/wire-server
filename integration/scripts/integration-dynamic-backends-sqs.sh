#!/usr/bin/env bash
# shellcheck disable=SC2004

set -eo pipefail

ENDPOINT_URL=$1

# Assumes this to be run in an environment with `aws` installed
echo 'Creating AWS resources'
aws configure set aws_access_key_id "$AWS_ACCESS_KEY_ID"
aws configure set aws_secret_access_key "$AWS_SECRET_ACCESS_KEY"
aws configure set region "$AWS_REGION"

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    suffix=$((i + 2))
    aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-brig-events$suffix
    aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-brig-events$suffix --attributes VisibilityTimeout=1
    aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-brig-events-internal$suffix
    aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-brig-events-internal$suffix --attributes VisibilityTimeout=1
    aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-user-events.fifo$suffix
    aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-user-events.fifo$suffix --attributes VisibilityTimeout=1

    # Gundeck's feedback queue
    aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-gundeck-events$suffix
    aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-gundeck-events$suffix --attributes VisibilityTimeout=1

    # Galley's team event queue
    aws --endpoint-url=$ENDPOINT_URL sqs create-queue --queue-name integration-team-events.fifo$suffix
    aws --endpoint-url=$ENDPOINT_URL sqs set-queue-attributes --queue-url $ENDPOINT_URL/integration-team-events.fifo$suffix --attributes VisibilityTimeout=1
done

echo 'AWS sqs queues created successfully!'

