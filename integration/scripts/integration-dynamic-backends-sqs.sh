#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    suffix=$((i + 2))
    aws --endpoint-url="$ENDPOINT_URL" sqs create-queue --queue-name integration-brig-events$suffix
    aws --endpoint-url="$ENDPOINT_URL" sqs set-queue-attributes --queue-url "$ENDPOINT_URL/integration-brig-events$suffix" --attributes VisibilityTimeout=1
    aws --endpoint-url="$ENDPOINT_URL" sqs create-queue --queue-name integration-brig-events-internal$suffix
    aws --endpoint-url="$ENDPOINT_URL" sqs set-queue-attributes --queue-url "$ENDPOINT_URL/integration-brig-events-internal$suffix" --attributes VisibilityTimeout=1
    aws --endpoint-url="$ENDPOINT_URL" sqs create-queue --queue-name integration-user-events.fifo$suffix
    aws --endpoint-url="$ENDPOINT_URL" sqs set-queue-attributes --queue-url "$ENDPOINT_URL/integration-user-events.fifo$suffix" --attributes VisibilityTimeout=1

    # Gundeck's feedback queue
    aws --endpoint-url="$ENDPOINT_URL" sqs create-queue --queue-name "integration-gundeck-events$suffix"
    aws --endpoint-url="$ENDPOINT_URL" sqs set-queue-attributes --queue-url "$ENDPOINT_URL/integration-gundeck-events$suffix" --attributes VisibilityTimeout=1

    # Galley's team event queue
    aws --endpoint-url="$ENDPOINT_URL" sqs create-queue --queue-name "integration-team-events.fifo$suffix"
    aws --endpoint-url="$ENDPOINT_URL" sqs set-queue-attributes --queue-url "$ENDPOINT_URL/integration-team-events.fifo$suffix" --attributes VisibilityTimeout=1
done

echo 'AWS sqs queues created successfully!'

