#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    suffix=$((i + 2))
    aws --endpoint-url="$ENDPOINT_URL" s3api create-bucket --bucket "dummy-bucket$suffix"
    aws --endpoint-url="$ENDPOINT_URL" s3api wait bucket-exists --bucket "dummy-bucket$suffix"
done

echo 'AWS s3-buckets queues created successfully!'
