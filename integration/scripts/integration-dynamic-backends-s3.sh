#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    bucket="dummy-bucket$((i + 2))"
    if aws --endpoint-url="$ENDPOINT_URL" s3api head-bucket --bucket "$bucket"; then
        echo "AWS s3-bucket '$bucket' already exists"
    else
        aws --endpoint-url="$ENDPOINT_URL" s3api create-bucket --bucket "$bucket"
        aws --endpoint-url="$ENDPOINT_URL" s3api wait bucket-exists --bucket "$bucket"
    fi
done

echo 'AWS s3 buckets created successfully!'
