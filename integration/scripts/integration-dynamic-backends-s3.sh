#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1

# Assumes this to be run in an environment with `aws` installed
echo 'Creating AWS resources'
aws configure set aws_access_key_id "$AWS_ACCESS_KEY_ID"
aws configure set aws_secret_access_key "$AWS_SECRET_ACCESS_KEY"
aws configure set region "$AWS_REGION"

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    suffix=$((i + 2))
    aws --endpoint-url="$ENDPOINT_URL" s3api create-bucket --bucket "dummy-bucket$suffix"
    aws --endpoint-url="$ENDPOINT_URL" s3api wait bucket-exists --bucket "dummy-bucket$suffix"
done

echo 'AWS s3-buckets queues created successfully!'
