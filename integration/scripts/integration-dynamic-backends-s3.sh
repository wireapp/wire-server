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
    aws --endpoint-url="$ENDPOINT_URL" s3api create-bucket --bucket "dummy-bucket$suffix"
    aws --endpoint-url="$ENDPOINT_URL" s3api wait bucket-exists --bucket "dummy-bucket$suffix"
done

echo 'AWS s3-buckets queues created successfully!'
