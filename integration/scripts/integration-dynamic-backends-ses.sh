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
    # Verify sender's email address (ensure the sender address is in sync with the config in brig)
    exec_until_ready "aws --endpoint-url=$ENDPOINT_URL ses verify-email-identity --email-address backend-integration$suffix@wire.com"
done

echo 'AWS ses created successfully!'


