#!/usr/bin/env bash

set -eo pipefail

ENDPOINT_URL=$1

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    suffix=$((i + 2))
    # Verify sender's email address (ensure the sender address is in sync with the config in brig)
    aws --endpoint-url="$ENDPOINT_URL" ses verify-email-identity --email-address "backend-integration$suffix@wire.com"
done

echo 'AWS ses created successfully!'


