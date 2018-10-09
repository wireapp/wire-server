#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/" && pwd )"

# Ensure that we have a file named integration-aws.yaml in the current
# dir. If not, fetch it from a known location on S3
if [ ! -f "${DIR}/integration-aws.yaml" ]
then
    echo "Could not find AWS config file to override settings, specify a location on S3 to download the file or add one at ${DIR}/integration-aws.yaml and retry: "
    read -r location
    aws s3 cp s3://"$location" "${DIR}/integration-aws.yaml"
fi

services=( brig cargohold galley gundeck cannon proxy spar )
for service in "${services[@]}"; do
    yaml merge "${DIR}/integration-aws.yaml" "${DIR}/${service}/${service}.integration.yaml" > "${DIR}/${service}/${service}.integration-aws.yaml"
done
