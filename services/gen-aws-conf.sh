#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/" && pwd )"

which yq >/dev/null || ( echo "*** please install yq ( https://github.com/mikefarah/yq ) in your path."; exit 22 )

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
    yq r "${DIR}/integration-aws.yaml" "${service}" > "/tmp/${service}-aws.yaml"
    yq m -a "/tmp/${service}-aws.yaml" "${DIR}/${service}/${service}.integration.yaml" > "${DIR}/${service}/${service}.integration-aws.yaml"
done

# Example of what the integration-aws.yaml could look like
# brig:
#   aws:
#     userJournalQueue: <queue_name>.fifo
#     prekeyTable: <table_name>
#     sqsEndpoint: https://sqs.eu-west-1.amazonaws.com
#     dynamoDBEndpoint: https://dynamodb.eu-west-1.amazonaws.com
#   internalEvents:
#     queueType: sqs
#     queueName: <queue_name>
#   emailSMS:
#     email:
#       sesQueue: <queue_name>
#       sesEndpoint: https://email.eu-west-1.amazonaws.com
#
# cargohold:
#   aws:
#     s3Bucket: <bucket_name>
#     s3Endpoint: https://s3.eu-west-1.amazonaws.com
#
# galley:
#   journal:
#     queueName: <queue_name>.fifo
#     endpoint: https://sqs.eu-west-1.amazonaws.com
#     region: eu-west-1
#
# gundeck:
#   aws:
#     queueName: integration-gundeck-events
#     region: eu-west-1
#     account: "<account_nr>"
#     arnEnv: <some_arn_env>
#     sqsEndpoint: https://sqs.eu-west-1.amazonaws.com
#     snsEndpoint: https://sns.eu-west-1.amazonaws.com
