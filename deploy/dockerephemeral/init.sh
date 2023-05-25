#!/usr/bin/env sh

exec_until_ready() {
        until $1; do echo 'service not ready yet'; sleep 1; done
}

# Assumes this to be run in an environment with `aws` installed
echo 'Creating AWS resources'
aws configure set aws_access_key_id dummykey
aws configure set aws_secret_access_key dummysecret
aws configure set region eu-west-1

# Potentially delete pre-existing tables
echo -n "waiting for dynamo: "
while (! aws --endpoint-url=http://dynamodb:8000 --cli-connect-timeout=1 dynamodb list-tables); do
    sleep 1;
done
echo " [ok!]"

for suffix in "" "2"; do
    aws --endpoint-url=http://dynamodb:8000 dynamodb delete-table --table-name integration-brig-userkey-blacklist$suffix || true
    aws --endpoint-url=http://dynamodb:8000 dynamodb delete-table --table-name integration-brig-prekeys$suffix || true

    # Create Dynamo/SQS resources
    exec_until_ready "aws --endpoint-url=http://dynamodb:8000 dynamodb create-table --table-name integration-brig-userkey-blacklist$suffix --attribute-definitions AttributeName=key,AttributeType=S --key-schema AttributeName=key,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5"
    exec_until_ready "aws --endpoint-url=http://dynamodb:8000 dynamodb create-table --table-name integration-brig-prekeys$suffix --attribute-definitions AttributeName=client,AttributeType=S --key-schema AttributeName=client,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5"

    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-brig-events$suffix"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs set-queue-attributes --queue-url http://sqs:4568/integration-brig-events$suffix --attributes VisibilityTimeout=1"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-brig-events-internal$suffix"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs set-queue-attributes --queue-url http://sqs:4568/integration-brig-events-internal$suffix --attributes VisibilityTimeout=1"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-user-events.fifo$suffix"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs set-queue-attributes --queue-url http://sqs:4568/integration-user-events.fifo$suffix --attributes VisibilityTimeout=1"

    # Gundeck's feedback queue
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-gundeck-events$suffix"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs set-queue-attributes --queue-url http://sqs:4568/integration-gundeck-events$suffix --attributes VisibilityTimeout=1"

    # Galley's team event queue
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs create-queue --queue-name integration-team-events.fifo$suffix"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs set-queue-attributes --queue-url http://sqs:4568/integration-team-events.fifo$suffix --attributes VisibilityTimeout=1"

    # Verify sender's email address (ensure the sender address is in sync with the config in brig)
    exec_until_ready "aws --endpoint-url=http://ses:4579 ses verify-email-identity --email-address backend-integration$suffix@wire.com"

    # Create SNS resources for gundeck's notifications
    exec_until_ready "aws --endpoint-url=http://sns:4575 sns create-platform-application --name integration-test$suffix --platform GCM --attributes PlatformCredential=testkey"
    exec_until_ready "aws --endpoint-url=http://sns:4575 sns create-platform-application --name integration-test$suffix --platform APNS_SANDBOX --attributes PlatformCredential=testprivatekey"
    exec_until_ready "aws --endpoint-url=http://sns:4575 sns create-platform-application --name integration-com.wire.ent$suffix --platform APNS_SANDBOX --attributes PlatformCredential=testprivatekey"

    # Cargohold's bucket; creating a bucket is not idempotent so we just try once and wait until it is ready
    # TODO: Lifecycle configuration for the bucket, if supported.
    aws --endpoint-url=http://s3:9000 s3api create-bucket --bucket "dummy-bucket$suffix"
    aws --endpoint-url=http://s3:9000 s3api wait bucket-exists --bucket "dummy-bucket$suffix"
done

echo 'AWS resources created successfully!'
