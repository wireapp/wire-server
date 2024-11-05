#!/usr/bin/env sh

exec_until_ready() {
        until $1; do echo 'service not ready yet'; sleep 1; done
}

# Assumes this to be run in an environment with `aws` installed
# Keep these in sync with .envrc
echo 'Creating AWS resources'
aws configure set aws_access_key_id dummykey
aws configure set aws_secret_access_key dummysecret
aws configure set region eu-west-1

# Potentially delete pre-existing tables
echo "waiting for dynamo: "
while (! aws --endpoint-url=http://dynamodb:8000 --cli-connect-timeout=1 dynamodb list-tables); do
    sleep 1;
done
echo " [ok!]"

for suffix in "" "2" "3" "4" "5" "-federation-v0"; do
    aws --endpoint-url=http://dynamodb:8000 dynamodb delete-table --table-name integration-brig-userkey-blacklist$suffix || true
    aws --endpoint-url=http://dynamodb:8000 dynamodb delete-table --table-name integration-brig-prekeys$suffix || true

    # Create Dynamo resources
    exec_until_ready "aws --endpoint-url=http://dynamodb:8000 dynamodb create-table --table-name integration-brig-userkey-blacklist$suffix --attribute-definitions AttributeName=key,AttributeType=S --key-schema AttributeName=key,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5"
    exec_until_ready "aws --endpoint-url=http://dynamodb:8000 dynamodb create-table --table-name integration-brig-prekeys$suffix --attribute-definitions AttributeName=client,AttributeType=S --key-schema AttributeName=client,KeyType=HASH --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5"

    # Verify sender's email address (ensure the sender address is in sync with the config in brig)
    exec_until_ready "aws --endpoint-url=http://ses:4579 ses verify-email-identity --email-address backend-integration$suffix@wire.com"

    # Create SNS resources for gundeck's notifications
    exec_until_ready "aws --endpoint-url=http://sns:4575 sns create-platform-application --name integration-test$suffix --platform GCM --attributes PlatformCredential=testkey"
    exec_until_ready "aws --endpoint-url=http://sns:4575 sns create-platform-application --name integration-test$suffix --platform APNS_SANDBOX --attributes PlatformCredential=testprivatekey"
    exec_until_ready "aws --endpoint-url=http://sns:4575 sns create-platform-application --name integration-test$suffix --platform APNS_VOIP_SANDBOX --attributes PlatformCredential=testprivatekey"
    exec_until_ready "aws --endpoint-url=http://sns:4575 sns create-platform-application --name integration-com.wire.ent$suffix --platform APNS_SANDBOX --attributes PlatformCredential=testprivatekey"

    # Cargohold's bucket; creating a bucket is not idempotent so we just try once and wait until it is ready
    # TODO: Lifecycle configuration for the bucket, if supported.
    aws --endpoint-url=http://s3:9000 s3api create-bucket --bucket "dummy-bucket$suffix"
    aws --endpoint-url=http://s3:9000 s3api wait bucket-exists --bucket "dummy-bucket$suffix"

    # Check that SQS resources are created
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs get-queue-url --queue-name integration-brig-events$suffix"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs get-queue-url --queue-name integration-brig-events-internal$suffix"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs get-queue-url --queue-name integration-user-events$suffix.fifo"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs get-queue-url --queue-name integration-gundeck-events"
    exec_until_ready "aws --endpoint-url=http://sqs:4568 sqs get-queue-url --queue-name integration-team-events$suffix.fifo"
done

echo 'AWS resources created successfully!'
