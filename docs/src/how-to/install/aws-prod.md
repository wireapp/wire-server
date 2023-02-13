(aws-prod)=

# Configuring AWS and wire-server (production) components

## Introduction

The following procedures are for configuring wire-server on top of AWS. They are not required to use wire-server in AWS, but they may be a good idea, depending on the AWS features you are comfortable using.

## Using real AWS services for SNS

AWS SNS is required to send notification events to clients via [FCM](https://firebase.google.com/docs/cloud-messaging/)/[APNS](https://developer.apple.com/notifications/) . These notification channels are useable only for clients that are connected from the public internet. Using these vendor provided communication channels allows client devices (phones) running a wire client to save a considerable amount of battery life, compared to the websockets approach.

For details on how to set up SNS in cooperation with us (We - Wire - will proxy push notifications through Amazon for you), see {ref}`push-sns`.

## Using real AWS services for SES / SQS

AWS SES and SQS are used for delivering emails to clients, and for receiving notifications of bounced emails. SQS is also used internally, in order to facilitate batch user deletion.

FIXME: detail this step.

## Using real AWS services for S3

S3-style services are used by cargohold to store encrypted files that users are sharing amongst each other, profile pics, etc.

Defining S3 services:
Create an S3 bucket in the region you are hosting your wire servers in. For example terraform code, see: <https://github.com/wireapp/wire-server-deploy/tree/develop/terraform/modules/aws-cargohold-asset-storage>

The S3 bucket you create should have it's contents downloadable from the internet, as clients get the content directly from S3, rather than having to talk through the wire backend.

Using S3 services:

There are three values in the `cargohold.config.aws` section of your 'values.yaml' that you need to provide while deploying wire-server:

- s3Bucket: the name of the S3 bucket you have created.
- s3Endpoint: the S3 service endpoint cargohold should talk to, to place files in the S3 bucket. On AWS, this takes the form of: `https://<bucket_name>.s3-<region_name>.amazonaws.com`.
- s3DownloadEndpoint: The URL base that clients should use to get contents from the S3 bucket. On AWS, this takes the form of: `https://s3.<region_name>.amazonaws.com`.
