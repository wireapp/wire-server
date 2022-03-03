# Installing and setting up Legal Hold

## Introduction

Legal Hold is a service allowing for all communications of specific users of a Wire installation to be recorded in a secure vault, typically for legal reasons.

The Legal Hold service is composed of three different sub-services:

* Collector, which collects the conversations from individual users
* Exporter, which exports the conversations for storage
* Hold, which holds the conversation after Collector collects them, and before Exporter exports them

A typical installation of Legal Hold looks like this:

1. The Legal Hold service/container is installed and run on a server
2. The Teams Settings for the team that will use Legal Hold is configured to use this service
3. Specific users in that team are selected to have Legal Hold activated
4. These users are asked to confirm that they are aware of the information collection. Once they do, Legal Hold is active for their account.
5. Information starts being collected by the Collector service and stored by the Hold service

## Installing Legal Hold

## Configuring Team Settings to use Legal Hold

