#!/usr/bin/env bash

set -xe

export HELM_DATA_HOME=$(mktemp -d)
export HELM_CONFIG_HOME=$(mktemp -d)
export HELM_CACHE_HOME=$(mktemp -d)

make clean-charts

NAMESPACE=test-stefan-2 DOCKER_TAG=0.0.2-pr.13994 make kube-integration-setup

NAMESPACE=test-stefan-2 DOCKER_TAG=0.0.2-pr.13994 HELM_PARALLELISM=0 make kube-integration-test
