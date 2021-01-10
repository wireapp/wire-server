#!/usr/bin/env bash
# Sets up a local kubernetes cluster using kind and sets the kubectl context to
# talk to it.
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export KIND_EXPERIMENTAL_PROVIDER=podman

KIND_VERSION=v1.15.0
#KIND_VERSION=v1.14.2 / v1.14.10
#KIND_VERSION=v1.15.12 # does this also work?
# see https://hub.docker.com/r/kindest/node/tags?page=1&ordering=-name for versions.

sudo podman pull docker.io/kindest/node:$KIND_VERSION

KIND=$(which kind)
sudo "$KIND" create cluster --config "$DIR/kind.yaml" --image kindest/node:v1.15.0 || (echo "Try 'kind delete cluster' first." && exit 1)

kubectl config use-context kind-kind
