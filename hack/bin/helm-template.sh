#!/usr/bin/env bash

# This script can be used to render all helm charts with values filled in from
# hack/helm_vars as overrrides, if available.  This allows debugging helm
# templating issues without actually installing anything, and without needing
# access to a kubernetes cluster

# Call the script directly (optionally with `--skip-deps`), or via the
# `helm-template` make target.

set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
CHARTS_DIR="${TOP_LEVEL}/.local/charts"
: "${FEDERATION_DOMAIN_BASE:=example.com}"
: "${FEDERATION_DOMAIN:=namespace1.example.com}"
: "${NAMESPACE:=namespace1}"

export FEDERATION_DOMAIN_BASE
export FEDERATION_DOMAIN
export NAMESPACE

if [ ! -f "$DIR/../helm_vars/wire-server/certificates-namespace1.yaml" ]; then
  "$DIR/selfsigned-kubernetes.sh" namespace1
fi

helmfile -f "$DIR/../helmfile-single.yaml" template "$@"
