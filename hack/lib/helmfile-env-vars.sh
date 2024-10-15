#!/usr/bin/env bash

export NAMESPACE=${NAMESPACE:-test-integration}
# Available $HELMFILE_ENV profiles: default, default-ssl, kind, kind-ssl
export HELMFILE_ENV=${HELMFILE_ENV:-default}
export HELM_PARALLELISM=${HELM_PARALLELISM:-1}

export NAMESPACE_1="$NAMESPACE"
export FEDERATION_DOMAIN_BASE_1="$NAMESPACE_1.svc.cluster.local"
export FEDERATION_DOMAIN_1="federation-test-helper.$FEDERATION_DOMAIN_BASE_1"

export NAMESPACE_2="$NAMESPACE-fed2"
export FEDERATION_DOMAIN_BASE_2="$NAMESPACE_2.svc.cluster.local"
export FEDERATION_DOMAIN_2="federation-test-helper.$FEDERATION_DOMAIN_BASE_2"
