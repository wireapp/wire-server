#!/usr/bin/env bash
# shellcheck disable=SC3040
#
set -eo pipefail

# Default config file path, can be overridden with BRIG_CONFIG_FILE env var
BRIG_CONFIG="${BRIG_CONFIG_FILE:-./services/brig/brig.integration.yaml}"

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    cmd="brig-index -c $BRIG_CONFIG reset --elasticsearch-index-prefix directory_dyn_$i $*"
    echo "$cmd"
    $cmd
done
