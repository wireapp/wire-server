#!/usr/bin/env bash
# shellcheck disable=SC3040
#
set -eo pipefail

for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
    cmd="brig-index reset --elasticsearch-index-prefix directory_dyn_$i $*"
    echo "$cmd"
    $cmd
done
