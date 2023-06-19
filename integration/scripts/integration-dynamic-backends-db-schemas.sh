#!/usr/bin/env bash
# shellcheck disable=SC3040,SC3045,SC3057

set -eo pipefail

genargs() {
    for service in brig galley gundeck spar; do
        for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
            echo "$service"/"$service"_test_dyn_"$i"
        done
    done
}

migrate_schema() {
    cmd="$1-schema --keyspace $2 ${*:3}"
    $cmd
}

export -f migrate_schema

genargs | parallel --jobs 100% migrate_schema "{1//}" "{1/}" "$@"
