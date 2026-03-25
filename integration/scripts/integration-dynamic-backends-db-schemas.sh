#!/usr/bin/env bash
# shellcheck disable=SC3040,SC3045,SC3057

set -eo pipefail

genargs() {
    for service in brig galley gundeck spar; do
        for i in $(seq "$INTEGRATION_DYNAMIC_BACKENDS_POOLSIZE"); do
            echo "$service"/"$service"_test_dyn_"$i"
        done
    done
    return 0
}

migrate_schema() {
    local service="$1"
    local keyspace="$2"
    shift 2
    local extra_args="$*"
    cmd="$service-schema --keyspace $keyspace $extra_args"
    $cmd
    return $?
}

export -f migrate_schema

genargs | parallel --jobs 100% migrate_schema "{1//}" "{1/}" "$@"
