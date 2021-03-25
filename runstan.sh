#!/usr/bin/env bash
set -u

basedir=$(pwd)
for dir in $(find  . -name '.hie'); do
    cd $(dirname "$basedir/$dir")
    echo ""------------------------------------------------------------
    echo "$(pwd)"
    echo ""------------------------------------------------------------
    stan \
        check --exclude --severity=Performance --scope-all \
        check --exclude --severity=Style --scope-all \
        check --exclude --severity=Warning --scope-all \
        --no-default \
        -s | sed $'s,\x1b\\[[0-9;]*[a-zA-Z],,g'

done
