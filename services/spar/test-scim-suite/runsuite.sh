#!/usr/bin/env bash

set -e

SOURCE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ "$INTEGRATION_SKIP_SCIM_SUITE" -eq 1 ]]; then
    exit 0
fi

make collection -C "$SOURCE_DIR"
"$SOURCE_DIR/run.sh"
