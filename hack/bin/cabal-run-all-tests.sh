#!/usr/bin/env bash

set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

packages=$(find "$TOP_LEVEL" -name '*.cabal' |
    grep -v dist-newstyle |
    xargs -n 1 dirname |
    xargs -n 1 basename)

for p in $packages; do
    echo "==== Testing $p..."
    "$DIR/cabal-run-tests.sh" "$p"
done
