#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

find "$TOP_LEVEL" -name '*.cabal' |
    grep -v dist-newstyle |
    xargs -n 1 dirname |
    xargs -n 1 basename |
    xargs -n 1 "$DIR/cabal-run-tests.sh"
