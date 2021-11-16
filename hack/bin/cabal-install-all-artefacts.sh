#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

for d in $(find "$TOP_LEVEL" -name '*.cabal' | grep -v dist-newstyle | xargs -n 1 dirname); do
    cd "$d"
    "$DIR/cabal-install-artefacts.sh" "$(basename "$d")"
done
