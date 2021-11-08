#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

DIST="$TOP_LEVEL/dist"

cabal-plan list-bins "$1"':exe:*' | awk '{print $2}' | xargs -I '{}' rsync -a {} "$DIST"
