#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

DIST="$TOP_LEVEL/dist"

mkdir -p "$DIST"

if [[ "$1" == "all" ]]; then
  pattern='*'
else
  pattern="$1"
fi

cabal-plan list-bins "$pattern:exe:*" |
  awk '{print $2}' |
  xargs -i sh -c 'test -f {} && echo {} || true' |
  xargs -P8 -i rsync -a {} "$DIST"
