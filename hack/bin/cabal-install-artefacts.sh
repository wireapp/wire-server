#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

DIST=${DIST:-"$TOP_LEVEL/dist"}
PLAN_FILE=${PLAN_FILE:-$TOP_LEVEL/dist-newstyle/cache/plan.json}

mkdir -p "$DIST"

if [[ "$1" == "all" ]]; then
  pattern='*'
else
  pattern="$1"
fi

cd "$TOP_LEVEL"

cabal-plan list-bins --plan-json "$PLAN_FILE" "$pattern:exe:*" |
  awk '{print $2}' |
  xargs -i sh -c 'test -f {} && echo {} || true' |
  xargs -P8 -I{} rsync -a {} "$DIST"
