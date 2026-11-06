#!/usr/bin/env bash
# shellcheck disable=SC2016
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
  xargs -I {} sh -c 'test -f "$1" && echo "$1" || true' _ {} |
  xargs -P8 -I {} sh -c 'rsync -a "$1" "$2"' _ {} "$DIST"
