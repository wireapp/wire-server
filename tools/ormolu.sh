#!/usr/bin/env bash

set -e

cd "$( dirname "${BASH_SOURCE[0]}" )/.."

command -v grep >/dev/null 2>&1 || { echo >&2 "grep is not installed, aborting."; exit 1; }
command -v awk  >/dev/null 2>&1 || { echo >&2 "awk is not installed, aborting."; exit 1; }
command -v sed  >/dev/null 2>&1 || { echo >&2 "sed is not installed, aborting."; exit 1; }
command -v yq   >/dev/null 2>&1 || { echo >&2 "yq is not installed, aborting. See https://github.com/mikefarah/yq"; exit 1; }

ORMOLU_VERSION=$(yq read stack.yaml 'extra-deps[*]' | sed -n 's/ormolu-//p')
( ormolu -v 2>/dev/null | grep -q $ORMOLU_VERSION ) || ( echo "please install ormolu $ORMOLU_VERSION (eg., run 'stack install ormolu' and ensure ormolu is on your PATH.)"; exit 1 )
echo "ormolu version: $ORMOLU_VERSION"

ARG_ALLOW_DIRTY_WC="0"
ARG_ORMOLU_MODE="inplace"

USAGE="
This bash script can either (a) apply ormolu formatting in-place to
all haskell modules in your working copy, or (b) check all modules for
formatting and fail if ormolu needs to be applied.

(a) is mostly for migrating from manually-formatted projects to
ormolu-formatted ones; (b) can be run in by a continuous integration
service to make sure no branches with non-ormolu formatting make get
merged.

For every-day dev work, consider using one of the ormolu editor
integrations (see https://github.com/tweag/ormolu#editor-integration).

USAGE: $0
    -h: show this help.
    -f: run even if working copy is dirty.  default: ${ARG_ALLOW_DIRTY_WC}
    -c: set ormolu mode to 'check'.  default: 'inplace'

"

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":fch" opt; do
  case ${opt} in
    f ) ARG_ALLOW_DIRTY_WC="1"
      ;;
    c ) ARG_ORMOLU_MODE="check"
      ;;
    h ) echo "$USAGE" 1>&2
         exit 0
      ;;
  esac
done
shift $((OPTIND -1))

if [ "$#" -ne 0 ]; then
  echo "$USAGE" 1>&2
  exit 1
fi

if [ "$(git status -s | grep -v \?\?)" != "" ]; then
    echo "working copy not clean."
    if [ "$ARG_ALLOW_DIRTY_WC" == "1" ]; then
        echo "running with -f.  this will mix ormolu and other changes."
    else
        echo "run with -f if you want to force mixing ormolu and other changes."
        exit 1
    fi
fi

LANGUAGE_EXTS=$(yq read package-defaults.yaml 'default-extensions[*]' | awk '{print "--ghc-opt -X" $0}' ORS=' ')
echo "ormolu mode: $ARG_ORMOLU_MODE"
echo "language extensions: $LANGUAGE_EXTS"

FAILURES=0

for hsfile in $(git ls-files | grep '\.hsc\?$'); do
    FAILED=0
    ormolu --mode $ARG_ORMOLU_MODE --check-idempotence $LANGUAGE_EXTS "$hsfile" || FAILED=1
    if [ "$FAILED" == "1" ]; then
        ((++FAILURES))
        echo "$hsfile...  *** FAILED"
    else
        echo "$hsfile...  ok"
    fi
done

if [ "$FAILURES" != 0 ]; then
    echo "ormolu failed on $FAILURES files."
    if [ "$ARG_ORMOLU_MODE" == "check" ]; then
        echo -en "\n\nyou can fix this by running 'make format' from the git repo root.\n\n"
    fi
    exit 1
fi
