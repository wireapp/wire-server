#!/usr/bin/env bash

set -e

REL_PATH=${REL_PATH:-"."}
cd "$(dirname "${BASH_SOURCE[0]}")/../$REL_PATH"

echo Formatting branch "$PR_BASE" in "${REL_PATH}"

ALLOW_DIRTY_WC="0"
ARG_ORMOLU_MODE="inplace"
PR_BASE=${PR_BASE:-"origin/develop"}

USAGE="
This bash script can either

* apply ormolu formatting in-place (the default)

* check all modules for formatting and fail if ormolu needs to be applied (-c flag). This can be run in CI to make sure no branches with non-ormolu formatting make get merged.

USAGE: $0
    -h     : show this help.
    -f pr  : run even if working copy is dirty. Check only on files changed by branch.
    -f all : run even if working copy is dirty. Check all files.
    -c     : set ormolu mode to 'check'.  default: 'inplace'
"

usage() {
  echo "$USAGE" 1>&2
  exit 1
}

# Option parsing:
# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":f:ch" opt; do
  case ${opt} in
  f)
    f=${OPTARG}
    if [ "$f" = "pr" ]; then
      ALLOW_DIRTY_WC=1
    elif [ "$f" = "all" ]; then
      ALLOW_DIRTY_WC=1
    else
      usage
    fi
    ;;
  c)
    ARG_ORMOLU_MODE="check"
    ;;
  h)
    echo "$USAGE" 1>&2
    exit 0
    ;;
  *) usage ;;
  esac
done
shift $((OPTIND - 1))

if [ "$#" -ne 0 ]; then
  echo "$USAGE" 1>&2
  exit 1
fi

if [ "$(git status -s | grep -v \?\?)" != "" ]; then
  if [ "$ALLOW_DIRTY_WC" == "1" ]; then
    :
  else
    echo "Working copy is not clean."
    echo "Run with -f pr or -f all if you want to run ormolu anyway"
    exit 1
  fi
fi

echo "ormolu mode: $ARG_ORMOLU_MODE"
echo "language extensions are taken from the resp. cabal files"

FAILURES=0

if [ -t 1 ]; then
  : "${ORMOLU_CONDENSE_OUTPUT:=1}"
fi

if [ "$f" = "all" ] || [ "$f" = "" ]; then
  files=$(git ls-files | grep '\.hsc\?$')
elif [ "$f" = "pr" ]; then
  files=$(
    git diff --diff-filter=ACMR --name-only "$PR_BASE"... | { grep '\.hsc\?$' || true; }
    git diff --diff-filter=ACMR --name-only HEAD | { grep \.hs\$ || true; }
  )
fi

count=$(echo "$files" | sed '/^\s*$/d' | wc -l)
echo "Checking $count file(s)…"

for hsfile in $files; do
  # run in background so that we can detect Ctrl-C properly
  ormolu --mode $ARG_ORMOLU_MODE --check-idempotence "$hsfile" &
  wait $! && err=0 || err=$?

  if [ "$err" == "100" ]; then
    ((++FAILURES))
    echo "$hsfile...  *** FAILED"
    clear=""
  elif [ "$err" == "0" ]; then
    echo -e "$clear$hsfile...  ok"
    [ "$ORMOLU_CONDENSE_OUTPUT" == "1" ] && clear="\033[A\r\033[K"
  else
    exit "$err"
  fi
done

if [ "$FAILURES" != 0 ]; then
  echo "ormolu failed on $FAILURES files."
  if [ "$ARG_ORMOLU_MODE" == "check" ]; then
    echo -en "\n\nyou can fix this by running 'make format' from the git repo root.\n\n"
  fi
  exit 1
fi
