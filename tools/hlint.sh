#!/usr/bin/env bash


usage() { echo "Usage: $0 -f [all, changeset, pr] -m [check, inplace]" 1>&2; exit 1; }

files=''

while getopts ':f:m:k' opt
 do
     case $opt in
         f) f=${OPTARG}
            if [ "$f" = "all" ]; then
              files=$(git ls-files | grep \.hs\$)
            elif [ "$f" = "pr" ]; then
              files=$(git diff --name-only origin/develop... | grep \.hs\$)
            elif [ "$f" = "changeset" ]; then
              files=$(git diff --name-only HEAD | grep \.hs\$)
            else
              usage
            fi
            ;;
         m) m=${OPTARG}
            if [ "$m" = "inplace" ]; then
            :
            elif [ "$m" = "check" ]; then
            :
            else
              usage
            fi
            ;;
         k) k=true;;
         *) usage;;
     esac
done

if [ -z "${f}" ] || [ -z "${m}" ]; then
    usage
fi

if [ "${k}" ]; then
  echo "Will fail on the first error"
  set -euo pipefail
fi

if [ "$f" = "all" ] && [ "$m" = "check" ]; then
  hlint -g
else
  count=$(echo "$files" | grep -c -v -e '^[[:space:]]*$')
  echo "Analysing $count file(s)…"
  for f in $files
  do
    echo "$f"
    if [ "$m" = "check" ]; then
      hlint --no-summary "$f"
    else
      hlint --refactor --refactor-options="--inplace" "$f"
    fi
  done
fi
