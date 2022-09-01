#!/usr/bin/env bash

usage() { echo "Usage: $0 -f [all, changeset] -m [check, inplace]" 1>&2; exit 1; }

files=''
check=true

while getopts ':f:m:' opt
 do
     case $opt in
         f) f=${OPTARG}
            if [ "$f" = "all" ]; then
              files=$(find libs/ services/ -not -path "*/test/*" -name "*.hs")
            elif [ "$f" = "changeset" ]; then
              files=$(git diff --name-only HEAD | grep \.hs\$)
            else
              usage
            fi
            ;;
         m) m=${OPTARG}
            if [ "$m" = "inplace" ]; then
              check=false
            elif [ "$m" = "check" ]; then
              check=true
            else
              usage
            fi
            ;;
         *) usage;;
     esac
done

if [ -z "${f}" ] || [ -z "${m}" ]; then
    usage
fi

count=$(echo "$files" | grep -c -v -e '^[[:space:]]*$')

echo "Analysing $count file(s)…"

for f in $files
do
  echo "$f"
  if [ $check = true ]; then
    hlint "$f" | grep -v 'No hints'
  else
    hlint --refactor --refactor-options="--inplace" "$f"
  fi
done
