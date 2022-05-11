#!/usr/bin/env bash
set -euo pipefail

for file in $(find . -name package.yaml -not -path './dist-newstyle/*'); do
  if [[ "$1" = "true" ]]
  then
    hpack -f $file;
  else
    hpack $file | sed '/is up-to-date/d';
  fi
done
