#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

for d in "$DIR"/*; do
    [[ -d "$d" ]] || continue
    if [[ "$(basename "$d")" == "99-pending" ]]; then continue; fi
    rm -f "$d"/*
done
git add "$DIR"
