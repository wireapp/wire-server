#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

getPRNumber() {
    git log --format=%s -- $1 | sed -r 's|^.*\((#.*)\)$|\1|'
}

for d in "$DIR"/*; do
    if [[ ! -d "$d" ]]; then continue; fi

    echo -n "## "
    sed '$ a\' "$d/.title"
    echo ""
    for f in "$d"/*; do
        pr=$(getPRNumber $f)
        sed -r -e '1 { s/^/\* /; }' -e '1 !{ s/^/  /; }' -e "s/##/$pr/g" -e "$ { /^.*\((#.*)\)$/ ! { s/$/ ($pr)/; } }" -e 's/\s+$//' -e '$ a\' "$f"
    done
    echo ""
done
