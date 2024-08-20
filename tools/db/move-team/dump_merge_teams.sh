#!/usr/bin/env bash
# modify dump so that all teams are merged into one

set -eu

dir="$1"

script=$(
    for row in $(jq '.[0]' < "$1/galley.team" | uniq); do
        echo "s/$row/\"e09f7a63-b5d4-4db4-a3c1-18bddf3df7fc\"/g;"
    done
)

for f in "$dir"/*; do
    echo "$f"
    sed -e "$script" -i "$f"
done
