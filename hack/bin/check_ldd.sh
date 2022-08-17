#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

for binary in "$TOP_LEVEL"/dist/*; do
    ldd "$binary" | grep "not found"
    grep_code=$?
    if [[ $grep_code -eq 0 ]]; then
        echo "Error: some shared library not available for binary $binary"
        exit 1
    fi
done
