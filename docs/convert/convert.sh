#!/usr/bin/env bash
#
set -e
# shellcheck disable=SC2044,SC3010
for f in $(find . -type f -name '*.rst'); do
    if [[ "$f" == */includes/* ]]; then
        echo skipping "$f"
        continue
    fi
    rst2myst convert -c convert/conversions.yaml --no-colon-fences "$f"
    rm -f "$f"
done
