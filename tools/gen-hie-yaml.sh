#!/usr/bin/env bash

# Run with `make hie.yaml` in the top-level directory.
#
# Useful when using Haskell IDE Engine
# https://github.com/haskell/haskell-ide-engine
#
# requires 'yq' executable on your path.
# Install from either
# https://github.com/mikefarah/yq
# or
# https://github.com/kislyuk/yq

set -euo pipefail

echo "cradle:"
echo "  stack:"
for pkg in `stack ide targets --stdout`; do
    dir=$(echo $pkg | awk -F: '{print $1}')
    type=$(echo $pkg | awk -F: '{print $2}')
    name=$(echo $pkg | awk -F: '{print $3}')
    package_yaml=$(find . -wholename "*/$dir/package.yaml")
    if [[ "$type" == "lib" ]]; then
        yq_cmd='.library["source-dirs"]'
    elif [[ "$type" == "exe" ]]; then
        yq_cmd=".executables[\"$name\"][\"source-dirs\"] // .executables[\"$name\"].main"
    elif [[ "$type" == "test" ]]; then
        yq_cmd=".tests[\"$name\"][\"source-dirs\"]"
    fi

    for src_dir in $(yq -r "[ $yq_cmd ] | flatten | .[]" "$package_yaml"); do
        echo "    - path: $(dirname "$package_yaml")/$src_dir"
        echo "      component: $pkg"
    done
done
