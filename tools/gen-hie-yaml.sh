#!/usr/bin/env bash

# Run with `make hie.yaml` in the top-level directory.
#
# Useful when using Haskell IDE Engine
# https://github.com/haskell/haskell-ide-engine
#
# requires the python-based 'yq' executable on your path.
# Install from https://github.com/kislyuk/yq
# (not to be confused with the go-based yq from https://github.com/mikefarah/yq
#  which has a different way of specifying arguments)

set -euo pipefail

# check which kind of 'yq' we have

command -v yq > /dev/null || {
    >&2 echo "you need to have yq on your path. See https://github.com/kislyuk/yq"
    exit 1
}

file -L "$(command -v yq)" | grep "Python script" > /dev/null || {
    >&2 echo "You have an executable of yq, but it doesn't work with this script. Ensure you have the python-based yq from here: https://github.com/kislyuk/yq"
    exit 1
}

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
