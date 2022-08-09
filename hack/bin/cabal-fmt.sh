#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

package_name="$1"

cabal_file="$(cd "$TOP_LEVEL"; find . -not \( -path ./dist-newstyle -prune \) -not \( -path ./charts -prune \) -name "$package_name.cabal" | head -n 1)"

if [ -z "$cabal_file" ]; then
    echo "Could not find $package_name.cabal in project"
    exit
fi

cabal_file=$(realpath "$TOP_LEVEL"/$cabal_file --relative-to="$(pwd)")

set -x
cabal-fmt -i "$cabal_file"
