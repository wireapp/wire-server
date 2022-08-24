#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)
cabalFiles=$(find "$ROOT_DIR" -name '*.cabal' \
                 | grep -v dist-newstyle | sort)

echo "$cabalFiles" \
    | xargs -I {} bash -c 'cd $(dirname {}); cabal2nix . --no-hpack > default.nix'

overridesFile="$ROOT_DIR/nix/local-haskell-packages.nix"

echo "hsuper: hself: {" > "$overridesFile"
echo "$cabalFiles" \
    | xargs -I {} bash -c 'name=$(basename {} | sed "s|.cabal||"); echo "  $name = hself.callPackage $(realpath --relative-to='"$ROOT_DIR/nix"' "$(dirname {})")/default.nix { };"' >> "$overridesFile"
echo "}" >> "$overridesFile"
