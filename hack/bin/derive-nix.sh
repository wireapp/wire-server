#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

pkgDirs=$(find "$TOP_LEVEL" -name '*.cabal' | grep  -v dist-newstyle | xargs dirname | sort)

echo "$pkgDirs" | xargs -I{} bash -c 'cabal2nix {} > {}/default.nix'

mkOverlay() {
    echo 'hself: hsuper: {'
    echo "$pkgDirs" | xargs -I{} bash -c 'echo \ \ $(basename {}) = hsuper.callPackage $(realpath --relative-to='"$TOP_LEVEL"'/nix/overlays {})/default.nix \{\}\;'
    echo '}'
}
mkOverlay > "$TOP_LEVEL/nix/overlays/haskell-local.nix"
