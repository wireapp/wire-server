#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

pkgDirs=$(find "$TOP_LEVEL" -name '*.cabal' | grep  -v dist-newstyle | xargs dirname | sort)

echo "$pkgDirs" | xargs -I{} bash -c 'cabal2nix {} > {}/default.nix'

mkLocalOverlay() {
    echo 'hself: hsuper: {'
    echo "$pkgDirs" | xargs -I{} bash -c 'echo \ \ $(basename {}) = hsuper.callPackage $(realpath --relative-to='"$TOP_LEVEL"'/nix/overlays {})/default.nix \{\}\;'
    echo '}'
}
mkLocalOverlay > "$TOP_LEVEL/nix/overlays/haskell-local.nix"

rm -rf "$TOP_LEVEL/nix/extra-deps"
mkdir -p "$TOP_LEVEL/nix/extra-deps"

yq -r '.["extra-deps"][] | select(type == "object") | select (.subdirs == null) | .git' "$TOP_LEVEL/stack.yaml" |
   xargs -I {} bash -c 'cabal2nix {} > '"$TOP_LEVEL"'/nix/extra-deps/$(basename {}).nix'

repos_with_subdirs=$(yq -r '.["extra-deps"][] | select(type == "object") | select (.subdirs != null) | .git' "$TOP_LEVEL/stack.yaml")

while read repo; do
    yq -r '.["extra-deps"][] | select(type == "object") | select (.git == $repo) | .subdirs[]' "$TOP_LEVEL/stack.yaml" --arg repo "$repo" |
        xargs -I {} bash -c 'cabal2nix '"$repo"' --subpath {} > '"$TOP_LEVEL"'/nix/extra-deps/'"$(basename "$repo")"'-$(basename {}).nix'
done <<<"$repos_with_subdirs"
