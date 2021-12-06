#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

pkgDirs=$(find "$TOP_LEVEL" -name '*.cabal' | grep  -v dist-newstyle | xargs dirname | sort)

echo "$pkgDirs" | xargs -I{} bash -c 'cabal2nix {} > {}/default.nix'

rm -rf "$TOP_LEVEL/nix/extra-deps"
mkdir -p "$TOP_LEVEL/nix/extra-deps"

repos_without_subdirs=$(yq -r '.["extra-deps"][] | select(type == "object") | select (.subdirs == null) | .git' "$TOP_LEVEL/stack.yaml")
echo "$repos_without_subdirs" | xargs -I {} bash -c 'cabal2nix {} > '"$TOP_LEVEL"'/nix/extra-deps/$(basename {}).nix'

repos_with_subdirs=$(yq -r '.["extra-deps"][] | select(type == "object") | select (.subdirs != null) | .git' "$TOP_LEVEL/stack.yaml")
while read repo; do
    yq -r '.["extra-deps"][] | select(type == "object") | select (.git == $repo) | .subdirs[]' "$TOP_LEVEL/stack.yaml" --arg repo "$repo" |
        xargs -I {} bash -c 'cabal2nix '"$repo"' --subpath {} > '"$TOP_LEVEL"'/nix/extra-deps/'"$(basename "$repo")"'-$(basename {}).nix'
done <<<"$repos_with_subdirs"

mkOverlay() {
    echo 'hself: hsuper: {'
    echo "$pkgDirs" |
        xargs -I{} bash -c 'echo \ \ $(basename {}) = hsuper.callPackage $(realpath --relative-to='"$TOP_LEVEL"'/nix/overlays {})/default.nix \{\}\;'
    echo "$repos_without_subdirs" | xargs -n 1 basename |
        xargs -I {} bash -c 'echo \ \ {} = hsuper.callPackage $(realpath --relative-to='"$TOP_LEVEL"'/nix/overlays '"$TOP_LEVEL"'/nix/extra-deps/{}.nix) \{\}\;'
    while read repo; do
        while read subdir; do
            name="$(basename "$repo")-$(basename "$subdir")"
            echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$TOP_LEVEL/nix/extra-deps/$name.nix") {};"
        done <<<"$(yq -r '.["extra-deps"][] | select(type == "object") | select (.git == $repo) | .subdirs[]' "$TOP_LEVEL/stack.yaml" --arg repo "$repo")"
    done <<<"$repos_with_subdirs"
    echo '}'
}
mkOverlay > "$TOP_LEVEL/nix/overlays/haskell.nix"
