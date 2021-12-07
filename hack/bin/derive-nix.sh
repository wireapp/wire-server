#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

pkgDirs=$(find "$TOP_LEVEL" -name '*.cabal' | grep  -v dist-newstyle | xargs dirname | sort)

echo "$pkgDirs" | xargs -I{} bash -c 'cabal2nix {} > {}/default.nix'

extra_deps_path="$TOP_LEVEL/nix/extra-deps"
rm -rf "$extra_deps_path"
mkdir -p "$extra_deps_path"

repos_without_subdirs=$(yq -r '.["extra-deps"][] | select(type == "object") | select (.subdirs == null) | .git' "$TOP_LEVEL/stack.yaml")
echo "$repos_without_subdirs" | xargs -I {} bash -c 'cabal2nix {} > '"$extra_deps_path"'/$(basename {}).nix'

repos_with_subdirs=$(yq -r '.["extra-deps"][] | select(type == "object") | select (.subdirs != null) | .git' "$TOP_LEVEL/stack.yaml")
while read repo; do
    yq -r '.["extra-deps"][] | select(type == "object") | select (.git == $repo) | .subdirs[]' "$TOP_LEVEL/stack.yaml" --arg repo "$repo" |
        xargs -I {} bash -c 'cabal2nix '"$repo"' --subpath {} > '"$extra_deps_path"'/'"$(basename "$repo")"'-$(basename {}).nix'
done <<<"$repos_with_subdirs"

getName() {
    grep 'pname = ' "$1" | sed 's|.*pname = "\(.*\)".*|\1|'
}

mkOverlay() {
    echo 'hself: hsuper: {'

    while read pkgDir; do
        drv_path="$pkgDir/default.nix"
        name=$(getName "$drv_path")
        echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$drv_path") {};"
    done <<<"$pkgDirs"

    while read repo; do
        drv_path="$extra_deps_path/$(basename $repo).nix"
        name=$(getName "$drv_path")
        echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$drv_path") {};"
    done <<<"$repos_without_subdirs"

    while read repo; do
        subdirs=$(yq -r '.["extra-deps"][] | select(type == "object") | select (.git == $repo) | .subdirs[]' "$TOP_LEVEL/stack.yaml" --arg repo "$repo")
        while read subdir; do
            drv_path="$extra_deps_path/$(basename "$repo")-$(basename "$subdir").nix"
            name=$(getName "$drv_path")
            echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$drv_path") {};"
        done <<<"$subdirs"
    done <<<"$repos_with_subdirs"
    echo '}'
}
mkOverlay > "$TOP_LEVEL/nix/overlays/haskell.nix"
