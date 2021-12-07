#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

pkg_dirs=$(find "$TOP_LEVEL" -name '*.cabal' | grep  -v dist-newstyle | xargs dirname | sort)

echo "$pkg_dirs" | xargs -I{} bash -c 'cabal2nix {} > {}/default.nix'

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

declare -a built_pkgs
mkOverlayBegining() {
    echo 'hself: hsuper: {'

    while read pkg_dir; do
        drv_path="$pkg_dir/default.nix"
        name=$(getName "$drv_path")
        built_pkgs+=("$name")
        echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$drv_path") {};"
    done <<<"$pkg_dirs"

    while read repo; do
        drv_path="$extra_deps_path/$(basename $repo).nix"
        name=$(getName "$drv_path")
        built_pkgs+=("$name")
        echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$drv_path") {};"
    done <<<"$repos_without_subdirs"

    while read repo; do
        subdirs=$(yq -r '.["extra-deps"][] | select(type == "object") | select (.git == $repo) | .subdirs[]' "$TOP_LEVEL/stack.yaml" --arg repo "$repo")
        while read subdir; do
            drv_path="$extra_deps_path/$(basename "$repo")-$(basename "$subdir").nix"
            name=$(getName "$drv_path")
            built_pkgs+=("$name")
            echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$drv_path") {};"
        done <<<"$subdirs"
    done <<<"$repos_with_subdirs"
}

mkOverlayBegining > "$TOP_LEVEL/nix/overlays/haskell.nix"

ghc_pkgs=(base ghc ghci ghc-boot ghc-boot-th ghc-heap)
all_pkgs=(${ghc_pkgs[@]} + ${built_pkgs[@]})

separator="\",\""
all_pkg_json="$(printf "${separator}%s" "${all_pkgs[@]}")"
all_pkg_json="${all_pkg_json:${#separator}}"
all_pkg_json="[\"$all_pkg_json\"]"

hackage_deps=$(cd "$TOP_LEVEL"; stack ls dependencies json | jq -r '.[] | select ([.name] | inside($derivedDeps) == false) | "\(.name)-\(.version)"' --argjson derivedDeps "$all_pkg_json")
echo "$hackage_deps" | xargs -P 16 -I {} bash -c 'cabal2nix cabal://{} > '"$extra_deps_path"'/{}.nix'

mkOverlayRest() {
    while read pkg; do
        drv_path="$extra_deps_path/$pkg.nix"
        name=$(getName "$drv_path")
        all_pkgs+=("$name")
        echo "  $name = hsuper.callPackage $(realpath --relative-to="$TOP_LEVEL/nix/overlays" "$drv_path") {};"
    done <<<"$hackage_deps"
    echo '}'
}
mkOverlayRest >> "$TOP_LEVEL/nix/overlays/haskell.nix"
