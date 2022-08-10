#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

package_name="$1"

format_all() {
    cabal_files="$(cd "$TOP_LEVEL"; find . -not \( -path ./dist-newstyle -prune \) -not \( -path ./charts -prune \) -name "*.cabal")"
    for cabal_file in $cabal_files; do
        cabal-fmt -i "$cabal_file"
    done
}

format_single() {
    cabal_file="$(cd "$TOP_LEVEL"; find . -not \( -path ./dist-newstyle -prune \) -not \( -path ./charts -prune \) -name "$package_name.cabal" | head -n 1)"

    if [ -z "$cabal_file" ]; then
        echo "Could not find $package_name.cabal in project"
        exit
    fi

    cabal_file=$(realpath "$TOP_LEVEL"/"$cabal_file" --relative-to="$(pwd)")

    set -x
    cabal-fmt -i "$cabal_file"
}

handle_emacs_autosave() {
    hs_autosave_files="$(cd "$TOP_LEVEL"; find . -not \( -path ./dist-newstyle -prune \) -not \( -path ./charts -prune \) -name "*.hs~")"

    if [ -n "${hs_autosave_files}" ]; then
        if [ "${WIRE_FORCE_RM_EMACS_AUTOSAVE_FILES-}" == "1" ]; then
            rm ${hs_autosave_files}
        else
            echo -e "\nautosave files are breaking cabal-fmt:\n\n${hs_autosave_files}\n\nto remove, run with 'WIRE_FORCE_RM_EMACS_AUTOSAVE_FILES=1'.\n"
            exit 1
        fi
    fi
}

handle_emacs_autosave

if [ "$1" = "all" ]; then
    format_all
else
    format_single "$1"
fi
