#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

getPRNumber() {
    git log --reverse --format=%s -- "$1" | sed -rn '1 { /\((#.*)\)$/ s|^.*\((#.*)\)$|\1|p; }' | grep "" ||
      echo "#PR_NOT_FOUND"
}

for d in "$DIR"/*; do
    if [[ ! -d "$d" ]]; then continue; fi

    entries=("$d"/*[^~])

    if [[ ${#entries[@]} -eq 0 ]]; then continue; fi

    echo -n "## "
    # shellcheck disable=SC1003
    sed '$ a\' "$d/.title"
    echo ""
    # shellcheck disable=SC2094
    for f in "${entries[@]}"; do
        pr=$(getPRNumber "$f")
        # shellcheck disable=SC1003
        < "$f" sed -r '
          # create a bullet point on the first line
          1 { s/^/\* /; }

          # indent subsequent lines
          1 !{ s/^/  /; }

          # replace ## with PR number throughout
          s/##/'"$pr"'/g' |
          (
            if grep -q -r '\(#[^)]\)' "$f"; then
              cat
            else
              sed -r '
                # add PR number at the end (unless already present)
                $ { /^.*\((#.*)\)$/ ! { s/$/ ('"$pr"')/; } }
            '
            fi
          ) | sed -r '
          # remove trailing whitespace
          s/\s+$//

          # make sure there is a trailing newline
          $ a\'
    done
    echo ""
done
