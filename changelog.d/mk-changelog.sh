#!/usr/bin/env bash

set -euo pipefail
shopt -s nullglob

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

getPRNumber() {
    git log --reverse --format=%s -- $1 | sed -rn '1 { /\((#.*)\)$/ s|^.*\((#.*)\)$|\1|p; }' | grep "" ||
      echo "#PR_NOT_FOUND"
}

for d in "$DIR"/*; do
    if [[ ! -d "$d" ]]; then continue; fi

    echo -n "## "
    sed '$ a\' "$d/.title"
    echo ""
    for f in "$d"/*[^~]; do
        pr=$(getPRNumber $f)
        sed -r '
          # create a bullet point on the first line
          1 { s/^/\* /; }

          # indent subsequent lines
          1 !{ s/^/  /; }

          # replace ## with PR number throughout
          s/##/'"$pr"'/g

          # add PR number at the end (unless already present)
          $ { /^.*\((#.*)\)$/ ! { s/$/ ('"$pr"')/; } }

          # remove trailing whitespace
          s/\s+$//

          # make sure there is a trailing newline
          $ a\' "$f"
    done
    echo ""
done
