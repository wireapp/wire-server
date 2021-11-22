#!/usr/bin/env bash
set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

cd "$TOP_LEVEL"

package_options=$1

local_projects=$(find . -name '*.cabal' | grep -v dist-newstyle | xargs -n 1 basename | sed 's|.cabal||g' | sort)

for project in $local_projects; do
    echo "package $project
    $package_options"
done
