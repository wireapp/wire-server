#!/usr/bin/env bash

# This script is meant to be run from inside a buildah container. See buildah-compile.sh for details.

set -e
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

cd "$TOP_LEVEL"

cabal build \
    --prefix=./buildah/dot-cabal \
    --builddir=./buildah/dist-newstyle \
    "$@"

DIST="$TOP_LEVEL"/buildah/dist PLAN_FILE="$TOP_LEVEL"/buildah/dist-newstyle/cache/plan.json ./hack/bin/cabal-install-artefacts.sh "$@"
