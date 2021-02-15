#!/usr/bin/env bash

# This script is meant to be run from inside a buildah container. See buildah-compile.sh for details.

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

cd "$TOP_LEVEL"
stack install --local-bin-path=dist-buildah --work-dir=.stack-work-buildah --stack-root="${TOP_LEVEL}"/.stack-root-buildah --fast "$@"
