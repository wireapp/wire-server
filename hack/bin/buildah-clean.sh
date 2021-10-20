#!/usr/bin/env bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

# Note: keep these paths in sync with the paths/names used in the other buildah-* scripts.
rm -rf "$TOP_LEVEL"/dist-buildah
rm -rf "$TOP_LEVEL"/.stack-root-buildah
rm -rf "$TOP_LEVEL"/.stack-work-buildah
buildah rm wire-server-dev
buildah rm output
