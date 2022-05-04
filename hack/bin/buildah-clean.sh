#!/usr/bin/env bash

set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TOP_LEVEL="$(cd "$DIR/../.." && pwd)"

rm -rf "$TOP_LEVEL"/buildah
# buildah rm wire-server-dev
buildah rm output
