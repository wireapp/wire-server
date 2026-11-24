#!/usr/bin/env bash
# Publishes the charts of a release from develop to the legacy channels: the S3 Helm
# repository S3_REPOSITORY, then the pin in wire-builds on WIRE_BUILDS_BRANCH. Both can be
# done again on the same release: a package already in the repository is skipped and a
# release already pinned is left alone. Appends s3=<repository> to GITHUB_OUTPUT once the
# charts are in the repository, then wire-builds=pinned|unchanged|ahead.
# CHART_DIR holds the packages of the candidate, SOURCE_DIR a checkout of wire-server with
# its history.
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${CHART_DIR:?}" "${COMMIT:?}" "${S3_REPOSITORY:?}" "${WIRE_BUILDS_BRANCH:?}" "${SOURCE_DIR:?}"
scripts=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

"${scripts}/publish-charts-s3.sh"
echo "s3=${S3_REPOSITORY}" >>"${GITHUB_OUTPUT:-/dev/null}"

WIRE_BUILDS_GUARD=true "${scripts}/bump-wire-builds.sh"
