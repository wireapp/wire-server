#!/usr/bin/env bash
# Prepares and packages the charts the way Concourse did (make release-chart-<chart>:
# copy to .local/charts, chart version and image tags set to VERSION), run from
# the source checkout, with appVersion set to VERSION too (ADR-0018). Writes
# <chart>-<VERSION>.tgz to OUT_DIR.
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${OUT_DIR:?}"
mkdir -p "$OUT_DIR"

read -ra charts <<<"$CHARTS"
for chart in "${charts[@]}"; do
  echo "::group::${chart}"
  make -s "release-chart-${chart}" HELM_SEMVER="$VERSION" DOCKER_TAG="$VERSION"
  ./hack/bin/update.sh ".local/charts/${chart}"
  helm package ".local/charts/${chart}" --app-version "$VERSION" --destination "$OUT_DIR"
  [[ -f "${OUT_DIR}/${chart}-${VERSION}.tgz" ]] || {
    echo "::error::${chart} did not package as ${chart}-${VERSION}.tgz"
    exit 1
  }
  echo "::endgroup::"
done

echo "dir=${OUT_DIR}" >>"$GITHUB_OUTPUT"
