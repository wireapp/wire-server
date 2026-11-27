#!/usr/bin/env bash
# Prepares and packages the charts the way Concourse did (make release-chart-<chart>:
# copy to .local/charts, chart version and image tags set to VERSION), run from
# the source checkout. When OUT_DIR is set, writes <chart>-<VERSION>.tgz there
# with appVersion set to VERSION too (ADR-0018).
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}"
if [[ -n ${OUT_DIR:-} ]]; then
  mkdir -p "$OUT_DIR"
fi

read -ra charts <<<"$CHARTS"
for chart in "${charts[@]}"; do
  echo "::group::${chart}"
  make -s "release-chart-${chart}" HELM_SEMVER="$VERSION" DOCKER_TAG="$VERSION"
  ./hack/bin/update.sh ".local/charts/${chart}"
  if [[ -n ${OUT_DIR:-} ]]; then
    helm package ".local/charts/${chart}" --app-version "$VERSION" --destination "$OUT_DIR"
    [[ -f "${OUT_DIR}/${chart}-${VERSION}.tgz" ]] || {
      echo "::error::${chart} did not package as ${chart}-${VERSION}.tgz"
      exit 1
    }
  fi
  echo "::endgroup::"
done

if [[ -n ${OUT_DIR:-} ]]; then
  echo "dir=${OUT_DIR}" >>"$GITHUB_OUTPUT"
fi
