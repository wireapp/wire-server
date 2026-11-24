#!/usr/bin/env bash
# Records the charts of the release that are not deployable (LEGACY_CHARTS, packaged
# in CHART_DIR): legacy-charts.sha256, in sha256sum format, and chart-lists.json, the
# lists of make echo-release-charts and echo-oci-charts at the source commit. Both are
# written to CHART_DIR, and the publication checks the candidate against them. Writes
# the assets (the packages and the two files, one path per line) to GITHUB_OUTPUT.
set -euo pipefail

: "${VERSION:?}" "${RELEASE_CHARTS:?}" "${OCI_CHARTS:?}" "${LEGACY_CHARTS:?}" "${CHART_DIR:?}"

die() {
  echo "::error::$*"
  exit 1
}

read -ra legacy <<<"$LEGACY_CHARTS"
read -ra release <<<"$RELEASE_CHARTS"
read -ra oci <<<"$OCI_CHARTS"
expected=$(printf '%s\n' "${legacy[@]}" "${oci[@]}" | sort)
[[ "$expected" == "$(printf '%s\n' "${release[@]}" | sort)" ]] \
  || die "the deployable and the other charts are not the charts of a release"

assets=()
: >"${CHART_DIR}/legacy-charts.sha256.tmp"
for chart in "${legacy[@]}"; do
  tgz="${CHART_DIR}/${chart}-${VERSION}.tgz"
  [[ -f "$tgz" ]] || die "${tgz} not found"
  (cd "$CHART_DIR" && sha256sum "${chart}-${VERSION}.tgz") >>"${CHART_DIR}/legacy-charts.sha256.tmp"
  assets+=("$tgz")
done
mv "${CHART_DIR}/legacy-charts.sha256.tmp" "${CHART_DIR}/legacy-charts.sha256"
jq -n --arg release "$RELEASE_CHARTS" --arg oci "$OCI_CHARTS" \
  '{release: ($release | split(" ")), oci: ($oci | split(" "))}' >"${CHART_DIR}/chart-lists.json"
assets+=("${CHART_DIR}/legacy-charts.sha256" "${CHART_DIR}/chart-lists.json")

{
  echo "assets<<EOF_ASSETS"
  printf '%s\n' "${assets[@]}"
  echo "EOF_ASSETS"
} >>"$GITHUB_OUTPUT"
