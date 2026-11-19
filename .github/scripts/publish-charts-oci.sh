#!/usr/bin/env bash
# Pushes the charts to oci://OCI_REPOSITORY/<chart> (quay.io/wire/charts/dev or
# quay.io/wire/charts/stable) with the publish-chart script of wire-rm-actions
# (PUBLISH_CHART), which never overwrites a published version, and writes the
# digest of each chart to DIGESTS_FILE (JSON object). CHART_DIR holds packaged
# charts, pushed as they are and tried again on a registry error: the
# publication of a release candidate. CHART_SRC holds prepared chart
# directories, packaged by publish-chart: a dev build, whose rerun on the same
# commit finds its charts already published and reads their digest back. With
# EXPIRES_AFTER (dev build, e.g. 30d) Quay deletes the dev charts after that delay.
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${OCI_REPOSITORY:?}" "${DIGESTS_FILE:?}" "${PUBLISH_CHART:?}"
: "${QUAY_USERNAME:?}" "${QUAY_PASSWORD:?}"
CHART_DIR=${CHART_DIR:-}
CHART_SRC=${CHART_SRC:-}

die() {
  echo "::error::$*"
  exit 1
}

[[ -f "$PUBLISH_CHART" ]] || die "publish-chart script not found at ${PUBLISH_CHART}"
[[ -n "$CHART_DIR" || -n "$CHART_SRC" ]] || die "CHART_DIR (packaged charts) or CHART_SRC (chart directories) is required"
[[ -z "$CHART_DIR" || -z "$CHART_SRC" ]] || die "CHART_DIR and CHART_SRC are exclusive"
[[ "$OCI_REPOSITORY" =~ ^([^/]+)/wire/charts/(dev|stable)$ ]] || die "OCI_REPOSITORY '${OCI_REPOSITORY}' is not <registry>/wire/charts/(dev|stable)"
registry=${BASH_REMATCH[1]}
channel=${BASH_REMATCH[2]}

printf '%s' "$QUAY_PASSWORD" | helm registry login "$registry" --username "$QUAY_USERNAME" --password-stdin

echo '{}' >"$DIGESTS_FILE"
published=$(mktemp)
trap 'rm -f "$published"' EXIT
read -ra charts <<<"$CHARTS"
for chart in "${charts[@]}"; do
  echo "::group::${chart}"
  : >"$published"
  if [[ -n "$CHART_DIR" ]]; then
    source_env=(CHART_FILE="${CHART_DIR}/${chart}-${VERSION}.tgz" ATTEMPTS=3)
  else
    source_env=(CHART_PATH="${CHART_SRC}/${chart}" EXPIRES_AFTER="${EXPIRES_AFTER:-}")
  fi
  env "${source_env[@]}" VERSION="$VERSION" REGISTRY="$registry" CHANNEL="$channel" \
    GITHUB_OUTPUT="$published" bash "$PUBLISH_CHART"
  digest=$(sed -n 's/^digest=//p' "$published" | tail -n1)
  [[ "$digest" =~ ^sha256:[0-9a-f]{64}$ ]] || die "no digest for ${OCI_REPOSITORY}/${chart}:${VERSION}"
  jq --arg c "$chart" --arg d "$digest" '.[$c] = $d' "$DIGESTS_FILE" >"${DIGESTS_FILE}.tmp"
  mv "${DIGESTS_FILE}.tmp" "$DIGESTS_FILE"
  echo "::endgroup::"
done
