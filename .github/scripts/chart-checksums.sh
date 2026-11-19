#!/usr/bin/env bash
# Records the packaged charts of the release: [{name, file, sha256}] in
# CHARTS_FILE, and the asset list (one path per line) in GITHUB_OUTPUT.
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${CHART_DIR:?}" "${CHARTS_FILE:?}"

echo '[]' >"$CHARTS_FILE"
assets=()
read -ra charts <<<"$CHARTS"
for chart in "${charts[@]}"; do
  tgz="${CHART_DIR}/${chart}-${VERSION}.tgz"
  [[ -f "$tgz" ]] || { echo "::error::${tgz} not found"; exit 1; }
  sha256=$(sha256sum "$tgz" | awk '{print $1}')
  jq --arg n "$chart" --arg f "$(basename "$tgz")" --arg s "$sha256" '. + [{name: $n, file: $f, sha256: $s}]' "$CHARTS_FILE" >"${CHARTS_FILE}.tmp"
  mv "${CHARTS_FILE}.tmp" "$CHARTS_FILE"
  assets+=("$tgz")
done
{
  echo "file=${CHARTS_FILE}"
  echo "assets<<EOF_ASSETS"
  printf '%s\n' "${assets[@]}"
  echo "EOF_ASSETS"
} >>"$GITHUB_OUTPUT"
