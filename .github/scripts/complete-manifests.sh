#!/usr/bin/env bash
# Completes every manifest of the release with its publication block (the
# release-manifest script of wire-rm-actions, mode complete: the frozen part is
# never edited), each one getting the digest of its own charts. Writes the
# completed copies to OUT_DIR and the replace-assets lines to GITHUB_OUTPUT.
set -euo pipefail

: "${MANIFESTS:?}" "${OUT_DIR:?}" "${RUN_URL:?}" "${CHART_DIGESTS_FILE:?}" "${RELEASE_MANIFEST:?}"
[[ -f "$RELEASE_MANIFEST" ]] || { echo "::error::release-manifest script not found at ${RELEASE_MANIFEST}"; exit 1; }

mkdir -p "$OUT_DIR"
replace=()
while IFS= read -r manifest; do
  [[ -n "$manifest" ]] || continue
  name=$(basename "$manifest")
  digests=$(jq -c --slurpfile d "$CHART_DIGESTS_FILE" '
    [.artifacts[] | select(.type == "chart") | {key: (.repository + ":" + .version), value: $d[0][(.repository | sub("^.*/"; ""))]}]
    | map(select(.value != null)) | from_entries' "$manifest")
  MODE=complete MANIFEST=$manifest OUT="${OUT_DIR}/${name}" RUN_URL=$RUN_URL DIGESTS_JSON=$digests GITHUB_OUTPUT=/dev/null \
    bash "$RELEASE_MANIFEST"
  replace+=("${name}=${OUT_DIR}/${name}")
done <<<"$MANIFESTS"
{
  echo "dir=${OUT_DIR}"
  echo "replace-assets<<EOF_REPLACE"
  printf '%s\n' "${replace[@]}"
  echo "EOF_REPLACE"
} >>"$GITHUB_OUTPUT"
