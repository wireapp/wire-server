#!/usr/bin/env bash
# Composes the body of the GitHub release: the release note (GA) or a pre-release
# banner, the unreleased changelog.d fragments, and the artifacts of the version.
set -euo pipefail

: "${RELEASE_TYPE:?}" "${VERSION:?}" "${DEV_VERSION:?}" "${SOURCE_COMMIT:?}" "${REPOSITORY:?}" "${SERVER_URL:?}" \
  "${NOTE_FILE:?}" "${CHANGELOG_FILE:?}" "${IMAGES:?}" "${CHARTS:?}" "${CHART_CHANNEL:?}" "${OUT_FILE:?}"
NOTE_PATH=${NOTE_PATH:-}

base_version=${VERSION%%-*}
read -ra images <<<"$IMAGES"
read -ra charts <<<"$CHARTS"
{
  if [[ "$RELEASE_TYPE" == ga ]]; then
    cat "$NOTE_FILE"
  else
    echo "> [!IMPORTANT]"
    echo "> **This is a pre-release (\`${RELEASE_TYPE}\`).** It is a candidate for \`${base_version}\`, not a shipped release: deploy it to non-production environments only."
    if [[ -n "$NOTE_PATH" ]]; then
      echo ">"
      echo "> Release note for ${base_version}: [\`${NOTE_PATH}\`](${SERVER_URL}/${REPOSITORY}/blob/${SOURCE_COMMIT}/${NOTE_PATH})"
    fi
  fi
  echo
  echo "---"
  echo
  if grep -q '[^[:space:]]' "$CHANGELOG_FILE"; then
    echo "### Unreleased changes (changelog.d)"
    echo
    cat "$CHANGELOG_FILE"
  else
    echo "_No changelog.d fragment pending._"
  fi
  echo
  echo "---"
  echo
  echo "### Artifacts"
  echo
  echo "| | |"
  echo "| --- | --- |"
  echo "| Docker images | ${#images[@]} images \`quay.io/wire/<name>:${VERSION}\`, promoted by digest from the dev build \`${DEV_VERSION}\` |"
  echo "| Helm charts | ${#charts[@]} charts at \`oci://quay.io/wire/charts/${CHART_CHANNEL}/<chart>\`, version \`${VERSION}\` |"
  echo "| Source commit | \`${SOURCE_COMMIT}\` |"
  echo
  echo "<details>"
  echo "<summary>Images and charts</summary>"
  echo
  for image in "${images[@]}"; do echo "- \`${image}:${VERSION}\`"; done
  for chart in "${charts[@]}"; do echo "- \`oci://quay.io/wire/charts/${CHART_CHANNEL}/${chart}:${VERSION}\`"; done
  echo
  echo "\`release-manifest.json\` (wire-server) and \`release-manifest.<component>.json\` attached to this release are the authoritative record."
  echo
  echo "</details>"
} >"$OUT_FILE"
echo "file=${OUT_FILE}" >>"$GITHUB_OUTPUT"
