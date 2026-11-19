#!/usr/bin/env bash
# Declares the published release to the release managers, one record per
# component: wire-server first, with the release note, the release declaration
# and its manifest, then every follower chart at the same version, tied to
# wire-server, with its own manifest. NOTIFY_RM is the notify-rm script of
# wire-rm-actions, which does the POST (OIDC token, staging and prod); RM_* go
# through to it. A failed declaration only warns, as for every component.
set -euo pipefail

: "${VERSION:?}" "${KIND:?}" "${COMMIT:?}" "${BRANCH:?}" "${COMPONENTS:?}" "${CHART_DIGESTS_FILE:?}" \
  "${OCI_REPOSITORY:?}" "${MANIFEST_DIR:?}" "${RELEASE_URL:?}" "${RELEASED_AT:?}" "${NOTE_FILE:?}" "${NOTIFY_RM:?}"
DECLARATION_FILE=${DECLARATION_FILE:-}

die() {
  echo "::error::$*"
  exit 1
}

[[ -f "$NOTIFY_RM" ]] || die "notify-rm script not found at ${NOTIFY_RM}"
[[ "$KIND" =~ ^(ga|prerelease)$ ]] || die "KIND must be ga or prerelease"

digest_of() {
  local digest
  digest=$(jq -r --arg c "$1" '.[$c] // empty' "$CHART_DIGESTS_FILE")
  [[ "$digest" =~ ^sha256:[0-9a-f]{64}$ ]] || die "no digest for ${1} in ${CHART_DIGESTS_FILE}"
  echo "$digest"
}

read -ra components <<<"$COMPONENTS"
enterprise="oci://${OCI_REPOSITORY}/wire-server-enterprise@$(digest_of wire-server-enterprise)"
echo "::notice::$((${#components[@]} + 1)) components to declare: wire-server ${components[*]}"

echo "::group::wire-server ${VERSION}"
COMPONENT=wire-server KIND=$KIND \
  REPOSITORY="oci://${OCI_REPOSITORY}/wire-server" DIGEST="$(digest_of wire-server)" \
  NOTE_TITLE="v${VERSION}" NOTE_DESCRIPTION="$(cat "$NOTE_FILE")" \
  META="chart.wire-server-enterprise=${enterprise}" \
  DECLARATION_FILE=$DECLARATION_FILE MANIFEST_FILE="${MANIFEST_DIR}/release-manifest.json" \
  bash "$NOTIFY_RM"
echo "::endgroup::"

for component in "${components[@]}"; do
  echo "::group::${component} ${VERSION}"
  manifest="${MANIFEST_DIR}/release-manifest.${component}.json"
  [[ -f "$manifest" ]] || die "no manifest for ${component} at ${manifest}"
  COMPONENT=$component KIND=$KIND \
    REPOSITORY="oci://${OCI_REPOSITORY}/${component}" DIGEST="$(digest_of "$component")" \
    NOTE_TITLE="" NOTE_DESCRIPTION="" META="follows=wire-server" \
    DECLARATION_FILE="" MANIFEST_FILE=$manifest \
    bash "$NOTIFY_RM"
  echo "::endgroup::"
done
