#!/usr/bin/env bash
# Declares the dev build to the release managers, one record per component, from
# the source checkout: wire-server first, with the release note built from
# changelog.d, then every other chart published to OCI that the release managers
# know as a component (all of CHARTS but wire-server-enterprise, which ships with
# wire-server), at the same version and tied to wire-server. NOTIFY_RM is the
# notify-rm script of wire-rm-actions, which does the POST (OIDC token, staging
# and prod); RM_* and REQUIRED go through to it. Every record carries a dev
# manifest written by RELEASE_MANIFEST (the release-manifest script of
# wire-rm-actions, chart-only mode, as the images are Concourse's): the
# wire-server one reads publication/compatibility.yaml, because the release
# managers only learn the interfaces a version provides from a manifest, and
# lists the other charts of the build; the others have an empty compatibility.
# EXPIRES_AFTER (e.g. 30d) puts an expires_at in each manifest.
set -euo pipefail

: "${VERSION:?}" "${COMMIT:?}" "${BRANCH:?}" "${CHARTS:?}" "${DIGESTS_FILE:?}" "${OCI_REPOSITORY:?}" "${NOTIFY_RM:?}"
: "${RELEASE_MANIFEST:?}" "${SOURCE_REPOSITORY:?}" "${RUN_URL:?}"
EXPIRES_AFTER=${EXPIRES_AFTER:-}

die() {
  echo "::error::$*"
  exit 1
}

[[ -f "$NOTIFY_RM" ]] || die "notify-rm script not found at ${NOTIFY_RM}"
[[ -f "$RELEASE_MANIFEST" ]] || die "release-manifest script not found at ${RELEASE_MANIFEST}"
[[ -x changelog.d/mk-changelog.sh ]] || die "changelog.d/mk-changelog.sh not found in the source checkout"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

read -ra charts <<<"$CHARTS"
components=(wire-server)
for chart in "${charts[@]}"; do
  case $chart in
    wire-server | wire-server-enterprise) ;;
    *) components+=("$chart") ;;
  esac
done

digest_of() {
  local digest
  digest=$(jq -r --arg c "$1" '.[$c] // empty' "$DIGESTS_FILE")
  [[ "$digest" =~ ^sha256:[0-9a-f]{64}$ ]] || die "no digest for ${1} in ${DIGESTS_FILE}"
  echo "$digest"
}

# write_manifest <component> <compatibility file> <manifest>
write_manifest() {
  MODE=write MANIFEST="$3" COMPONENT="$1" RELEASE_TYPE=dev CHART_ONLY=true \
    VERSION="$VERSION" SOURCE_REPOSITORY="$SOURCE_REPOSITORY" SOURCE_REF="$BRANCH" SOURCE_COMMIT="$COMMIT" \
    CHART_REPOSITORY="oci://${OCI_REPOSITORY}/${1}" CHART_DIGEST="$(digest_of "$1")" \
    COMPATIBILITY_FILE="$2" EXPIRES_AFTER="$EXPIRES_AFTER" \
    BUILD_WORKFLOW="${GITHUB_WORKFLOW:-}" BUILD_RUN_ID="${GITHUB_RUN_ID:-}" BUILD_RUN_URL="$RUN_URL" \
    GITHUB_OUTPUT='' bash "$RELEASE_MANIFEST"
}

changelog=$(./changelog.d/mk-changelog.sh)
note="Dev build of \`${BRANCH}\` at ${COMMIT}."
if [[ -n "$changelog" ]]; then
  note+=$'\n\n'"Unreleased changes (changelog.d):"$'\n\n'"$changelog"
fi
enterprise="oci://${OCI_REPOSITORY}/wire-server-enterprise@$(digest_of wire-server-enterprise)"

# The manifest of wire-server: its chart, what publication/compatibility.yaml
# provides and requires, then the other charts of the build. A file the
# compatibility script refuses only warns: the build is then declared without
# a manifest, and the release managers see no interface served by this version.
manifest=""
if [[ ! -f publication/compatibility.yaml ]]; then
  echo "::warning::no publication/compatibility.yaml at ${COMMIT}: wire-server ${VERSION} is declared without a manifest"
elif ! write_manifest wire-server publication/compatibility.yaml "${work}/wire-server.json"; then
  echo "::warning::publication/compatibility.yaml was refused: wire-server ${VERSION} is declared without a manifest"
else
  others=()
  for chart in "${charts[@]}"; do
    [[ "$chart" == wire-server ]] || others+=("$chart")
  done
  jq --arg oci "$OCI_REPOSITORY" --arg version "$VERSION" --arg others "${others[*]}" --slurpfile digests "$DIGESTS_FILE" '
    .artifacts += [$others | split(" ")[] | select(. != "")
                   | {type: "chart", repository: ("oci://" + $oci + "/" + .), version: $version, digest: $digests[0][.]}]' \
    "${work}/wire-server.json" >"${work}/wire-server.all.json"
  jq -e 'all(.artifacts[]; (.digest // "") | test("^sha256:[0-9a-f]{64}$"))' "${work}/wire-server.all.json" >/dev/null \
    || die "a chart of the build has no digest in ${DIGESTS_FILE}"
  manifest="${work}/wire-server.all.json"
  echo "::notice::wire-server manifest: $(jq -c '[.compatibility.provides[] | "\(.interface) \(.supported)"]' "$manifest")"
fi

no_compatibility="${work}/no-compatibility.yaml"
printf 'providesApis: []\nconsumesApis: []\ndependsOn: []\nincludes: []\n' >"$no_compatibility"

echo "::notice::${#components[@]} components to declare: ${components[*]}"
for component in "${components[@]}"; do
  echo "::group::${component} ${VERSION}"
  if [[ "$component" == wire-server ]]; then
    meta="chart.wire-server-enterprise=${enterprise}"
    title=$VERSION
    description=$note
    manifest_file=$manifest
  else
    meta="follows=wire-server"
    title=""
    description=""
    manifest_file="${work}/${component}.json"
    write_manifest "$component" "$no_compatibility" "$manifest_file"
  fi
  COMPONENT=$component \
    KIND=dev \
    REPOSITORY="oci://${OCI_REPOSITORY}/${component}" \
    DIGEST="$(digest_of "$component")" \
    NOTE_TITLE=$title \
    NOTE_DESCRIPTION=$description \
    META=$meta \
    MANIFEST_FILE=$manifest_file \
    bash "$NOTIFY_RM"
  echo "::endgroup::"
done
