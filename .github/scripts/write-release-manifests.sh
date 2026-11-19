#!/usr/bin/env bash
# Writes the release manifests of the candidate (wire.component-release-manifest/v1alpha1,
# ADR-0020 section 5), one per component the release managers know: wire-server, with
# every image of the release (promoted by digest from the dev build DEV_VERSION, never
# rebuilt), its chart and the wire-server-enterprise chart; then one reduced manifest per
# follower chart, with that chart only. release-manifest.json is wire-server's,
# release-manifest.<component>.json the others. Prints the list of files to GITHUB_OUTPUT.
set -euo pipefail

: "${OUT_DIR:?}" "${VERSION:?}" "${RELEASE_TYPE:?}" "${DEV_VERSION:?}" "${SOURCE_REPOSITORY:?}" \
  "${SOURCE_REF:?}" "${SOURCE_COMMIT:?}" "${CHARTS_FILE:?}" "${CHART_CHANNEL:?}" "${IMAGE_DIGESTS_FILE:?}" \
  "${COMPATIBILITY_FILE:?}" "${BUILD_WORKFLOW:?}" "${BUILD_RUN_ID:?}" "${BUILD_RUN_URL:?}"
DOCUMENTS_JSON=${DOCUMENTS_JSON:-{\}}

die() {
  echo "::error::$*"
  exit 1
}

[[ "$RELEASE_TYPE" =~ ^(ga|alpha|beta|rc)$ ]] || die "RELEASE_TYPE must be ga, alpha, beta or rc, not '${RELEASE_TYPE}'"
[[ "$CHART_CHANNEL" =~ ^(dev|stable)$ ]] || die "CHART_CHANNEL must be dev or stable"
[[ "$SOURCE_COMMIT" =~ ^[0-9a-f]{40}$ ]] || die "SOURCE_COMMIT is not a full sha"
jq -e 'type == "object" and length > 0 and all(.[]; test("^sha256:[0-9a-f]{64}$"))' "$IMAGE_DIGESTS_FILE" >/dev/null \
  || die "${IMAGE_DIGESTS_FILE} is not an object of image digests"
jq -e 'type == "array" and length > 0' "$CHARTS_FILE" >/dev/null || die "${CHARTS_FILE} is not a list of charts"
jq -e '.provides and .requires' "$COMPATIBILITY_FILE" >/dev/null \
  || die "${COMPATIBILITY_FILE} is not the provides/requires object of the compatibility action"

kind=prerelease
stage=$RELEASE_TYPE
if [[ "$RELEASE_TYPE" == ga ]]; then
  kind=ga
  stage=""
fi
created_at=$(date -u +%Y-%m-%dT%H:%M:%SZ)
mkdir -p "$OUT_DIR"
files=()

write() {
  local component=$1 images=$2 charts=$3 compatibility=$4 documents=$5 out=$6
  jq -n \
    --arg component "$component" --arg version "$VERSION" --arg kind "$kind" --arg stage "$stage" \
    --arg source_repository "$SOURCE_REPOSITORY" --arg source_ref "$SOURCE_REF" --arg source_commit "$SOURCE_COMMIT" \
    --arg dev_version "$DEV_VERSION" --arg channel "$CHART_CHANNEL" \
    --argjson images "$images" --argjson charts "$charts" --argjson compatibility "$compatibility" \
    --argjson documents "$documents" \
    --arg workflow "$BUILD_WORKFLOW" --arg run_id "$BUILD_RUN_ID" --arg run_url "$BUILD_RUN_URL" --arg created_at "$created_at" '
    {
      schemaVersion: "wire.component-release-manifest/v1alpha1",
      component: $component,
      version: $version,
      kind: $kind
    }
    + (if $stage != "" then {stage: $stage} else {} end)
    + {
      source: {repository: $source_repository, ref: $source_ref, commit: $source_commit, tag: ("v" + $version)},
      inputs: [],
      artifacts: (
        ($images | to_entries | map({type: "image", repository: .key, tag: $version, digest: .value, candidate_tag: $dev_version}))
        + ($charts | map({type: "chart", repository: ("oci://quay.io/wire/charts/" + $channel + "/" + .name), version: $version, file: .file, sha256: .sha256}))
      ),
      compatibility: {
        provides: $compatibility.provides,
        requires: $compatibility.requires,
        includes: ($compatibility.includes // [])
      },
      build: {workflow: $workflow, run_id: $run_id, run_url: $run_url, created_at: $created_at},
      request_id: null,
      rm_origin: null,
      documents: $documents
    }' >"$out"
  files+=("$out")
  echo "$(jq -r '.component' "$out"): $(jq -r '.artifacts | map(.type) | group_by(.) | map("\(length) \(.[0])") | join(", ")' "$out") -> ${out}"
}

charts_of() {
  jq -c --argjson names "$1" 'map(select(.name as $n | $names | index($n)))' "$CHARTS_FILE"
}

write wire-server "$(jq -c '.' "$IMAGE_DIGESTS_FILE")" "$(charts_of '["wire-server","wire-server-enterprise"]')" \
  "$(jq -c '.' "$COMPATIBILITY_FILE")" "$DOCUMENTS_JSON" "${OUT_DIR}/release-manifest.json"

for component in $(jq -r '.[].name' "$CHARTS_FILE"); do
  case $component in
    wire-server | wire-server-enterprise) continue ;;
  esac
  write "$component" '{}' "$(charts_of "[\"${component}\"]")" '{"provides":[],"requires":[],"includes":[]}' '{}' \
    "${OUT_DIR}/release-manifest.${component}.json"
done

{
  echo "dir=${OUT_DIR}"
  echo "files<<EOF_MANIFESTS"
  printf '%s\n' "${files[@]}"
  echo "EOF_MANIFESTS"
} >>"$GITHUB_OUTPUT"
