#!/usr/bin/env bash
# Verifies the candidate downloaded from the draft release against its manifests
# before anything is published: the wire-server manifest names the version, the
# commit the draft targets and a releasable branch; every chart it lists is in
# ASSET_DIR with the recorded sha256; every image digest has the sha256 form; and
# every follower chart has its own manifest. Writes what publication needs to
# GITHUB_OUTPUT.
set -euo pipefail

: "${MANIFEST:?}" "${ASSET_DIR:?}" "${VERSION:?}" "${TARGET_COMMIT:?}"
RELEASE_BRANCHES=${RELEASE_BRANCHES:-'^(dev|release/[0-9]+\.[0-9]+)$'}

die() {
  echo "::error::$*"
  exit 1
}

[[ -f "$MANIFEST" ]] || die "manifest ${MANIFEST} not found"
schema="wire.component-release-manifest/v1alpha1"
[[ "$(jq -r '.schemaVersion // ""' "$MANIFEST")" == "$schema" ]] || die "${MANIFEST} is not a ${schema} manifest"
[[ "$(jq -r '.component // ""' "$MANIFEST")" == wire-server ]] || die "${MANIFEST} is not the wire-server manifest"
[[ "$(jq -r '.version // ""' "$MANIFEST")" == "$VERSION" ]] || die "manifest version '$(jq -r '.version' "$MANIFEST")' is not ${VERSION}"
kind=$(jq -r '.kind // ""' "$MANIFEST")
type=ga
if [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+-(alpha|beta|rc)\.[1-9][0-9]*$ ]]; then
  type=${BASH_REMATCH[1]}
  [[ "$kind" == prerelease ]] || die "version ${VERSION} is a pre-release but the manifest kind is '${kind}'"
  [[ "$(jq -r '.stage // ""' "$MANIFEST")" == "$type" ]] || die "manifest stage is not ${type}"
elif [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  [[ "$kind" == ga ]] || die "version ${VERSION} is a GA but the manifest kind is '${kind}'"
else
  die "version '${VERSION}' is neither X.Y.Z nor X.Y.Z-<alpha|beta|rc>.N"
fi
source_commit=$(jq -r '.source.commit // ""' "$MANIFEST")
source_ref=$(jq -r '.source.ref // ""' "$MANIFEST")
[[ "$source_commit" == "$TARGET_COMMIT" ]] || die "manifest source commit ${source_commit} is not the draft target ${TARGET_COMMIT}"
[[ "$source_ref" =~ $RELEASE_BRANCHES ]] || die "manifest source ref '${source_ref}' is not a releasable branch"
[[ "$(jq -r '.source.tag // ""' "$MANIFEST")" == "v${VERSION}" ]] || die "manifest source tag is not v${VERSION}"

images=$(jq -c '[.artifacts[] | select(.type == "image")] | map({key: .repository, value: .digest}) | from_entries' "$MANIFEST")
jq -e 'length > 0 and all(.[]; test("^sha256:[0-9a-f]{64}$"))' <<<"$images" >/dev/null || die "the manifest lists no image, or a digest is not sha256"
jq -e --arg v "$VERSION" '[.artifacts[] | select(.type == "image")] | all(.tag == $v and (.candidate_tag | length > 0) and (.repository | startswith("quay.io/wire/")))' "$MANIFEST" >/dev/null \
  || die "an image artifact does not carry the version, a candidate tag and a quay.io/wire repository"
dev_version=$(jq -r '[.artifacts[] | select(.type == "image")][0].candidate_tag' "$MANIFEST")

channel=""
charts=()
while IFS=$'\t' read -r name repository file sha256; do
  [[ "$repository" =~ ^oci://quay\.io/wire/charts/(dev|stable)/([a-z0-9-]+)$ ]] || die "chart repository '${repository}' is not oci://quay.io/wire/charts/(dev|stable)/<chart>"
  [[ -z "$channel" || "$channel" == "${BASH_REMATCH[1]}" ]] || die "the charts of the manifest use several channels"
  channel=${BASH_REMATCH[1]}
  [[ "$name" == "${BASH_REMATCH[2]}" ]] || die "chart ${name} is pushed to the repository of ${BASH_REMATCH[2]}"
  [[ "$file" == "${name}-${VERSION}.tgz" ]] || die "chart file '${file}' is not ${name}-${VERSION}.tgz"
  [[ -f "${ASSET_DIR}/${file}" ]] || die "chart package ${file} was not downloaded"
  actual=$(sha256sum "${ASSET_DIR}/${file}" | awk '{print $1}')
  [[ "$actual" == "$sha256" ]] || die "${file} sha256 ${actual} does not match the manifest ${sha256}: refusing to publish"
  charts+=("$name")
done < <(jq -r --arg v "$VERSION" '.artifacts[] | select(.type == "chart") | [(.file | rtrimstr("-" + $v + ".tgz")), .repository, .file, .sha256] | @tsv' "$MANIFEST")
for chart in wire-server wire-server-enterprise; do
  [[ " ${charts[*]} " == *" ${chart} "* ]] || die "the wire-server manifest does not list the ${chart} chart"
done

components=()
manifests=("$MANIFEST")
for file in "${ASSET_DIR}"/release-manifest.*.json; do
  [[ -f "$file" ]] || continue
  component=$(basename "$file" .json)
  component=${component#release-manifest.}
  [[ "$(jq -r '.schemaVersion // ""' "$file")" == "$schema" ]] || die "${file} is not a ${schema} manifest"
  [[ "$(jq -r '.component // ""' "$file")" == "$component" ]] || die "${file} does not declare ${component}"
  [[ "$(jq -r '.version // ""' "$file")" == "$VERSION" && "$(jq -r '.source.commit // ""' "$file")" == "$TARGET_COMMIT" ]] \
    || die "${file} is not for ${VERSION} at ${TARGET_COMMIT}"
  while IFS=$'\t' read -r repository cfile sha256; do
    [[ "$repository" == "oci://quay.io/wire/charts/${channel}/${component}" ]] || die "${file}: chart repository '${repository}' is not the ${channel} repository of ${component}"
    [[ "$cfile" == "${component}-${VERSION}.tgz" && -f "${ASSET_DIR}/${cfile}" ]] || die "${file}: chart package ${cfile} missing"
    actual=$(sha256sum "${ASSET_DIR}/${cfile}" | awk '{print $1}')
    [[ "$actual" == "$sha256" ]] || die "${cfile} sha256 ${actual} does not match ${file}: refusing to publish"
  done < <(jq -r '.artifacts[] | select(.type == "chart") | [.repository, .file, .sha256] | @tsv' "$file")
  components+=("$component")
  manifests+=("$file")
  charts+=("$component")
done
((${#components[@]})) || die "no follower manifest (release-manifest.<component>.json) in ${ASSET_DIR}"

declaration=""
[[ -f "${ASSET_DIR}/release-declaration.json" ]] && declaration="${ASSET_DIR}/release-declaration.json"
if [[ "$type" == ga && -z "$declaration" ]]; then
  die "a GA needs release-declaration.json among the draft assets"
fi

echo "::notice::candidate v${VERSION} (${type}) from ${source_ref} at ${source_commit}: $(jq 'length' <<<"$images") images from dev build ${dev_version}, ${#charts[@]} charts to charts/${channel}, ${#components[@]} follower components"
{
  echo "version=${VERSION}"
  echo "type=${type}"
  echo "kind=${kind}"
  echo "source-ref=${source_ref}"
  echo "source-commit=${source_commit}"
  echo "dev-version=${dev_version}"
  echo "channel=${channel}"
  echo "images-json=${images}"
  echo "charts=${charts[*]}"
  echo "components=${components[*]}"
  echo "declaration-file=${declaration}"
  echo "manifests<<EOF_MANIFESTS"
  printf '%s\n' "${manifests[@]}"
  echo "EOF_MANIFESTS"
} >>"$GITHUB_OUTPUT"
