#!/usr/bin/env bash
# Checks the inputs Concourse sends after a build of wire-server, run from the
# source checkout, and writes them to GITHUB_OUTPUT. The charts come from the
# Makefile of the built commit (make echo-release-charts, the same list a release
# reads): all of them to S3 when that channel is on, and the deployable ones
# (make echo-oci-charts) to OCI. The version is the dev form of
# compute-version.sh, derived from a GA tag or from a pre-release tag.
set -euo pipefail

die() {
  echo "::error::$*"
  exit 1
}

[[ "${VERSION:-}" =~ ^[0-9]+\.[0-9]+\.[0-9]+-((alpha|beta|rc)\.[1-9][0-9]*\.)?[0-9]{14}\.g([0-9a-f]{7})$ ]] \
  || die "version '${VERSION:-}' is not X.Y.Z-<YYYYMMDDHHMMSS>.g<sha7> nor X.Y.Z-<pre>.N.<YYYYMMDDHHMMSS>.g<sha7>"
sha7=${BASH_REMATCH[3]}

[[ "${COMMIT:-}" =~ ^[0-9a-f]{40}$ ]] || die "commit '${COMMIT:-}' is not a full sha"
[[ "${COMMIT:0:7}" == "$sha7" ]] || die "the version names g${sha7} but the commit is ${COMMIT:0:7}"

[[ "${SOURCE_REF:-}" =~ ^[A-Za-z0-9._/-]+$ ]] || die "source_ref '${SOURCE_REF:-}' is not a branch name"
git rev-parse --verify --quiet "refs/remotes/origin/${SOURCE_REF}" >/dev/null \
  || die "branch '${SOURCE_REF}' not found"
git merge-base --is-ancestor "$COMMIT" "refs/remotes/origin/${SOURCE_REF}" \
  || die "commit ${COMMIT} is not on ${SOURCE_REF}"
[[ "$(git rev-parse HEAD)" == "$COMMIT" ]] || die "the source checkout is not at ${COMMIT}"

release_charts=$(make -s echo-release-charts)
[[ -n "$release_charts" ]] || die "make echo-release-charts printed nothing at ${COMMIT}"
oci_list=$(make -s echo-oci-charts)
[[ -n "$oci_list" ]] || die "make echo-oci-charts printed nothing at ${COMMIT}"

charts=()
for chart in $release_charts; do
  [[ "$chart" =~ ^[a-z0-9][a-z0-9-]*$ ]] || die "'${chart}' is not a chart name"
  [[ -f "charts/${chart}/Chart.yaml" ]] || die "charts/${chart}/Chart.yaml not found at ${COMMIT}"
  charts+=("$chart")
done
for chart in wire-server wire-server-enterprise; do
  [[ " ${charts[*]} " == *" ${chart} "* ]] || die "echo-release-charts does not list ${chart}"
done
oci_charts=()
for chart in $oci_list; do
  [[ " ${charts[*]} " == *" ${chart} "* ]] || die "echo-oci-charts lists ${chart}, which echo-release-charts does not"
  oci_charts+=("$chart")
done
for chart in wire-server wire-server-enterprise; do
  [[ " ${oci_charts[*]} " == *" ${chart} "* ]] || die "echo-oci-charts does not list ${chart}"
done
echo "::notice::${#charts[@]} charts to S3: ${charts[*]}"
echo "::notice::${#oci_charts[@]} charts to OCI: ${oci_charts[*]}"

{
  echo "version=${VERSION}"
  echo "commit=${COMMIT}"
  echo "source-ref=${SOURCE_REF}"
  echo "charts=${charts[*]}"
  echo "oci-charts=${oci_charts[*]}"
} >>"$GITHUB_OUTPUT"
