#!/usr/bin/env bash
# Reads the chart lists of the source checkout, run from it: every chart of a release
# (make echo-release-charts), the deployable ones among them (make echo-oci-charts),
# and the others, which only go to the S3 Helm repository and wire-builds. Writes
# release, oci and legacy (space separated) to GITHUB_OUTPUT.
set -euo pipefail

die() {
  echo "::error::$*"
  exit 1
}

release=$(make -s echo-release-charts)
oci=$(make -s echo-oci-charts)
[[ -n "$release" ]] || die "make echo-release-charts printed nothing"
[[ -n "$oci" ]] || die "make echo-oci-charts printed nothing"

for chart in $release $oci; do
  [[ "$chart" =~ ^[a-z0-9][a-z0-9-]*$ ]] || die "'${chart}' is not a chart name"
done
# shellcheck disable=SC2086
[[ -z "$(printf '%s\n' $release | sort | uniq -d)" ]] || die "echo-release-charts lists a chart twice"
# shellcheck disable=SC2086
[[ -z "$(printf '%s\n' $oci | sort | uniq -d)" ]] || die "echo-oci-charts lists a chart twice"
for chart in $oci; do
  [[ " ${release} " == *" ${chart} "* ]] || die "echo-oci-charts lists ${chart}, which echo-release-charts does not"
done

legacy=()
for chart in $release; do
  [[ " ${oci} " == *" ${chart} "* ]] || legacy+=("$chart")
done

echo "::notice::$(wc -w <<<"$release" | tr -d ' ') charts in a release, $(wc -w <<<"$oci" | tr -d ' ') deployable (OCI), ${#legacy[@]} for S3 and wire-builds only"
{
  echo "release=${release}"
  echo "oci=${oci}"
  echo "legacy=${legacy[*]}"
} >>"$GITHUB_OUTPUT"
