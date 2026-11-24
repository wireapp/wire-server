#!/usr/bin/env bash
# Pushes the packaged charts to the S3 Helm repository S3_REPOSITORY
# (s3://public.wire.com/<S3_REPOSITORY>), and can be run again on the same charts.
# A package already in the bucket is skipped when its sha256 is the one of the local
# file and refused when it differs: a published package is never overwritten. Nothing
# is pushed before every package has been checked. ON_DIFFERENT_SHA256=skip, which the
# dev builds set because helm package does not give the same bytes twice, keeps the
# package of the bucket with a warning instead. Then index.yaml is read back and
# every chart must be listed at VERSION: a chart missing from it (a push interrupted
# between the package and the index, or an index update lost to a concurrent push) is
# pushed again, a few times, then the script fails. The package is then the very file
# the bucket holds, so helm s3 push --force only adds the index entry; helm s3
# reindex would rebuild the index of the whole repository and is not used.
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${CHART_DIR:?}" "${S3_REPOSITORY:?}"
export AWS_REGION=${AWS_REGION:-eu-west-1}
bucket=public.wire.com
helm_s3_version=0.10.0
attempts=3
on_different_sha256=${ON_DIFFERENT_SHA256:-error}

die() {
  echo "::error::$*"
  exit 1
}

[[ -n "${AWS_ACCESS_KEY_ID:-}" && -n "${AWS_SECRET_ACCESS_KEY:-}" ]] \
  || die "the secrets S3_HELM_CHARTS_AWS_ACCESS_KEY_ID and S3_HELM_CHARTS_AWS_SECRET_ACCESS_KEY are not set"
[[ "$on_different_sha256" == error || "$on_different_sha256" == skip ]] \
  || die "ON_DIFFERENT_SHA256 is '${on_different_sha256}', not error or skip"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
read -ra charts <<<"$CHARTS"

if [[ "$(helm plugin list | awk '$1 == "s3" {print $2}')" != "$helm_s3_version" ]]; then
  helm plugin remove s3 >/dev/null 2>&1 || true
  helm plugin install https://github.com/hypnoglow/helm-s3.git --version "v${helm_s3_version}"
fi
helm repo add "$S3_REPOSITORY" "s3://${bucket}/${S3_REPOSITORY}"

sha256_of() {
  sha256sum | awk '{print $1}'
}

object_exists() {
  local error
  if error=$(aws s3api head-object --bucket "$bucket" --key "$1" 2>&1 >/dev/null); then
    return 0
  fi
  [[ "$error" == *"(404)"* || "$error" == *"Not Found"* || "$error" == *NoSuchKey* ]] \
    || die "cannot tell whether s3://${bucket}/$1 exists: $(tr -cd 'A-Za-z0-9 ._:/,()=-' <<<"$error" | cut -c1-300)"
  return 1
}

push() {
  local tgz=$1
  shift
  (cd "$CHART_DIR" && helm s3 push --relative "$@" "$tgz" "$S3_REPOSITORY")
}

missing_from_index() {
  aws s3 cp "s3://${bucket}/${S3_REPOSITORY}/index.yaml" "${work}/index.yaml" >/dev/null || return 1
  python3 -I - "${work}/index.yaml" "$VERSION" "${charts[@]}" <<'EOF'
import sys

import yaml

path, version, *charts = sys.argv[1:]
with open(path, encoding="utf-8") as handle:
    index = yaml.safe_load(handle) or {}
entries = index.get("entries") or {}
for chart in charts:
    if not any(str(entry.get("version")) == version for entry in entries.get(chart) or []):
        print(chart)
EOF
}

to_push=()
kept=()
for chart in "${charts[@]}"; do
  tgz="${chart}-${VERSION}.tgz"
  [[ -f "${CHART_DIR}/${tgz}" ]] || die "${CHART_DIR}/${tgz} not found"
  if object_exists "${S3_REPOSITORY}/${tgz}"; then
    remote=$(aws s3 cp "s3://${bucket}/${S3_REPOSITORY}/${tgz}" - | sha256_of) \
      || die "cannot read s3://${bucket}/${S3_REPOSITORY}/${tgz}"
    local_sha=$(sha256_of <"${CHART_DIR}/${tgz}")
    if [[ "$remote" == "$local_sha" ]]; then
      echo "${tgz} already in ${S3_REPOSITORY} with the same sha256, not pushed again"
    elif [[ "$on_different_sha256" == skip ]]; then
      echo "::warning::${tgz} is already in ${S3_REPOSITORY} with sha256 ${remote}, and the candidate has ${local_sha}: the package of the bucket is kept"
      kept+=("$chart")
    else
      die "${tgz} is already in ${S3_REPOSITORY} with sha256 ${remote}, and the candidate has ${local_sha}: a published package is never overwritten. Nothing was pushed."
    fi
  else
    to_push+=("$tgz")
  fi
done

for tgz in "${to_push[@]}"; do
  push "$tgz"
  echo "${tgz} pushed to ${S3_REPOSITORY}"
done

for ((attempt = 1; attempt <= attempts; attempt++)); do
  listing=$(missing_from_index) || die "cannot read index.yaml of ${S3_REPOSITORY}"
  read -ra missing <<<"${listing//$'\n'/ }"
  if ((${#missing[@]} == 0)); then
    echo "index.yaml of ${S3_REPOSITORY} lists ${#charts[@]} charts at ${VERSION}"
    exit 0
  fi
  echo "::warning::index.yaml of ${S3_REPOSITORY} does not list ${missing[*]} at ${VERSION} (attempt ${attempt}/${attempts})"
  ((attempt < attempts)) || break
  for chart in "${missing[@]}"; do
    [[ " ${kept[*]:-} " != *" ${chart} "* ]] \
      || die "${chart}-${VERSION}.tgz is in ${S3_REPOSITORY} with other bytes than the candidate and is not in index.yaml: it cannot be added without overwriting it"
  done
  for chart in "${missing[@]}"; do
    push "${chart}-${VERSION}.tgz" --force
    echo "${chart}-${VERSION}.tgz added to the index of ${S3_REPOSITORY}"
  done
done
die "index.yaml of ${S3_REPOSITORY} still does not list ${missing[*]} at ${VERSION} after ${attempts} attempts"
