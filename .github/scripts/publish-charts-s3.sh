#!/usr/bin/env bash
# Pushes the packaged charts to the S3 Helm repository S3_REPOSITORY
# (s3://public.wire.com/<S3_REPOSITORY>), skipping a package already there.
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${CHART_DIR:?}" "${S3_REPOSITORY:?}"
: "${AWS_ACCESS_KEY_ID:?}" "${AWS_SECRET_ACCESS_KEY:?}"
export AWS_REGION=${AWS_REGION:-eu-west-1}
bucket=public.wire.com
helm_s3_version=0.10.0

if [[ "$(helm plugin list | awk '$1 == "s3" {print $2}')" != "$helm_s3_version" ]]; then
  helm plugin remove s3 >/dev/null 2>&1 || true
  helm plugin install https://github.com/hypnoglow/helm-s3.git --version "v${helm_s3_version}"
fi
helm repo add "$S3_REPOSITORY" "s3://${bucket}/${S3_REPOSITORY}"

read -ra charts <<<"$CHARTS"
for chart in "${charts[@]}"; do
  tgz="${chart}-${VERSION}.tgz"
  if aws s3api head-object --bucket "$bucket" --key "${S3_REPOSITORY}/${tgz}" >/dev/null 2>&1; then
    echo "${tgz} already in ${S3_REPOSITORY}, skipped"
    continue
  fi
  (cd "$CHART_DIR" && helm s3 push --relative "$tgz" "$S3_REPOSITORY")
  echo "${tgz} pushed to ${S3_REPOSITORY}"
done
