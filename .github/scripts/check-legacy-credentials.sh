#!/usr/bin/env bash
# Checks, before anything is published, that the credentials of the legacy channels are
# set and work as far as a read can tell: the AWS keys read index.yaml of the S3 Helm
# repository S3_REPOSITORY, and the token of wire-builds lists WIRE_BUILDS_BRANCH.
# Nothing is written and no secret is printed.
set -euo pipefail

: "${S3_REPOSITORY:?}" "${WIRE_BUILDS_BRANCH:?}"
export AWS_REGION=${AWS_REGION:-eu-west-1}
bucket=public.wire.com

die() {
  echo "::error::$*"
  exit 1
}

missing=()
[[ -n "${AWS_ACCESS_KEY_ID:-}" ]] || missing+=(S3_HELM_CHARTS_AWS_ACCESS_KEY_ID)
[[ -n "${AWS_SECRET_ACCESS_KEY:-}" ]] || missing+=(S3_HELM_CHARTS_AWS_SECRET_ACCESS_KEY)
[[ -n "${WIRE_BUILDS_TOKEN:-}" ]] || missing+=(WIRE_BUILDS_WRITE_ACCESS_GH_TOKEN)
if ((${#missing[@]})); then
  die "secrets not set: ${missing[*]}. A release from develop also goes to the S3 Helm repository and to wire-builds; nothing was published."
fi

error=$(mktemp)
trap 'rm -f "$error"' EXIT
clean() {
  tr -cd 'A-Za-z0-9 ._:/,()=-' <"$error" | tail -c 300
}

aws s3 cp "s3://${bucket}/${S3_REPOSITORY}/index.yaml" - >/dev/null 2>"$error" \
  || die "the AWS keys cannot read s3://${bucket}/${S3_REPOSITORY}/index.yaml: $(clean). Nothing was published."

GIT_ASKPASS="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/wire-builds-askpass.sh"
export GIT_ASKPASS GIT_TERMINAL_PROMPT=0
git ls-remote --exit-code --heads "https://x-access-token@github.com/wireapp/wire-builds.git" "$WIRE_BUILDS_BRANCH" >/dev/null 2>"$error" \
  || die "the token WIRE_BUILDS_WRITE_ACCESS_GH_TOKEN cannot list the branch ${WIRE_BUILDS_BRANCH} of wireapp/wire-builds: $(clean). Nothing was published."

echo "::notice::the S3 keys read ${S3_REPOSITORY}/index.yaml and the wire-builds token reads the branch ${WIRE_BUILDS_BRANCH}"
