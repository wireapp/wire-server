#!/usr/bin/env bash
# Pins the published charts at VERSION in wire-builds build.json on WIRE_BUILDS_BRANCH,
# from the S3 Helm repository, the way Concourse did (one commit, prerelease bump).
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${COMMIT:?}" "${S3_REPOSITORY:?}" "${WIRE_BUILDS_BRANCH:?}"
: "${SERVER_URL:?}" "${REPOSITORY:?}"
[[ -n "${WIRE_BUILDS_TOKEN:-}" ]] || {
  echo "::error::WIRE_BUILDS_WRITE_ACCESS_GH_TOKEN is not set: cannot update wire-builds"
  exit 1
}

repo_url="https://s3-eu-west-1.amazonaws.com/public.wire.com/${S3_REPOSITORY}"
commit_url="${SERVER_URL}/${REPOSITORY}/commit/${COMMIT}"
read -ra charts <<<"$CHARTS"

work=$(mktemp -d)
git clone --quiet --depth 1 --branch "$WIRE_BUILDS_BRANCH" \
  "https://x-access-token:${WIRE_BUILDS_TOKEN}@github.com/wireapp/wire-builds.git" "$work/wire-builds"
cd "$work/wire-builds"
git config user.email "zebot@users.noreply.github.com"
git config user.name "Zebot"

for attempt in 1 2 3; do
  git fetch --quiet --depth 1 origin "$WIRE_BUILDS_BRANCH"
  git reset --quiet --hard FETCH_HEAD
  if [[ "$(jq -r '.helmCharts["wire-server"].version // ""' build.json)" == "$VERSION" ]]; then
    echo "wire-builds:${WIRE_BUILDS_BRANCH} already pins wire-server ${VERSION}"
    exit 0
  fi
  b=$(cat build.json)
  for chart in "${charts[@]}"; do
    b=$(./bin/set-chart-fields "$chart" \
      "version=${VERSION}" \
      "repo=${repo_url}" \
      "meta.commit=${COMMIT}" \
      "meta.commitURL=${commit_url}" <<<"$b")
  done
  ./bin/bump-prerelease <<<"$b" >build.json
  git add build.json
  git commit --quiet -m "Bump wire-server charts to ${VERSION}"
  if git push --quiet origin "HEAD:${WIRE_BUILDS_BRANCH}"; then
    echo "wire-builds:${WIRE_BUILDS_BRANCH} pins wire-server ${VERSION}"
    exit 0
  fi
  echo "::notice::push to wire-builds:${WIRE_BUILDS_BRANCH} rejected (attempt ${attempt}/3), retrying"
done
echo "::error::could not update wire-builds:${WIRE_BUILDS_BRANCH}"
exit 1
