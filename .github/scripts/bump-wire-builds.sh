#!/usr/bin/env bash
# Pins the published charts at VERSION in wire-builds build.json on WIRE_BUILDS_BRANCH,
# from OCI (with DIGESTS_FILE) or the S3 Helm repository. Updates build.json and
# bumps its prerelease version in one commit, then appends
# wire-builds=pinned|unchanged|ahead to GITHUB_OUTPUT. Run again on a release it already
# pinned, it changes nothing.
# With WIRE_BUILDS_GUARD=true, which the releases set and the dev builds do not, a pin
# never moves backwards. On dev, the pin is skipped with a warning when the pinned
# wire-server commit is known in the checkout SOURCE_DIR and is not COMMIT or one of its
# ancestors: dev is ahead. On main, the pin is refused when the pinned wire-server
# version is greater than VERSION.
set -euo pipefail

: "${VERSION:?}" "${CHARTS:?}" "${WIRE_BUILDS_BRANCH:?}"
[[ -n "${WIRE_BUILDS_TOKEN:-}" ]] || {
  echo "::error::WIRE_BUILDS_WRITE_ACCESS_GH_TOKEN is not set: cannot update wire-builds"
  exit 1
}
guard=${WIRE_BUILDS_GUARD:-false}
output=${GITHUB_OUTPUT:-/dev/null}

die() {
  echo "::error::$*"
  exit 1
}

read -ra charts <<<"$CHARTS"
if [[ -n ${OCI_REPOSITORY:-} ]]; then
  : "${DIGESTS_FILE:?}"
  declare -A digests
  for chart in "${charts[@]}"; do
    digest=$(jq -r --arg chart "$chart" '.[$chart] // empty' "$DIGESTS_FILE")
    [[ "$digest" =~ ^sha256:[0-9a-f]{64}$ ]] || die "no valid digest for ${chart} in ${DIGESTS_FILE}"
    digests[$chart]=$digest
  done
else
  : "${S3_REPOSITORY:?}" "${COMMIT:?}" "${SERVER_URL:?}" "${REPOSITORY:?}"
  repo_url="https://s3-eu-west-1.amazonaws.com/public.wire.com/${S3_REPOSITORY}"
  commit_url="${SERVER_URL}/${REPOSITORY}/commit/${COMMIT}"
fi

GIT_ASKPASS="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/wire-builds-askpass.sh"
export GIT_ASKPASS GIT_TERMINAL_PROMPT=0
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
git clone --quiet --depth 1 --branch "$WIRE_BUILDS_BRANCH" \
  "https://x-access-token@github.com/wireapp/wire-builds.git" "$work/wire-builds"
cd "$work/wire-builds"
git config user.email "zebot@users.noreply.github.com"
git config user.name "Zebot"

# Returns 0 when the pin may go ahead, 1 when dev is already ahead.
guard_allows() {
  local pinned_version pinned_commit status
  case "$WIRE_BUILDS_BRANCH" in
    dev)
      [[ -n "${SOURCE_DIR:-}" ]] || die "SOURCE_DIR, a checkout of wire-server, is required to guard the pin on dev"
      pinned_commit=$(jq -r '.helmCharts["wire-server"].meta.commit // ""' build.json)
      if [[ -z "$pinned_commit" ]]; then
        # OCI dev pins carry the source commit abbreviation in their version.
        pinned_version=$(jq -r '.helmCharts["wire-server"].version // ""' build.json)
        if [[ "$pinned_version" =~ \.g([0-9a-f]{7})$ ]]; then
          pinned_commit=$(git -C "$SOURCE_DIR" rev-parse --verify "${BASH_REMATCH[1]}^{commit}" 2>/dev/null || true)
        fi
      fi
      if [[ ! "$pinned_commit" =~ ^[0-9a-f]{40}$ ]] || ! git -C "$SOURCE_DIR" cat-file -e "${pinned_commit}^{commit}" 2>/dev/null; then
        echo "::notice::wire-builds:dev pins wire-server at '${pinned_commit}', which is not in the checkout: the pin cannot be compared and goes ahead"
        return 0
      fi
      status=0
      git -C "$SOURCE_DIR" merge-base --is-ancestor "$pinned_commit" "$COMMIT" || status=$?
      case "$status" in
        0) return 0 ;;
        1)
          echo "::warning::wire-builds:dev already pins wire-server at ${pinned_commit}, which is not ${COMMIT} nor one of its ancestors: dev is ahead of v${VERSION} and is left as it is"
          return 1
          ;;
        *) die "cannot compare ${pinned_commit} with ${COMMIT} in ${SOURCE_DIR}" ;;
      esac
      ;;
    main)
      pinned_version=$(jq -r '.helmCharts["wire-server"].version // ""' build.json)
      [[ -n "$pinned_version" ]] || return 0
      ./bin/semver validate "$pinned_version" >/dev/null || die "wire-builds:main pins wire-server at '${pinned_version}', which is not a version: the pin cannot be compared"
      [[ "$(./bin/semver compare "$pinned_version" "$VERSION")" -le 0 ]] \
        || die "wire-builds:main pins wire-server ${pinned_version}, which is greater than ${VERSION}: refusing to move it backwards"
      return 0
      ;;
    *) die "the pin is guarded on dev and main, not on '${WIRE_BUILDS_BRANCH}'" ;;
  esac
}

for attempt in 1 2 3; do
  git fetch --quiet --depth 1 origin "$WIRE_BUILDS_BRANCH"
  git reset --quiet --hard FETCH_HEAD
  if [[ -z ${OCI_REPOSITORY:-} && "$(jq -r '.helmCharts["wire-server"].version // ""' build.json)" == "$VERSION" ]]; then
    echo "wire-builds:${WIRE_BUILDS_BRANCH} already pins wire-server ${VERSION}"
    echo "wire-builds=unchanged" >>"$output"
    exit 0
  fi
  if [[ "$guard" == true ]] && ! guard_allows; then
    echo "wire-builds=ahead" >>"$output"
    exit 0
  fi
  b=$(cat build.json)
  for chart in "${charts[@]}"; do
    if [[ -n ${OCI_REPOSITORY:-} ]]; then
      b=$(./bin/set-chart-fields "$chart" \
        "version=${VERSION}" \
        "repo=oci://${OCI_REPOSITORY%/}/${chart}" \
        "meta.digest=${digests[$chart]}" <<<"$b")
    else
      b=$(./bin/set-chart-fields "$chart" \
        "version=${VERSION}" \
        "repo=${repo_url}" \
        "meta.commit=${COMMIT}" \
        "meta.commitURL=${commit_url}" <<<"$b")
    fi
  done
  if [[ -n ${OCI_REPOSITORY:-} ]] && jq -e --argjson updated "$b" '.helmCharts == $updated.helmCharts' build.json >/dev/null; then
    echo "wire-builds:${WIRE_BUILDS_BRANCH} already pins the OCI charts for wire-server ${VERSION}"
    echo "wire-builds=unchanged" >>"$output"
    exit 0
  fi
  ./bin/bump-prerelease <<<"$b" >build.json
  git add build.json
  git commit --quiet -m "Bump wire-server charts to ${VERSION}"
  if git push --quiet origin "HEAD:${WIRE_BUILDS_BRANCH}"; then
    echo "wire-builds:${WIRE_BUILDS_BRANCH} pins wire-server ${VERSION}"
    echo "wire-builds=pinned" >>"$output"
    exit 0
  fi
  echo "::notice::push to wire-builds:${WIRE_BUILDS_BRANCH} rejected (attempt ${attempt}/3), retrying"
done
die "could not update wire-builds:${WIRE_BUILDS_BRANCH}"
