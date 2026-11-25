#!/usr/bin/env bash
# Finds the dev build of the source commit on quay.io: the build Concourse made
# of it. Its version was computed when the commit was built, from the tags that
# existed then. A release tagged since, on the commit or before it, changes what
# compute-version.sh answers today (COMPUTED_VERSION), not the name of the
# images. That version is tried first; when no image carries it, the build is
# found by the part of its version that only depends on the commit, the
# <ts>.g<sha7> ending. Writes version=<dev build version> to GITHUB_OUTPUT.
set -euo pipefail

: "${IMAGES:?}" "${COMPUTED_VERSION:?}" "${SOURCE_COMMIT:?}"

command -v crane >/dev/null || { echo "::error::crane is not installed"; exit 1; }

read -ra images <<<"$IMAGES"
image=${images[0]}

if [[ ! "$COMPUTED_VERSION" =~ ([0-9]{14})\.g([0-9a-f]{7})$ ]]; then
  echo "::error::${COMPUTED_VERSION} does not end in <ts>.g<sha7>: not the version of a dev build of a release branch"
  exit 1
fi
ending="${BASH_REMATCH[1]}\\.g${BASH_REMATCH[2]}"

if crane digest "${image}:${COMPUTED_VERSION}" >/dev/null 2>&1; then
  version=$COMPUTED_VERSION
else
  errors=$(mktemp)
  trap 'rm -f "$errors"' EXIT
  if ! tags=$(crane ls "$image" 2>"$errors"); then
    echo "::error::cannot list the tags of ${image}: $(tail -n 1 "$errors" | tr -cd 'A-Za-z0-9 ._:/,()=-' | cut -c1-300)"
    exit 1
  fi
  mapfile -t found < <(grep -E "^[0-9]+\.[0-9]+\.[0-9]+-((alpha|beta|rc)\.[0-9]+\.)?${ending}\$" <<<"$tags" | sort -V)
  if ((${#found[@]} == 0)); then
    echo "::error::no dev build of ${SOURCE_COMMIT}: ${image} has no ${COMPUTED_VERSION} and no other tag of that commit. Concourse must have built the source commit before a release is prepared from it."
    exit 1
  fi
  version=${found[-1]}
  echo "::notice::the dev build of ${SOURCE_COMMIT:0:7} is ${version}: it was built before the tags that now give ${COMPUTED_VERSION}"
  if ((${#found[@]} > 1)); then
    echo "::notice::${#found[@]} builds of that commit (${found[*]}), the highest is used"
  fi
fi

echo "Dev build of ${SOURCE_COMMIT}: ${version}"
if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  echo "version=${version}" >>"$GITHUB_OUTPUT"
fi
