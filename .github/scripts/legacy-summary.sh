#!/usr/bin/env bash
# Prints, in markdown, where the charts of a release went besides Quay and the release
# managers: the S3 Helm repository and wire-builds, or the reason they did not go there.
# RECOVERY is the workflow that does it again. S3_RESULT and WIRE_BUILDS_RESULT are the
# outputs of publish-legacy.sh, OUTCOME the outcome of its step.
set -euo pipefail

: "${VERSION:?}" "${SOURCE_REF:?}" "${LEGACY:?}" "${RECOVERY:?}"
OUTCOME=${OUTCOME:-}
S3_REPOSITORY=${S3_REPOSITORY:-}
WIRE_BUILDS_BRANCH=${WIRE_BUILDS_BRANCH:-}
S3_RESULT=${S3_RESULT:-}
WIRE_BUILDS_RESULT=${WIRE_BUILDS_RESULT:-}

echo
if [[ "$LEGACY" != true ]]; then
  echo "Released from \`${SOURCE_REF}\`: the images and the charts went to Quay and the release managers only, not to the S3 Helm repository nor to wire-builds."
  exit 0
fi

if [[ -n "$S3_RESULT" ]]; then
  echo "Charts pushed to the S3 Helm repository \`${S3_REPOSITORY}\`."
else
  echo "Charts not pushed to the S3 Helm repository \`${S3_REPOSITORY}\`."
fi
case "$WIRE_BUILDS_RESULT" in
  pinned) echo "Pinned in wire-builds \`${WIRE_BUILDS_BRANCH}\`." ;;
  unchanged) echo "wire-builds \`${WIRE_BUILDS_BRANCH}\` already pinned v${VERSION}." ;;
  ahead) echo "wire-builds \`${WIRE_BUILDS_BRANCH}\` was not changed: it already pins a newer wire-server commit." ;;
  *) echo "Not pinned in wire-builds \`${WIRE_BUILDS_BRANCH}\`." ;;
esac

if [[ "$OUTCOME" != success ]]; then
  echo
  echo "> :warning: The publication to the S3 Helm repository and wire-builds did not complete (${OUTCOME:-not run}). The release itself is published on Quay and declared to the release managers. Run the workflow \"${RECOVERY}\" with the version ${VERSION} to complete it: it can be run again safely."
elif [[ "$WIRE_BUILDS_RESULT" == ahead ]]; then
  echo
  echo "> :warning: wire-builds \`${WIRE_BUILDS_BRANCH}\` was not pinned to v${VERSION} because it already pins a newer wire-server commit. The workflow \"${RECOVERY}\" with the version ${VERSION} tries again, and skips again while \`${WIRE_BUILDS_BRANCH}\` is ahead."
fi
