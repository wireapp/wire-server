#!/usr/bin/env bash
# Promotes every image of the release to its release tag by digest, with the
# promote-image script of wire-rm-actions (crane tag, no rebuild, idempotent, a
# release tag already at another digest is a hard failure), then checks that the
# release tag resolves to the digest of the manifest.
set -euo pipefail

: "${IMAGES_JSON:?}" "${TAG:?}" "${PROMOTE_IMAGE:?}"
[[ -f "$PROMOTE_IMAGE" ]] || { echo "::error::promote-image script not found at ${PROMOTE_IMAGE}"; exit 1; }
command -v crane >/dev/null || { echo "::error::crane is not installed"; exit 1; }

count=0
while IFS=$'\t' read -r repository digest; do
  echo "::group::${repository}:${TAG}"
  REPOSITORY=$repository DIGEST=$digest TAG=$TAG REGISTRY_USER="" REGISTRY_PASSWORD="" GITHUB_OUTPUT=/dev/null bash "$PROMOTE_IMAGE"
  final=$(crane digest "${repository}:${TAG}")
  [[ "$final" == "$digest" ]] || { echo "::error::${repository}:${TAG} resolves to ${final}, expected ${digest}"; exit 1; }
  count=$((count + 1))
  echo "::endgroup::"
done < <(jq -r 'to_entries[] | [.key, .value] | @tsv' <<<"$IMAGES_JSON")
echo "::notice::${count} images promoted to ${TAG}"
