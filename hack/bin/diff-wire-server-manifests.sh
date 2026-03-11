#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  ./hack/bin/diff-wire-server-manifests.sh BEFORE_MANIFEST AFTER_MANIFEST [OUTPUT_DIR]

Examples:
  ./hack/bin/diff-wire-server-manifests.sh /tmp/before.yaml /tmp/after.yaml
  ./hack/bin/diff-wire-server-manifests.sh /tmp/before.yaml /tmp/after.yaml /tmp/wire-server-diff

Splits each manifest into one file per YAML document and compares the resulting
directories with `git diff --no-index`.

Generated file names are based on:
  <namespace>-<kind>-<metadata.name>.yaml

If multiple documents resolve to the same base name, a numeric suffix is added.

If OUTPUT_DIR is omitted, a temporary directory is used.

Optional environment variables:
  DIFF_OUTPUT_FILE=/tmp/wire-server-manifest.diff
EOF
}

if [[ $# -lt 2 || $# -gt 3 ]]; then
  usage
  exit 2
fi

before_manifest=$1
after_manifest=$2
output_dir=${3:-}
diff_output_file=${DIFF_OUTPUT_FILE:-}

if [[ ! -f "$before_manifest" ]]; then
  echo "Missing before manifest: $before_manifest" >&2
  exit 2
fi

if [[ ! -f "$after_manifest" ]]; then
  echo "Missing after manifest: $after_manifest" >&2
  exit 2
fi

if ! command -v yq >/dev/null 2>&1; then
  echo "Missing dependency: yq" >&2
  exit 2
fi

if ! command -v git >/dev/null 2>&1; then
  echo "Missing dependency: git" >&2
  exit 2
fi

if [[ -z "$output_dir" ]]; then
  output_dir=$(mktemp -d)
  cleanup_output_dir=true
else
  mkdir -p "$output_dir"
  cleanup_output_dir=false
fi

# shellcheck disable=SC2329
cleanup() {
  if [[ "${cleanup_output_dir}" == "true" ]]; then
    rm -rf "$output_dir"
  fi
}
trap cleanup EXIT

before_dir="$output_dir/before"
after_dir="$output_dir/after"

rm -rf "$before_dir" "$after_dir"
mkdir -p "$before_dir" "$after_dir"

sanitize_filename_part() {
  local value=$1
  value=${value// /_}
  value=${value//\//_}
  value=${value//:/_}
  value=${value//[^[:alnum:]._-]/-}
  printf '%s\n' "$value"
}

render_manifest_dir() {
  local manifest=$1
  local target_dir=$2
  local count=0

  declare -A seen=()

  while IFS= read -r -d '' doc; do
    [[ -z "$doc" ]] && continue

    local kind
    local name
    local namespace
    local base_name
    local suffix
    local file_name

    kind=$(yq -r '.kind // ""' <<<"$doc" 2>/dev/null || true)
    name=$(yq -r '.metadata.name // ""' <<<"$doc" 2>/dev/null || true)
    namespace=$(yq -r '.metadata.namespace // "default"' <<<"$doc" 2>/dev/null || true)

    # Skip empty/comment-only or otherwise unparsable YAML documents.
    if [[ -z "$kind" || -z "$name" ]]; then
      continue
    fi

    base_name="$(sanitize_filename_part "$namespace")-$(sanitize_filename_part "$kind")-$(sanitize_filename_part "$name")"
    suffix=${seen["$base_name"]:-0}

    if [[ "$suffix" -eq 0 ]]; then
      file_name="$base_name.yaml"
    else
      file_name="$base_name-$suffix.yaml"
    fi
    seen["$base_name"]=$((suffix + 1))

    printf '%s' "$doc" >"$target_dir/$file_name"
    count=$((count + 1))
  done < <(
    awk '
      BEGIN { doc = "" }
      /^---[[:space:]]*$/ {
        if (doc != "") {
          printf "%s%c", doc, 0
          doc = ""
        }
        next
      }
      { doc = doc $0 ORS }
      END {
        if (doc != "") {
          printf "%s%c", doc, 0
        }
      }
    ' "$manifest"
  )

  echo "$count"
}

before_count=$(render_manifest_dir "$before_manifest" "$before_dir")
after_count=$(render_manifest_dir "$after_manifest" "$after_dir")

echo "Before manifest resources: $before_count"
echo "After manifest resources: $after_count"
echo "Before directory: $before_dir"
echo "After directory: $after_dir"
if [[ -n "$diff_output_file" ]]; then
  echo "Diff output file: $diff_output_file"
fi
echo

set +e
if [[ -n "$diff_output_file" ]]; then
  mkdir -p "$(dirname "$diff_output_file")"
  git diff --no-index -- "$before_dir" "$after_dir" >"$diff_output_file"
else
  git diff --no-index -- "$before_dir" "$after_dir"
fi
diff_exit=$?
set -e

if [[ $diff_exit -eq 0 ]]; then
  echo "No differences found."
  exit 0
fi

if [[ $diff_exit -eq 1 ]]; then
  echo
  echo "Differences found."
  exit 1
fi

echo "git diff failed with exit code $diff_exit" >&2
exit "$diff_exit"
