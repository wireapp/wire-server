#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  ./hack/bin/diff-wire-server-manifests.sh BEFORE_MANIFEST AFTER_MANIFEST [OUTPUT_DIR]

Examples:
  ./hack/bin/diff-wire-server-manifests.sh /tmp/before.yaml /tmp/after.yaml
  ./hack/bin/diff-wire-server-manifests.sh /tmp/before.yaml /tmp/after.yaml /tmp/wire-server-diff

Compares two rendered wire-server manifest files and highlights:
  - resource inventory changes
  - workload image changes
  - workload pod template changes
  - service spec changes
  - secret payload/spec changes
  - per-service ConfigMap payload changes for core wire services

If OUTPUT_DIR is omitted, a temporary directory is used.
EOF
}

if [[ $# -lt 2 || $# -gt 3 ]]; then
  usage
  exit 2
fi

before_manifest=$1
after_manifest=$2
output_dir=${3:-}

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

overall_status=0

compare_files() {
  local title=$1
  local before_file=$2
  local after_file=$3

  echo
  echo "== $title =="
  if diff -u "$before_file" "$after_file"; then
    echo "No changes"
  else
    overall_status=1
  fi
}

write_or_missing() {
  local expr=$1
  local input_file=$2
  local output_file=$3

  yq -r "$expr" "$input_file" >"$output_file"
  if [[ ! -s "$output_file" ]]; then
    printf '__MISSING__\n' >"$output_file"
  fi
}

services=(
  background-worker
  brig
  cannon
  cargohold
  galley
  gundeck
  proxy
  spar
)

yq -r '.kind + " " + (.metadata.namespace // "default") + "/" + .metadata.name' "$before_manifest" \
  | sort >"$output_dir/before.ids"
yq -r '.kind + " " + (.metadata.namespace // "default") + "/" + .metadata.name' "$after_manifest" \
  | sort >"$output_dir/after.ids"
compare_files "Resource Inventory" "$output_dir/before.ids" "$output_dir/after.ids"

yq -r '
  select(.kind == "Deployment" or .kind == "StatefulSet") |
  .kind + " " + .metadata.name + " -> " + ([.spec.template.spec.containers[].image] | join(", "))
' "$before_manifest" | sort >"$output_dir/before.images"
yq -r '
  select(.kind == "Deployment" or .kind == "StatefulSet") |
  .kind + " " + .metadata.name + " -> " + ([.spec.template.spec.containers[].image] | join(", "))
' "$after_manifest" | sort >"$output_dir/after.images"
compare_files "Workload Images" "$output_dir/before.images" "$output_dir/after.images"

yq -c '
  select(.kind == "Deployment" or .kind == "StatefulSet") |
  {
    kind: .kind,
    name: .metadata.name,
    podSpec: .spec.template.spec
  }
' "$before_manifest" | sort >"$output_dir/before.workloads"
yq -c '
  select(.kind == "Deployment" or .kind == "StatefulSet") |
  {
    kind: .kind,
    name: .metadata.name,
    podSpec: .spec.template.spec
  }
' "$after_manifest" | sort >"$output_dir/after.workloads"
compare_files "Workload Pod Specs" "$output_dir/before.workloads" "$output_dir/after.workloads"

yq -c '
  select(.kind == "Service") |
  {
    name: .metadata.name,
    type: .spec.type,
    ports: .spec.ports,
    selector: .spec.selector
  }
' "$before_manifest" | sort >"$output_dir/before.services"
yq -c '
  select(.kind == "Service") |
  {
    name: .metadata.name,
    type: .spec.type,
    ports: .spec.ports,
    selector: .spec.selector
  }
' "$after_manifest" | sort >"$output_dir/after.services"
compare_files "Service Specs" "$output_dir/before.services" "$output_dir/after.services"

yq -c '
  select(.kind == "Secret") |
  {
    name: .metadata.name,
    type: .type,
    data: .data,
    stringData: .stringData
  }
' "$before_manifest" | sort >"$output_dir/before.secrets"
yq -c '
  select(.kind == "Secret") |
  {
    name: .metadata.name,
    type: .type,
    data: .data,
    stringData: .stringData
  }
' "$after_manifest" | sort >"$output_dir/after.secrets"
compare_files "Secret Payloads" "$output_dir/before.secrets" "$output_dir/after.secrets"

for service in "${services[@]}"; do
  config_key="${service}.yaml"
  write_or_missing \
    "select(.kind == \"ConfigMap\" and .metadata.name == \"$service\") | .data[\"$config_key\"]" \
    "$before_manifest" \
    "$output_dir/before.${service}.config"
  write_or_missing \
    "select(.kind == \"ConfigMap\" and .metadata.name == \"$service\") | .data[\"$config_key\"]" \
    "$after_manifest" \
    "$output_dir/after.${service}.config"
  compare_files "ConfigMap ${service}" "$output_dir/before.${service}.config" "$output_dir/after.${service}.config"
done

if [[ $overall_status -eq 0 ]]; then
  echo
  echo "No differences found in the checked sections."
else
  echo
  echo "Differences found. Review the sections above."
fi

exit "$overall_status"
