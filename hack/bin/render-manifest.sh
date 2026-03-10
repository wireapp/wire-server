#!/usr/bin/env bash
# best run via make render-manifest
# otherwise run make clean-charts and make charts-integration before

set -euo pipefail

if [[ $# -ne 1 ]]; then
  cat >&2 <<'EOF'
Usage: render-manifest.sh <values-file>

Optional environment variables:
  OUTPUT_FILE=/tmp/rendered.yaml
EOF
  exit 1
fi

VALUES_FILE="$1"
OUTPUT_FILE="${OUTPUT_FILE:-/tmp/wire-server.yaml}"

if [[ ! -f "$VALUES_FILE" ]]; then
  echo "Values file not found: $VALUES_FILE" >&2
  exit 1
fi

helm_opts=(
  --namespace wire
  --no-hooks
  -f "$VALUES_FILE"
)

helm dependency build --skip-refresh ./.local/charts/wire-server
helm template wire-server ./.local/charts/wire-server \
  "${helm_opts[@]}" \
  > "$OUTPUT_FILE"

echo "Rendered manifest: $OUTPUT_FILE"
