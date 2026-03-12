#!/usr/bin/env bash
# best run via make render-wire-server-resources
# otherwise run make clean-charts and make charts-integration before

set -euo pipefail

# from repo root
export NAMESPACE_1="${NAMESPACE_1:-test-a}"
export NAMESPACE_2="${NAMESPACE_2:-test-b}"
export FEDERATION_DOMAIN_1="${FEDERATION_DOMAIN_1:-integration.example.com}"
export FEDERATION_DOMAIN_2="${FEDERATION_DOMAIN_2:-integration2.example.com}"
export FEDERATION_DOMAIN_BASE_1="${FEDERATION_DOMAIN_BASE_1:-example.com}"
export FEDERATION_DOMAIN_BASE_2="${FEDERATION_DOMAIN_BASE_2:-example.com}"
export FEDERATION_CA_CERTIFICATE="${FEDERATION_CA_CERTIFICATE:-$(cat services/nginz/integration-test/conf/nginz/integration-ca.pem)}"
export ENTERPRISE_IMAGE_PULL_SECRET="${ENTERPRISE_IMAGE_PULL_SECRET:-{}}"

helmfile -f hack/helmfile.yaml.gotmpl \
  -e default \
  --skip-deps \
  -l name=wire-server \
  write-values \
  --output-file-template '/tmp/{{ .Release.Name }}-{{ .Release.Namespace }}.yaml'

VALUES_FILE="/tmp/wire-server-${NAMESPACE_1}.yaml"

echo "Rendered values:   $VALUES_FILE"
