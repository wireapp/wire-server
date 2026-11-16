#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

# Synthetic SSLyze output: a successful scan with an ECDSA leaf, intermediate and root.
report='{"server_scan_results":[{
  "connectivity_status":"COMPLETED", "scan_status":"COMPLETED",
  "scan_result":{
    "tls_1_2_cipher_suites":{"result":{"tls_version_used":"TLS_1_2","accepted_cipher_suites":[{}]}},
    "tls_1_3_cipher_suites":{"result":{"tls_version_used":"TLS_1_3","accepted_cipher_suites":[{}]}},
    "certificate_info":{"result":{"certificate_deployments":[{"verified_certificate_chain":[
      {"signature_algorithm_oid":{"name":"ecdsa-with-SHA256"}},
      {"signature_algorithm_oid":{"name":"ecdsa-with-SHA384"}},
      {"signature_algorithm_oid":{"name":"ecdsa-with-SHA384"}}
    ]}]}}
  }
}]}'
check() {
    jq -e --slurpfile policy tr-02102-2/tls.json -f check-scan.jq >/dev/null
}
check <<< "$report"

for position in 0 1 2; do
    if jq --argjson position "$position" '
        .server_scan_results[0].scan_result.certificate_info.result.certificate_deployments[0]
        .verified_certificate_chain[$position].signature_algorithm_oid.name = "sha256WithRSAEncryption"
    ' <<< "$report" | check; then
        echo "Accepted an RSA signature at chain position $position" >&2
        exit 1
    fi
done
for mutation in \
    '.server_scan_results[0].scan_result.certificate_info.result.certificate_deployments[0].verified_certificate_chain = null' \
    '.server_scan_results[0].scan_result.certificate_info.result.certificate_deployments = []' \
    '.server_scan_results[0].scan_result.tls_1_3_cipher_suites.result.accepted_cipher_suites = []' \
    '.server_scan_results = []'; do
    if jq "$mutation" <<< "$report" | check; then
        echo "Accepted incomplete scan: $mutation" >&2
        exit 1
    fi
done
echo 'Scan checks passed: ECDSA chain accepted; RSA signatures and incomplete results rejected.'
