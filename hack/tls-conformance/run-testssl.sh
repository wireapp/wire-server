#!/usr/bin/env bash
# Usage: bash run-testssl.sh /path/to/testssl.sh [hostname] [output-directory] [IP]
# Fetch the reviewed scanner with:
# git clone --depth 1 --branch v3.2.2 https://github.com/testssl/testssl.sh.git /tmp/testssl
set -euo pipefail

scanner=${1:?Pass the path to the testssl.sh executable}
hostname=${2:-tr.hops.wire.link}
output_dir=${3:-$(mktemp -d /tmp/wire-tls-scan.XXXXXX)}
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
scan_options=()
if [[ -n ${4:-} ]]; then
  python3 -c 'import ipaddress, sys; ipaddress.ip_address(sys.argv[1])' "$4"
  scan_options+=(--ip "$4")
fi
[[ $hostname =~ ^[a-zA-Z0-9][a-zA-Z0-9.-]*$ ]] || { echo "Invalid hostname" >&2; exit 2; }
[[ -f $scanner ]] || { echo "Scanner not found" >&2; exit 2; }
mkdir -p -- "$output_dir"

# The default socket probes cover suites beyond the local OpenSSL build.
# Do not use --ssl-native, which would reduce that coverage.
bash "$scanner" --warnings batch --color 0 --connect-timeout 10 \
  --openssl-timeout 15 --protocols --cipher-per-proto --server-preference \
  --server-defaults --fs --jsonfile "$output_dir/testssl.json" \
  "${scan_options[@]}" "$hostname:443" | tee "$output_dir/testssl.txt"
python3 "$script_dir/check-testssl.py" "$output_dir/testssl.json"
echo "Evidence saved in $output_dir"
