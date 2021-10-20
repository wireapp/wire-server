#!/usr/bin/env bash

set -euo pipefail

# Create a self-signed x509 certificate in script directory.
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for federator unit tests

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

set -e
TEMP=${TEMP:-"$(mktemp -d)"}
REGENERATE=${REGENERATE:-0}
CSR="$TEMP/csr.json"
OUTPUTNAME_CA="$DIR/unit-ca"
OUTPUTNAME_LOCALHOST_CERT="$DIR/localhost"
OUTPUTNAME_LOCALHOST_DOT_CERT="$DIR/localhost-dot"
OUTPUTNAME_EXAMPLE_COM_CERT="$DIR/localhost.example.com"
OUTPUTNAME_SECOND_EXAMPLE_COM_CERT="$DIR/second-federator.example.com"

command -v cfssl >/dev/null 2>&1 || { echo >&2 "cfssl is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }
command -v cfssljson >/dev/null 2>&1 || { echo >&2 "cfssljson is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }

echo '{
    "CN": "ca.example.com",
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' >"$CSR"

# generate CA key and cert
if [[ ! -f "$OUTPUTNAME_CA.pem" ]] || [[ "$REGENERATE" -eq "1" ]]; then
  cfssl gencert -initca "$CSR" | cfssljson -bare "$OUTPUTNAME_CA"
fi

echo '{
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' >"$CSR"

generate() {
    local hostname=$1
    local file=$2

    if [[ ! -f "$file.pem" ]] || [[ "$REGENERATE" -eq "1" ]]; then
        cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname="$hostname" "$CSR" | cfssljson -bare "$file"
    fi
}

generate cert and key based on CA given comma-separated hostnames as SANs
generate "localhost" "$OUTPUTNAME_LOCALHOST_CERT"
generate "localhost." "$OUTPUTNAME_LOCALHOST_DOT_CERT"
generate "localhost.example.com" "$OUTPUTNAME_EXAMPLE_COM_CERT"
generate "second-federator.example.com" "$OUTPUTNAME_SECOND_EXAMPLE_COM_CERT"

# cleanup unneeded files
rm -f "$OUTPUTNAME_CA.csr"
rm -f "$OUTPUTNAME_LOCALHOST_CERT.csr"
rm -f "$OUTPUTNAME_LOCALHOST_DOT_CERT.csr"
rm -f "$OUTPUTNAME_EXAMPLE_COM_CERT.csr"
rm -f "$OUTPUTNAME_SECOND_EXAMPLE_COM_CERT.csr"
