#!/usr/bin/env bash

set -euo pipefail

# Create a self-signed x509 certificate in script directory.
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for federator unit tests

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

set -ex
TEMP=${TEMP:-"$(mktemp -d)"}
REGENERATE=${REGENERATE:-0}
CSR="$TEMP/csr.json"
OUTPUTNAME_CA="$DIR/unit-ca"
OUTPUTNAME_MULTIDOMAIN_CERT="$DIR/multidomain-federator.example.com"

command -v cfssl >/dev/null 2>&1 || {
    echo >&2 "cfssl is not installed, aborting. See https://github.com/cloudflare/cfssl"
    exit 1
}
command -v cfssljson >/dev/null 2>&1 || {
    echo >&2 "cfssljson is not installed, aborting. See https://github.com/cloudflare/cfssl"
    exit 1
}

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
    },
    "hosts": [
      "something.example.com",
      "somethingelse.example.com",
      "federator.example.com",
      "more.example.com"
    ],
    "CN": "webapp.example.com"
}' >"$CSR"

generate_multi() {
    local file=$1

    if [[ ! -f "$file.pem" ]] || [[ "$REGENERATE" -eq "1" ]]; then
        cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" "$CSR" | cfssljson -bare "$file"
    fi
}

# generate cert and key based on CA given comma-separated hostnames as SANs
generate_multi "$OUTPUTNAME_MULTIDOMAIN_CERT"

# cleanup unneeded files
rm -f "$OUTPUTNAME_CA.csr"
rm -f "$OUTPUTNAME_MULTIDOMAIN_CERT.csr"
