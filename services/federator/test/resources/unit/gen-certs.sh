#!/usr/bin/env bash

set -euo pipefail

# Create a self-signed x509 certificate in script directory.
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for federator unit tests

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

set -e
TEMP=${TEMP:-"$(mktemp -d)"}
CSR="$TEMP/csr.json"
OUTPUTNAME_CA="$DIR/unit-ca"
OUTPUTNAME_LOCALHOST_CERT="$DIR/localhost"
OUTPUTNAME_LOCALHOST_DOT_CERT="$DIR/localhost-dot"
OUTPUTNAME_EXAMPLE_COM_CERT="$DIR/localhost.example.com"

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
cfssl gencert -initca "$CSR" | cfssljson -bare "$OUTPUTNAME_CA"

echo '{
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' >"$CSR"

# generate cert and key based on CA given comma-separated hostnames as SANs
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname="localhost" "$CSR" | cfssljson -bare "$OUTPUTNAME_LOCALHOST_CERT"
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname="localhost." "$CSR" | cfssljson -bare "$OUTPUTNAME_LOCALHOST_DOT_CERT"
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname="localhost.example.com" "$CSR" | cfssljson -bare "$OUTPUTNAME_EXAMPLE_COM_CERT"

# cleanup unneeded files
rm "$OUTPUTNAME_CA.csr"
rm "$OUTPUTNAME_LOCALHOST_CERT.csr"
rm "$OUTPUTNAME_LOCALHOST_DOT_CERT.csr"
rm "$OUTPUTNAME_EXAMPLE_COM_CERT.csr"
