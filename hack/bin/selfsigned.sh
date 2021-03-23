#!/usr/bin/env bash

# Create a self-signed x509 certificate in the current working directory.
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for integration tests that explicitly disable certificate checking

set -e
TEMP=${TEMP:-/tmp}
CSR="$TEMP/csr.json"
OUTPUTNAME_CA="integration-ca"
OUTPUTNAME_LEAF_CERT="integration-leaf"

command -v cfssl >/dev/null 2>&1 || { echo >&2 "cfssl is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }
command -v cfssljson >/dev/null 2>&1 || { echo >&2 "cfssljson is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }

echo '{
    "CN": "ca.example.com",
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > "$CSR"

# generate CA key and cert
cfssl gencert -initca "$CSR" | cfssljson -bare "$OUTPUTNAME_CA"

echo '{
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > "$CSR"

# generate cert and key based on CA given comma-separated hostnames as SANs
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname=*.integration.example.com,localhost "$CSR" | cfssljson -bare "$OUTPUTNAME_LEAF_CERT"

# cleanup unneeded files
rm "$OUTPUTNAME_LEAF_CERT.csr"
rm "$OUTPUTNAME_CA.csr"


