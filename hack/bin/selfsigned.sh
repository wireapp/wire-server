#!/usr/bin/env bash

# Create a self-signed x509 certificate in the current working directory.
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for integration tests that explicitly disable certificate checking

set -e
TEMP=${TEMP:-/tmp}
CSR="$TEMP/csr.json"

command -v cfssl >/dev/null 2>&1 || { echo >&2 "cfssl is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }
command -v cfssljson >/dev/null 2>&1 || { echo >&2 "cfssljson is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }

echo '{
    "CN": "integration.example.com",
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > "$CSR"

cfssl gencert -initca "$CSR" | cfssljson -bare ca

