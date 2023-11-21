#!/usr/bin/env bash

# Create a self-signed x509 certificate in the current working directory.
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for integration tests that explicitly disable certificate checking

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)

TEMP=$(mktemp -d wire-server-self-signed-XXXXXX)
CSR="$TEMP/csr.json"
OUTPUTNAME_CA="$TEMP/integration-ca"
OUTPUTNAME_LEAF_CERT="$TEMP/integration-leaf"

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
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname=*.integration.example.com,host.docker.internal,localhost "$CSR" | cfssljson -bare "$OUTPUTNAME_LEAF_CERT"

cp "$OUTPUTNAME_CA.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"
cp "$OUTPUTNAME_CA-key.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"
cp "$OUTPUTNAME_LEAF_CERT.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"
cp "$OUTPUTNAME_LEAF_CERT-key.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"

cp "$OUTPUTNAME_CA.pem" "$ROOT_DIR/deploy/dockerephemeral/federation-v0/"
cp "$OUTPUTNAME_LEAF_CERT.pem" "$ROOT_DIR/deploy/dockerephemeral/federation-v0/"
cp "$OUTPUTNAME_LEAF_CERT-key.pem" "$ROOT_DIR/deploy/dockerephemeral/federation-v0/"

rm -rf "$TEMP"
