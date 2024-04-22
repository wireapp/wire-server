#!/usr/bin/env bash

# Create a self-signed x509 certificate in the current working directory.
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for integration tests that explicitly disable certificate checking

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)

TEMP=$(mktemp -d wire-server-self-signed-XXXXXX --tmpdir)
CSR_FEDERATION="$TEMP/csr-federation.json"
CSR_FEDERATION_CA="$TEMP/csr-federation-ca.json"
CSR_ELASTICSEARCH="$TEMP/csr-elasitcsearch.json"
CSR_ELASTICSEARCH_CA="$TEMP/csr-elasticsearch-ca.json"
FEDERATION_CA="$TEMP/integration-ca"
FEDERATION_LEAF_CERT="$TEMP/integration-leaf"
ELASTICSEARCH_CA="$TEMP/elasticsearch-ca"
ELASTICSEARCH_LEAF_CERT="$TEMP/elasticsearch-leaf"

command -v cfssl >/dev/null 2>&1 || { echo >&2 "cfssl is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }
command -v cfssljson >/dev/null 2>&1 || { echo >&2 "cfssljson is not installed, aborting. See https://github.com/cloudflare/cfssl"; exit 1; }

echo '{
    "CN": "ca.example.com",
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > "$CSR_FEDERATION_CA"

# generate CA key and cert
cfssl gencert -initca "$CSR_FEDERATION_CA" | cfssljson -bare "$FEDERATION_CA"

echo '{
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > "$CSR_FEDERATION"

# generate cert and key based on CA given comma-separated hostnames as SANs
cfssl gencert \
  -ca "$FEDERATION_CA.pem" \
  -ca-key "$FEDERATION_CA-key.pem" \
  -hostname=*.integration.example.com,host.docker.internal,localhost \
  "$CSR_FEDERATION" \
  | cfssljson -bare "$FEDERATION_LEAF_CERT"

cp "$FEDERATION_CA.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"
cp "$FEDERATION_CA-key.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"
cp "$FEDERATION_LEAF_CERT.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"
cp "$FEDERATION_LEAF_CERT-key.pem" "$ROOT_DIR/services/nginz/integration-test/conf/nginz/"

cp "$FEDERATION_CA.pem" "$ROOT_DIR/deploy/dockerephemeral/federation-v0/"
cp "$FEDERATION_LEAF_CERT.pem" "$ROOT_DIR/deploy/dockerephemeral/federation-v0/"
cp "$FEDERATION_LEAF_CERT-key.pem" "$ROOT_DIR/deploy/dockerephemeral/federation-v0/"

echo '{
    "CN": "elasticsearch.ca.example.com",
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > "$CSR_ELASTICSEARCH_CA"

# generate CA key and cert
cfssl gencert -initca "$CSR_ELASTICSEARCH_CA" | cfssljson -bare "$ELASTICSEARCH_CA"

echo '{
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' > "$CSR_ELASTICSEARCH"

# generate cert and key based on CA given comma-separated hostnames as SANs
cfssl gencert \
  -ca "$ELASTICSEARCH_CA.pem" \
  -ca-key "$ELASTICSEARCH_CA-key.pem" \
  -hostname=localhost \
  "$CSR_ELASTICSEARCH" \
  | cfssljson -bare "$ELASTICSEARCH_LEAF_CERT"

cp "$ELASTICSEARCH_CA.pem" "$ROOT_DIR/deploy/dockerephemeral/docker/elasticsearch-ca.pem"
cp "$ELASTICSEARCH_LEAF_CERT.pem" "$ROOT_DIR/deploy/dockerephemeral/docker/elasticsearch-cert.pem"
cp "$ELASTICSEARCH_LEAF_CERT-key.pem" "$ROOT_DIR/deploy/dockerephemeral/docker/elasticsearch-key.pem"

cp "$ELASTICSEARCH_CA.pem" "$ROOT_DIR/hack/helm_vars/elasticsearch-certs/elasticsearch-ca.pem"
cp "$ELASTICSEARCH_CA-key.pem" "$ROOT_DIR/hack/helm_vars/elasticsearch-certs/elasticsearch-ca-key.pem"

rm -rf "$TEMP"
