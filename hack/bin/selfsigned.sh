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
}' >"$CSR"

# generate CA key and cert
cfssl gencert -initca "$CSR" | cfssljson -bare "$OUTPUTNAME_CA"
# TODO: only generate CA once!

echo '{
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' >"$CSR"

# keep this in sync with hack/bin/integration-setup.sh
# TODO take variable as argument?
#
# For federation end2end tests, only the
# 'federation-test-helper.$NAMESPACE.svc.cluster.local' is necessary for
# ingress->federator traffic. For other potential traffic in the integration
# tests of the future, we use a wildcard certificate here.
FEDERATION_DOMAIN="*.$NAMESPACE.svc.cluster.local"

# generate cert and key based on CA given comma-separated hostnames as SANs
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname="$FEDERATION_DOMAIN" "$CSR" | cfssljson -bare "$OUTPUTNAME_LEAF_CERT"
# cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname=""*.integration.example.com,localhost "$CSR" | cfssljson -bare "$OUTPUTNAME_LEAF_CERT"

# the following yaml override file is needed as an override to
# nginx-ingress-services helm chart
# for domain A, ingress@A needs cert+key for A
{
    echo "secrets:"
    echo "  tlsWildcardCert: |"
    sed -e 's/^/    /' $OUTPUTNAME_LEAF_CERT.pem
    echo "  tlsWildcardKey: |"
    sed -e 's/^/    /' $OUTPUTNAME_LEAF_CERT-key.pem
} >$OUTPUTNAME_LEAF_CERT.yaml

# the following yaml override file is needed as an override to
# the wire-server (federator) helm chart
# for domain A, federator@A needs the CA for B
{
    echo "federator:"
    echo "  remoteCAContents: |"
    sed -e 's/^/    /' $OUTPUTNAME_CA.pem
} >$OUTPUTNAME_CA.yaml

# cleanup unneeded files
rm "$OUTPUTNAME_LEAF_CERT.csr"
rm "$OUTPUTNAME_CA.csr"
rm "$OUTPUTNAME_LEAF_CERT.pem"
rm "$OUTPUTNAME_LEAF_CERT-key.pem"
