#!/usr/bin/env bash

# Create a self-signed x509 certificate in the hack/helm_vars directories (as helm yaml config).
# Requires 'cfssl' to be on your PATH (see https://github.com/cloudflare/cfssl)
# These certificates are only meant for integration tests.
# (The CA certificates are assumed to be re-used across the domains A and B for end2end integration tests.)

set -ex
SUFFIX=${1:?"need suffix argument"}
TEMP=${TEMP:-/tmp}
CSR="$TEMP/csr.json"
OUTPUTNAME_CA="integration-ca"
OUTPUTNAME_LEAF_CERT="integration-leaf"
OUTPUTNAME_CLIENT_CERT="integration-client"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOP_LEVEL="$DIR/../.."
OUTPUT_CONFIG_FEDERATOR="$TOP_LEVEL/hack/helm_vars/wire-server/certificates-$SUFFIX.yaml"
OUTPUT_CONFIG_INGRESS="$TOP_LEVEL/hack/helm_vars/nginx-ingress-services/certificates-$SUFFIX.yaml"

command -v cfssl >/dev/null 2>&1 || {
    echo >&2 "cfssl is not installed, aborting. See https://github.com/cloudflare/cfssl"
    exit 1
}
command -v cfssljson >/dev/null 2>&1 || {
    echo >&2 "cfssljson is not installed, aborting. See https://github.com/cloudflare/cfssl"
    exit 1
}

FEDERATION_DOMAIN_BASE=${FEDERATION_DOMAIN_BASE:?"you must provide a FEDERATION_DOMAIN_BASE env variable"}

# generate CA key and cert
if [ ! -f "$OUTPUTNAME_CA.pem" ]; then
    echo "CA file not found, generating CA..."
    echo '{
    "CN": "ca.example.com",
        "key": {
            "algo": "rsa",
            "size": 2048
        }
    }' >"$CSR"
    cfssl gencert -initca "$CSR" | cfssljson -bare "$OUTPUTNAME_CA"
    rm "$OUTPUTNAME_CA.csr"
else
    echo "Re-using previous CA"
fi

# For federation end2end tests, only the
# 'federation-test-helper.$FEDERATION_DOMAIN_BASE' is necessary for
# ingress->federator traffic. For other potential traffic in the integration
# tests of the future, we use a wildcard certificate here.
echo '{
    "key": {
        "algo": "rsa",
        "size": 2048
    }
}' >"$CSR"
# generate cert and key based on CA given comma-separated hostnames as SANs
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname="*.$FEDERATION_DOMAIN_BASE" "$CSR" | cfssljson -bare "$OUTPUTNAME_LEAF_CERT"

# generate client certificate and key
cfssl gencert -ca "$OUTPUTNAME_CA.pem" -ca-key "$OUTPUTNAME_CA-key.pem" -hostname="*.$FEDERATION_DOMAIN_BASE" "$CSR" | cfssljson -bare "$OUTPUTNAME_CLIENT_CERT"

# the following yaml override file is needed as an override to
# nginx-ingress-services helm chart
# for domain A, ingress@A needs cert+key for A
{
    echo "secrets:"
    echo "  tlsWildcardCert: |"
    sed -e 's/^/    /' $OUTPUTNAME_LEAF_CERT.pem
    echo "  tlsWildcardKey: |"
    sed -e 's/^/    /' $OUTPUTNAME_LEAF_CERT-key.pem
    echo "  tlsClientCA: |"
    sed -e 's/^/    /' $OUTPUTNAME_CA.pem
} | tee "$OUTPUT_CONFIG_INGRESS"

# the following yaml override file is needed as an override to
# the wire-server (federator) helm chart
# e.g. for installing on domain A, federator@A needs the CA for B
# As a "shortcut" for integration tests, we re-use the same CA for both domains
# A and B.
{
    echo "federator:"
    echo "  remoteCAContents: |"
    sed -e 's/^/    /' $OUTPUTNAME_CA.pem
    echo "  clientCertificateContents: |"
    sed -e 's/^/    /' $OUTPUTNAME_CLIENT_CERT.pem
    echo "  clientPrivateKeyContents: |"
    sed -e 's/^/    /' $OUTPUTNAME_CLIENT_CERT-key.pem
} | tee "$OUTPUT_CONFIG_FEDERATOR"

# cleanup unneeded files
rm "$OUTPUTNAME_LEAF_CERT.csr"
rm "$OUTPUTNAME_LEAF_CERT.pem"
rm "$OUTPUTNAME_LEAF_CERT-key.pem"
rm "$OUTPUTNAME_CLIENT_CERT.csr"
rm "$OUTPUTNAME_CLIENT_CERT.pem"
rm "$OUTPUTNAME_CLIENT_CERT-key.pem"
rm "$CSR"
