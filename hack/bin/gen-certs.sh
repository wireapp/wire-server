#!/usr/bin/env bash
set -eo pipefail

# Create certificates needed for running integration tests.

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
ROOT_DIR=$(cd -- "$SCRIPT_DIR/../../" &> /dev/null && pwd)

TEMP=$(mktemp -d wire-server-self-signed-XXXXXX --tmpdir)

cleanup() {
  rm -fr "$TEMP"
}
trap cleanup EXIT

# usage: gen_ca root name
#
# Generate self-signed CA certificate and key at root/ca.pem and
# root/ca-key.pem respectively.
gen_ca() {
  echo "generating CA: $2"
  openssl req -x509 -newkey rsa:2048 -keyout "$1/ca-key.pem" -out "$1/ca.pem" -sha256 -days 3650 -nodes -subj "/CN=$2" 2>/dev/null

}

# usage: gen_cert root san name
#
# Generate leaf certificate in the given root directory. Assumes that ca.pem
# and ca-key.pem exist in the same directory. The generated certificate and
# private key will end up in root/cert.pem and root/key.pem.
gen_cert() {
  echo "generating certificate: $2"
  subj=()
  if [ -n "$3" ]; then
    subj=(-subj "/CN=$3")
  fi
  openssl x509 -req -in <(openssl req -nodes -newkey rsa:2048 -keyout "$1/key.pem" -out /dev/stdout -subj "/" 2>/dev/null) -CA "$1/ca.pem" -CAkey "$1/ca-key.pem" "${subj[@]}" -out "$1/cert.pem" -set_serial 0 -days 3650 -extfile <( echo "extendedKeyUsage = serverAuth, clientAuth"; echo "subjectAltName = critical, $2" ) 2>/dev/null
}

# usage: install_certs source_dir target_dir ca ca-key cert key
#
# Copy certificates into the target directory, using the given file names. If a
# name is empty, the corresponding certificate is skipped.
install_certs() {
  if [ -n "$3" ]; then cp "$1/ca.pem" "$2/$3.pem"; fi
  if [ -n "$4" ]; then cp "$1/ca-key.pem" "$2/$4.pem"; fi
  if [ -n "$5" ]; then cp "$1/cert.pem" "$2/$5.pem"; fi
  if [ -n "$6" ]; then cp "$1/key.pem" "$2/$6.pem"; fi
}

# federation
mkdir -p "$TEMP/federation"
gen_ca "$TEMP/federation" ca.example.com
gen_cert "$TEMP/federation" "DNS:*.integration.example.com, DNS:host.docker.internal, DNS:localhost"
install_certs "$TEMP/federation" "$ROOT_DIR/services/nginz/integration-test/conf/nginz" \
    integration-ca integration-ca-key integration-leaf integration-leaf-key
install_certs "$TEMP/federation" "$ROOT_DIR/deploy/dockerephemeral/federation-v0" \
    integration-ca "" integration-leaf integration-leaf-key
install_certs "$TEMP/federation" "$ROOT_DIR/deploy/dockerephemeral/federation-v1" \
    integration-ca "" integration-leaf integration-leaf-key

# elasticsearch
mkdir -p "$TEMP/es"
gen_ca "$TEMP/es" elasticsearch.ca.example.com
gen_cert "$TEMP/es" "DNS:localhost" localhost
install_certs "$TEMP/es" "$ROOT_DIR/deploy/dockerephemeral/docker" \
    elasticsearch-ca "" elasticsearch-cert elasticsearch-key
install_certs "$TEMP/es" "$ROOT_DIR/hack/helm_vars/certs" \
    elasticsearch-ca elasticsearch-ca-key

# redis
mkdir -p "$TEMP/redis"
gen_ca "$TEMP/redis" redis.ca.example.com
REDIS="$ROOT_DIR/deploy/dockerephemeral/docker"
cp "$TEMP/redis/ca.pem" "$REDIS/redis-ca.pem"
for redis_node in $(seq 1 6); do
  gen_cert "$TEMP/redis" "DNS:redis-${redis_node}, IP:172.20.0.3${redis_node}"
  chmod 0644 "$TEMP/redis/key.pem"
  install_certs "$TEMP/redis" "$REDIS" "" "" \
      "redis-node-${redis_node}-cert" \
      "redis-node-${redis_node}-key"
done

# rabbitmq
RABBITMQ="$ROOT_DIR/deploy/dockerephemeral/rabbitmq-config/certificates"
gen_ca "$RABBITMQ" rabbitmq.ca.example.com
gen_cert "$RABBITMQ" "DNS:localhost, DNS:rabbitmq, IP:127.0.0.1" localhost
chmod a+r "$RABBITMQ/key.pem"
