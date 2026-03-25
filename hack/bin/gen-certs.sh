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
  local root="$1"
  local name="$2"
  echo "generating CA: $name"
  openssl req -x509 -newkey rsa:2048 -keyout "$root/ca-key.pem" -out "$root/ca.pem" -sha256 -days 3650 -nodes -subj "/CN=$name" 2>/dev/null
  return 0
}

# usage: gen_cert root san name
#
# Generate leaf certificate in the given root directory. Assumes that ca.pem
# and ca-key.pem exist in the same directory. The generated certificate and
# private key will end up in root/cert.pem and root/key.pem.
gen_cert() {
  local root="$1"
  local san="$2"
  local name="$3"
  echo "generating certificate: $name"
  subj=()
  if [[ -n "$name" ]]; then
    subj=(-subj "/CN=$name")
  fi
  openssl x509 -req -in <(openssl req -nodes -newkey rsa:2048 -keyout "$root/key.pem" -out /dev/stdout -subj "/" 2>/dev/null) -CA "$root/ca.pem" -CAkey "$root/ca-key.pem" "${subj[@]}" -out "$root/cert.pem" -set_serial 0 -days 3650 -extfile <( echo "extendedKeyUsage = serverAuth, clientAuth"; echo "subjectAltName = critical, $san" ) 2>/dev/null
  return 0
}

# usage: install_certs source_dir target_dir ca ca-key cert key
#
# Copy certificates into the target directory, using the given file names. If a
# name is empty, the corresponding certificate is skipped.
install_certs() {
  local source_dir="$1"
  local target_dir="$2"
  local ca="$3"
  local ca_key="$4"
  local cert="$5"
  local key="$6"
  if [[ -n "$ca" ]]; then cp "$source_dir/ca.pem" "$target_dir/$ca.pem"; fi
  if [[ -n "$ca_key" ]]; then cp "$source_dir/ca-key.pem" "$target_dir/$ca_key.pem"; fi
  if [[ -n "$cert" ]]; then cp "$source_dir/cert.pem" "$target_dir/$cert.pem"; fi
  if [[ -n "$key" ]]; then cp "$source_dir/key.pem" "$target_dir/$key.pem"; fi
  return 0
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
