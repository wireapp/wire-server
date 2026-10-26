#!/bin/sh

set -eu

redis_cli() {
  host=$1
  port=$2
  shift 2
  redis-cli \
    --tls \
    --cacert /usr/local/etc/redis/ca.pem \
    -h "$host" \
    -p "$port" \
    "$@"
}

while :; do
  nodes_ready=1
  for endpoint in \
    172.20.0.31:6373 \
    172.20.0.32:6374 \
    172.20.0.33:6375 \
    172.20.0.34:6376 \
    172.20.0.35:6377 \
    172.20.0.36:6378; do
    host=${endpoint%:*}
    port=${endpoint#*:}
    if ! redis_cli "$host" "$port" ping >/dev/null 2>&1; then
      nodes_ready=0
    fi
  done

  if [ "$nodes_ready" -eq 0 ]; then
    sleep 1
    continue
  fi

  cluster_info=$(redis_cli 172.20.0.31 6373 cluster info 2>/dev/null | tr -d '\r' || true)

  if printf '%s\n' "$cluster_info" | grep -q '^cluster_state:ok$'; then
    exit 0
  fi

  if printf '%s\n' "$cluster_info" | grep -q '^cluster_state:fail$'; then
    break
  fi

  sleep 1
done

redis-cli \
  --tls \
  --cacert /usr/local/etc/redis/ca.pem \
  --cluster create \
  172.20.0.31:6373 \
  172.20.0.32:6374 \
  172.20.0.33:6375 \
  172.20.0.34:6376 \
  172.20.0.35:6377 \
  172.20.0.36:6378 \
  --cluster-replicas 1 \
  --cluster-yes
