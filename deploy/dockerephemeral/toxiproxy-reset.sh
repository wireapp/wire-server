#!/usr/bin/env bash
# Resets toxiproxy (see docker-compose.yaml) to a clean passthrough state:
# both proxies enabled, no toxics. Use after toxiproxy-outage.sh was
# interrupted, errored, or to force a clean slate before a new repro run.
set -euo pipefail

TOXIPROXY_URL="${TOXIPROXY_URL:-http://localhost:8474}"
AMQP_PROXY=amqp_proxy
ADMIN_PROXY=admin_proxy

set_proxy_enabled() {
  local proxy=$1 enabled=$2
  curl -sf -X POST "$TOXIPROXY_URL/proxies/$proxy" \
    -d '{"enabled":'"$enabled"'}' >/dev/null
}

remove_all_toxics() {
  local proxy=$1
  curl -sf "$TOXIPROXY_URL/proxies/$proxy/toxics" \
    | jq -r '.[].name' \
    | while read -r name; do
        curl -sf -X DELETE "$TOXIPROXY_URL/proxies/$proxy/toxics/$name" >/dev/null
      done
}

main() {
  set_proxy_enabled "$AMQP_PROXY" true
  set_proxy_enabled "$ADMIN_PROXY" true
  remove_all_toxics "$AMQP_PROXY"
  remove_all_toxics "$ADMIN_PROXY"
  echo "toxiproxy reset: both proxies enabled, no toxics."
}

main "$@"
