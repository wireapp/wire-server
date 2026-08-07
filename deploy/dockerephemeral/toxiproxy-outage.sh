#!/usr/bin/env bash
# Drives a RabbitMQ outage through toxiproxy (see docker-compose.yaml) to
# reproduce the backend-notification-pusher bug described in analysis.md.
#
# Prereqs:
#   docker compose up -d rabbitmq toxiproxy
#   (toxiproxy starts with amqp_proxy/admin_proxy already defined and passing
#   traffic through untouched, per toxiproxy-config.json)
#   background-worker running with services/background-worker/background-worker.integration.yaml
#   (rabbitmq.port/adminPort already point at toxiproxy's 15673/15674)
#
# Sequence: cut both AMQP and admin-API paths together (matches the shared
# network outage seen in the incident), then restore AMQP only while keeping
# the admin API down past its 60s retry cap (getRemoteDomains in
# BackendNotificationPusher.hs), then restore the admin API too.
set -euo pipefail

TOXIPROXY_URL="${TOXIPROXY_URL:-http://localhost:8474}"
AMQP_PROXY=amqp_proxy
ADMIN_PROXY=admin_proxy
CUT_AMQP_AFTER_S="${CUT_AMQP_AFTER_S:-40}"
CUT_ADMIN_AFTER_S="${CUT_ADMIN_AFTER_S:-70}"

set_proxy_enabled() {
  local proxy=$1 enabled=$2
  curl -sf -X POST "$TOXIPROXY_URL/proxies/$proxy" \
    -d '{"enabled":'"$enabled"'}' >/dev/null
}

add_toxic() {
  local proxy=$1 name=$2
  curl -sf -X POST "$TOXIPROXY_URL/proxies/$proxy/toxics" \
    -d '{"name":"'"$name"'","type":"timeout","stream":"downstream","attributes":{"timeout":0}}' >/dev/null
}

remove_toxic() {
  local proxy=$1 name=$2
  curl -sf -X DELETE "$TOXIPROXY_URL/proxies/$proxy/toxics/$name" >/dev/null
}

ts() { date -u +"%Y-%m-%d %H:%M:%S.%3N"; }

main() {
  echo "[$(ts)] == cutting both AMQP and admin-API paths =="
  # AMQP: disable the proxy outright (clean refused connection, one
  # reconnect cycle) instead of a timeout toxic, which hangs already-open
  # streams and causes reconnect storms that mask the bug.
  set_proxy_enabled "$AMQP_PROXY" false
  add_toxic "$ADMIN_PROXY" cut_admin

  sleep "$CUT_AMQP_AFTER_S"

  echo "[$(ts)] == restoring AMQP path only (admin API stays down) =="
  set_proxy_enabled "$AMQP_PROXY" true

  sleep "$CUT_ADMIN_AFTER_S"

  echo "[$(ts)] == restoring admin-API path (outage fully over) =="
  remove_toxic "$ADMIN_PROXY" cut_admin

  echo "Outage sequence complete. Check:"
  echo "  - wire_background_worker_running_workers{worker=\"backend-notification-pusher\"} (expect stuck at 0)"
  echo "  - wire_background_worker_running_workers{worker=\"background-job-consumer\"} (expect recovered to 1)"
  echo "  - background-worker logs: 'backend-notification-pusher' tagged logger going silent"
}

main "$@"
