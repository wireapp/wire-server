#!/usr/bin/env bash
# Repeats the toxiproxy-outage.sh sequence (enabled:false toggle on
# amqp_proxy, timeout toxic on admin_proxy) until the
# backend-notification-pusher bug from analysis.md reproduces, then stops
# and leaves state as-is for inspection. This was the most reliable
# mechanism this session for hitting the permanent-death race (confirmed
# via the diagnostic log lines in BackendNotificationPusher.hs's cleanup
# and Extended.hs's openChan/closed-handler) -- a single run isn't
# guaranteed to hit it since the fatal path needs a secondary,
# probabilistic stale-fd/MVar-deadlock race to land on the specific
# reconnect lineage that matters.
#
# Prereqs:
#   docker compose up -d rabbitmq toxiproxy
#   background-worker running with services/background-worker/background-worker.integration.yaml
#   (rabbitmq.port/adminPort already point at toxiproxy's 15673/15674)
set -uo pipefail # NOT -e: a single failed curl/check shouldn't kill the loop

TOXIPROXY_URL="${TOXIPROXY_URL:-http://localhost:8474}"
METRICS_URL="${METRICS_URL:-http://localhost:8089/i/metrics}"
AMQP_PROXY=amqp_proxy
ADMIN_PROXY=admin_proxy

CUT_AMQP_AFTER_S="${CUT_AMQP_AFTER_S:-20}"
# Long enough for getRemoteDomains's 60s-cumulative-backoff cap to be
# exhausted after AMQP recovers (with Env.hs's shortened 2s responseTimeout,
# roughly 13 attempts * 2s + ~80s backoff sum ~= 105-110s; keep margin).
WAIT_FOR_ADMIN_TIMEOUT_S="${WAIT_FOR_ADMIN_TIMEOUT_S:-150}"
SETTLE_S="${SETTLE_S:-10}"
# Upper bound on waiting for the pusher to confirm recovered (gauge back to
# 1) before starting the next outage -- without this, iterations could
# stack a fresh cut on top of a still-recovering one, which isn't the
# "healthy service hits a fresh outage" scenario we're trying to reproduce.
RECOVERY_TIMEOUT_S="${RECOVERY_TIMEOUT_S:-60}"
RECOVERY_POLL_INTERVAL_S="${RECOVERY_POLL_INTERVAL_S:-1}"
MAX_ITERATIONS="${MAX_ITERATIONS:-0}" # 0 = unlimited

ts() { date -u +"%Y-%m-%d %H:%M:%S.%3N"; }

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

reset_state() {
  set_proxy_enabled "$AMQP_PROXY" true
  remove_toxic "$ADMIN_PROXY" cut_admin 2>/dev/null || true
}

pusher_gauge() {
  curl -sf "$METRICS_URL" 2>/dev/null \
    | awk -F' ' '/^wire_background_worker_running_workers{worker="backend-notification-pusher"}/ {print $2}'
}

wait_for_recovery() {
  local deadline=$(($(date +%s) + RECOVERY_TIMEOUT_S))
  while [[ "$(date +%s)" -lt "$deadline" ]]; do
    [[ "$(pusher_gauge)" == "1.0" ]] && return 0
    sleep "$RECOVERY_POLL_INTERVAL_S"
  done
  return 1
}

report_and_wait() {
  local reason=$1
  echo "[$(ts)] $reason"
  echo "  curl -s -u guest:alpaca-grapefruit http://localhost:15674/api/consumers/%2F | jq"
  sleep infinity
}

run_one_outage() {
  echo "[$(ts)] cutting both AMQP and admin-API paths"
  set_proxy_enabled "$AMQP_PROXY" false
  add_toxic "$ADMIN_PROXY" cut_admin

  sleep "$CUT_AMQP_AFTER_S"

  echo "[$(ts)] restoring AMQP path only (admin API stays down)"
  set_proxy_enabled "$AMQP_PROXY" true

  echo "[$(ts)] waiting ${WAIT_FOR_ADMIN_TIMEOUT_S}s for getRemoteDomains's retry cap to exhaust"
  sleep "$WAIT_FOR_ADMIN_TIMEOUT_S"

  echo "[$(ts)] restoring admin-API path"
  remove_toxic "$ADMIN_PROXY" cut_admin

  sleep "$SETTLE_S"
}

main() {
  local i=0
  while true; do
    i=$((i + 1))
    if [[ "$MAX_ITERATIONS" -gt 0 && "$i" -gt "$MAX_ITERATIONS" ]]; then
      echo "[$(ts)] hit MAX_ITERATIONS=$MAX_ITERATIONS without reproducing, stopping"
      exit 1
    fi
    echo "[$(ts)] === iteration $i ==="
    reset_state

    echo "[$(ts)] waiting for pusher to confirm healthy before cutting again"
    if ! wait_for_recovery; then
      report_and_wait "pusher never confirmed healthy within ${RECOVERY_TIMEOUT_S}s -- treating this as reproduced already"
    fi

    run_one_outage

    gauge=$(pusher_gauge)
    echo "[$(ts)] iteration $i: backend-notification-pusher gauge = ${gauge:-<no response>}"
    if [[ "$gauge" == "0" ]]; then
      report_and_wait "REPRODUCED after $i iteration(s). Leaving state as-is for inspection."
    fi
    echo "[$(ts)] iteration $i: not reproduced (pusher recovered), looping"
  done
}

main "$@"
