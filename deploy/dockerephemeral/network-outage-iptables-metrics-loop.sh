#!/usr/bin/env bash
# Metrics-gated variant of network-outage-iptables-storm.sh: instead of
# cycling AMQP down/up on a fixed (jittered) schedule and hoping the timing
# happens to overlap with a live reconnect, poll
# wire_background_worker_running_workers{worker="backend-notification-pusher"}
# and only cut AMQP once the pusher is confirmed back up. Every cut then
# starts from a known-live connection instead of an arbitrary guessed
# window, and the loop stops itself the moment the pusher fails to recover
# within WAIT_UP_TIMEOUT_S - which is the actual reproduction signal, no
# need to keep guessing cycle counts.
#
# Requires sudo and background-worker pointed directly at rabbitmq's
# published host ports (see network-outage-iptables.sh's own header).
#
# Prereqs:
#   docker compose up -d rabbitmq
#   background-worker running with services/background-worker/background-worker.integration.yaml
set -euo pipefail

AMQP_PORT="${AMQP_PORT:-5671}"
ADMIN_PORT="${ADMIN_PORT:-15671}"
METRICS_URL="${METRICS_URL:-http://localhost:8089/i/metrics}"
MAX_CYCLES="${MAX_CYCLES:-50}"
DOWN_S="${DOWN_S:-1}"
POLL_INTERVAL_S="${POLL_INTERVAL_S:-0.2}"
WAIT_UP_TIMEOUT_S="${WAIT_UP_TIMEOUT_S:-30}"
SETTLE_AFTER_S="${SETTLE_AFTER_S:-30}"

ts() { date -u +"%Y-%m-%d %H:%M:%S.%3N"; }

pusher_gauge() {
  curl -sf "$METRICS_URL" 2>/dev/null \
    | awk -F' ' '/^wire_background_worker_running_workers\{worker="backend-notification-pusher"\}/ {print $2}'
}

wait_for_pusher_state() {
  # $1 = "1" (up) or "0" (down); polls until the gauge matches or we time out
  local want=$1 deadline
  deadline=$(( $(date +%s) + WAIT_UP_TIMEOUT_S ))
  while [[ "$(date +%s)" -lt "$deadline" ]]; do
    [[ "$(pusher_gauge)" == "$want" ]] && return 0
    sleep "$POLL_INTERVAL_S"
  done
  return 1
}

drop_port() {
  local port=$1
  sudo iptables -I OUTPUT -o lo -p tcp --dport "$port" -j DROP
  sudo iptables -I INPUT -i lo -p tcp --sport "$port" -j DROP
  # state all: default ss state filter is "connected" only, which misses
  # syn-sent (a pending connect attempt mid-handshake would otherwise just
  # sit in the kernel's own slow SYN-retransmission backoff instead of
  # failing immediately).
  sudo ss -K state all "( dport = :$port or sport = :$port )" >/dev/null 2>&1 || true
}

restore_port() {
  local port=$1
  sudo iptables -D OUTPUT -o lo -p tcp --dport "$port" -j DROP || true
  sudo iptables -D INPUT -i lo -p tcp --sport "$port" -j DROP || true
}

main() {
  echo "[$(ts)] == cutting admin-API path ($ADMIN_PORT) for the whole run =="
  drop_port "$ADMIN_PORT"

  for i in $(seq 1 "$MAX_CYCLES"); do
    if ! wait_for_pusher_state 1; then
      echo "[$(ts)] cycle $i: pusher didn't come back up within ${WAIT_UP_TIMEOUT_S}s -- assuming it's stuck, stopping"
      break
    fi
    echo "[$(ts)] cycle $i/$MAX_CYCLES: pusher confirmed up, cutting AMQP"
    drop_port "$AMQP_PORT"
    sleep "$DOWN_S"
    echo "[$(ts)] cycle $i/$MAX_CYCLES: restoring AMQP"
    restore_port "$AMQP_PORT"
  done

  echo "[$(ts)] == settling for ${SETTLE_AFTER_S}s, then restoring admin-API path =="
  sleep "$SETTLE_AFTER_S"
  restore_port "$ADMIN_PORT"

  echo "Done. Final state:"
  curl -s "$METRICS_URL" | grep wire_background_worker_running_workers
}

main "$@"
