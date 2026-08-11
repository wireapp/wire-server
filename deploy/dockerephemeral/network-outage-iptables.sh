#!/usr/bin/env bash
# Drives a RabbitMQ outage via host-level iptables DROP rules, to reproduce
# the backend-notification-pusher bug described in analysis.md with a more
# realistic network partition than toxiproxy's proxy-level connection
# teardown (which can leave stale file descriptors behind, see the
# "Bad file descriptor" / MVar-deadlock noise observed with the toxiproxy
# scripts). Silently dropping packets (rather than actively refusing the
# connection) matches analysis.md's read of the production incident.
#
# Requires sudo (modifies host iptables rules) and background-worker
# pointed directly at rabbitmq's published host ports (see
# services/background-worker/background-worker.integration.yaml:
# port 5671, adminPort 15672), NOT at toxiproxy.
#
# Prereqs:
#   docker compose up -d rabbitmq
#   background-worker running with services/background-worker/background-worker.integration.yaml
set -euo pipefail

AMQP_PORT="${AMQP_PORT:-5671}"
ADMIN_PORT="${ADMIN_PORT:-15671}"
CUT_AMQP_AFTER_S="${CUT_AMQP_AFTER_S:-20}"
CUT_ADMIN_AFTER_S="${CUT_ADMIN_AFTER_S:-150}"

ts() { date -u +"%Y-%m-%d %H:%M:%S.%3N"; }

drop_port() {
  local port=$1
  # dport on OUTPUT blocks packets we send to the broker; sport on INPUT
  # blocks the broker's own pushed data/heartbeats/acks coming back to us.
  # Both are needed to fully cut a loopback TCP session in both directions.
  sudo iptables -I OUTPUT -o lo -p tcp --dport "$port" -j DROP
  sudo iptables -I INPUT -i lo -p tcp --sport "$port" -j DROP
  # Existing connections stay ESTABLISHED and idle until something tries to
  # use them or a heartbeat times out. Force it: RST any current sockets on
  # this port (both the docker-proxy-facing leg on 127.0.0.1 and the
  # proxy-to-container leg on the docker bridge) so the client notices
  # immediately instead of waiting.
  sudo ss -K "( dport = :$port or sport = :$port )" >/dev/null 2>&1 || true
}

restore_port() {
  local port=$1
  # `|| true`: don't let a missing/stale rule (e.g. left over from an
  # interrupted prior run) abort the script via `set -e` and skip
  # restoring everything else.
  sudo iptables -D OUTPUT -o lo -p tcp --dport "$port" -j DROP || true
  sudo iptables -D INPUT -i lo -p tcp --sport "$port" -j DROP || true
}

main() {
  echo "[$(ts)] == cutting both AMQP ($AMQP_PORT) and admin-API ($ADMIN_PORT) paths =="
  drop_port "$AMQP_PORT"
  drop_port "$ADMIN_PORT"

  sleep "$CUT_AMQP_AFTER_S"

  echo "[$(ts)] == restoring AMQP path only (admin API stays down) =="
  restore_port "$AMQP_PORT"

  sleep "$CUT_ADMIN_AFTER_S"

  echo "[$(ts)] == restoring admin-API path (outage fully over) =="
  restore_port "$ADMIN_PORT"

  echo "Outage sequence complete. Check:"
  echo "  - wire_background_worker_running_workers{worker=\"backend-notification-pusher\"} (expect stuck at 0)"
  echo "  - wire_background_worker_running_workers{worker=\"background-job-consumer\"} (expect recovered to 1)"
  echo "  - background-worker logs: 'backend-notification-pusher' tagged logger going silent"
}

main "$@"
