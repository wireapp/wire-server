#!/usr/bin/env bash
# Storm variant of network-outage-iptables.sh: rapidly cycles the AMQP port
# down/up many times while the admin port stays cut for the whole storm,
# instead of one clean cut-and-restore.
#
# Rationale: a single clean outage mostly self-heals (the closed-handler's
# recursive connectWithRetries gets a clean shot and succeeds). Permanent
# death additionally needs a stray stale-fd/MVar-deadlock race (see
# "Bad file descriptor" / "thread blocked indefinitely in an MVar
# operation" noise in amqp-0.24.0's own thread finalisers) to land on the
# specific reconnect lineage that matters. That race is more likely when
# many reconnect attempts are in flight and overlapping, which is what
# rapid-fire short cut/restore cycles produce, versus one long outage.
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
# More, shorter cycles: STORM_UP_S is intentionally shorter than a typical
# reconnect+channel-setup cycle, so the next cut is more likely to land
# WHILE a reconnect is actively in flight rather than after it has already
# settled — that overlap is what produces fd-reuse collisions.
STORM_CYCLES="${STORM_CYCLES:-50}"
STORM_DOWN_S="${STORM_DOWN_S:-1}"
STORM_UP_S="${STORM_UP_S:-0.4}"
SETTLE_AFTER_STORM_S="${SETTLE_AFTER_STORM_S:-30}"

ts() { date -u +"%Y-%m-%d %H:%M:%S.%3N"; }

drop_port() {
  local port=$1
  sudo iptables -I OUTPUT -o lo -p tcp --dport "$port" -j DROP
  sudo iptables -I INPUT -i lo -p tcp --sport "$port" -j DROP
  # `state all` matters here: the default ss state filter is "connected"
  # only, which excludes syn-sent. Without it, a connect attempt that's
  # mid-handshake when we cut just sits in the kernel's own slow
  # SYN-retransmission backoff (~1s, doubling) instead of failing
  # immediately, decoupling what the app experiences from our cycle timing.
  sudo ss -K state all "( dport = :$port or sport = :$port )" >/dev/null 2>&1 || true
}

restore_port() {
  local port=$1
  sudo iptables -D OUTPUT -o lo -p tcp --dport "$port" -j DROP || true
  sudo iptables -D INPUT -i lo -p tcp --sport "$port" -j DROP || true
}

main() {
  echo "[$(ts)] == cutting admin-API path ($ADMIN_PORT), staying down for the whole storm =="
  drop_port "$ADMIN_PORT"

  echo "[$(ts)] == starting AMQP ($AMQP_PORT) storm: $STORM_CYCLES cycles of ${STORM_DOWN_S}s down / ${STORM_UP_S}s up =="
  # Small random jitter on top of the base durations, so fixed-period
  # toggling doesn't resonate/alias against connectWithRetries's own
  # jittered exponential backoff — varying the overlap each cycle covers
  # more of the timing space instead of repeatedly missing (or always
  # hitting) the same phase of the retry loop.
  jitter() { awk -v base="$1" 'BEGIN { srand(); print base + (rand() * base * 0.5) }'; }

  for i in $(seq 1 "$STORM_CYCLES"); do
    echo "[$(ts)] storm cycle $i/$STORM_CYCLES: cutting AMQP"
    drop_port "$AMQP_PORT"
    sleep "$(jitter "$STORM_DOWN_S")"

    echo "[$(ts)] storm cycle $i/$STORM_CYCLES: restoring AMQP"
    restore_port "$AMQP_PORT"
    sleep "$(jitter "$STORM_UP_S")"
  done

  echo "[$(ts)] == storm done, admin-API path still down for $SETTLE_AFTER_STORM_S s to let any in-flight retry loops exhaust their cap =="
  sleep "$SETTLE_AFTER_STORM_S"

  echo "[$(ts)] == restoring admin-API path (outage fully over) =="
  restore_port "$ADMIN_PORT"

  echo "Storm sequence complete. Check:"
  echo "  - wire_background_worker_running_workers{worker=\"backend-notification-pusher\"} (expect stuck at 0 if the race landed)"
  echo "  - wire_background_worker_running_workers{worker=\"background-job-consumer\"} (expect recovered to 1)"
  echo "  - background-worker logs: 'backend-notification-pusher' tagged logger going silent, no further reconnects"
}

main "$@"
