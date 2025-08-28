#!/usr/bin/env bash
set -euo pipefail

COMPOSE_FILE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="$COMPOSE_FILE_DIR/docker-compose.yaml"
TOXIPROXY_SVC="toxiproxy"
RABBITMQ_SVC="rabbitmq"

# Primary proxy to target; adjust if you want to target others
DEFAULT_PROXY="rabbitmq-amqp-tls"

exec_toxi() {
  docker compose -f "$COMPOSE_FILE" exec -T "$TOXIPROXY_SVC" toxiproxy-cli "$@"
}

cleanup_toxics() {
  local proxy="$1" name
  for name in force_rst blackhole_up blackhole_down odd_fin_up odd_fin_down odd_slow_close; do
    if ! exec_toxi toxic remove "$proxy" -n "$name" >/dev/null 2>&1; then
      true
    fi
  done
}

prompt_proxy() {
  local proxy
  read -r -p "Proxy to target [${DEFAULT_PROXY}]: " proxy || proxy=""
  proxy=${proxy:-$DEFAULT_PROXY}
  echo "$proxy"
}

echo "Choose termination mode:"
echo "  1) Abrupt RST (reset_peer)"
echo "  2) Silent black hole (timeout both directions)"
echo "  3) Odd/graceful FIN at weird moment (limit_data + slow_close)"
echo "  4) Graceful termination (RabbitMQ close_all_connections)"
echo
read -r -p "Enter choice [1-4]: " choice

case "${choice}" in
  1)
    proxy=$(prompt_proxy)
    echo "Applying Abrupt RST on proxy '${proxy}'..."
    # Sends TCP RST to peer immediately
    exec_toxi toxic add "$proxy" -n force_rst -t reset_peer -d downstream || true
    exec_toxi toxic add "$proxy" -n force_rst -t reset_peer -d upstream || true
    echo "RST toxic applied. Press Enter to remove it and restore normal traffic."
    read -r _
    cleanup_toxics "$proxy"
    ;;
  2)
    proxy=$(prompt_proxy)
    echo "Applying Silent Black Hole on proxy '${proxy}'..."
    # timeout toxic with timeout=0 drops all traffic indefinitely (blackhole)
    exec_toxi toxic add "$proxy" -n blackhole_down -t timeout -d downstream -a timeout=0 || true
    exec_toxi toxic add "$proxy" -n blackhole_up -t timeout -d upstream -a timeout=0 || true
    echo "Blackhole toxics applied. Press Enter to remove them and restore normal traffic."
    read -r _
    cleanup_toxics "$proxy"
    ;;
  3)
    proxy=$(prompt_proxy)
    echo "Applying Odd/Graceful FIN on proxy '${proxy}'..."
    # Limit bytes to force mid-stream termination then close slowly (FIN)
    exec_toxi toxic add "$proxy" -n odd_fin_down -t limit_data -d downstream -a bytes=64 || true
    exec_toxi toxic add "$proxy" -n odd_fin_up -t limit_data -d upstream -a bytes=64 || true
    exec_toxi toxic add "$proxy" -n odd_slow_close -t slow_close -d downstream -a delay=1000 || true
    echo "Odd FIN toxics applied. Press Enter to remove them and restore normal traffic."
    read -r _
    cleanup_toxics "$proxy"
    ;;
  4)
    echo "Gracefully closing all RabbitMQ connections via rabbitmqctl..."
    # This requests the broker to gracefully close connections (FIN)
    docker compose -f "$COMPOSE_FILE" exec -T "$RABBITMQ_SVC" \
      rabbitmqctl close_all_connections "Closed by toxiproxy-rabbitmq-terminate.sh" || true
    echo "Requested graceful termination on the broker."
    ;;
  *)
    echo "Invalid choice. Exiting." >&2
    exit 1
    ;;
esac

echo "Done."

