#!/usr/bin/env bash
# shellcheck disable=SC2034
set -euo pipefail

COMPOSE_FILE_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="$COMPOSE_FILE_DIR/docker-compose.yaml"
TOXIPROXY_SVC="toxiproxy"
RABBITMQ_SVC="rabbitmq"

# Primary proxy to target; adjust if you want to target others
DEFAULT_PROXY="rabbitmq-amqp-tls"

curlx() {
  if command -v curl >/dev/null 2>&1; then
    curl -sS "$@"
  else
    # Fallback to using the compose service with curl
    docker compose -f "$COMPOSE_FILE" run --rm --no-deps --entrypoint curl init_vhosts -sS "$@"
  fi
}

TOXIPROXY_API="${TOXIPROXY_API:-http://127.0.0.1:8474}"

add_toxic() {
  local proxy="$1" name="$2" type="$3" stream="$4" attrs_json="$5"
  local resp code
  resp=$(curlx -X POST -H 'Content-Type: application/json' \
    --data "{\"name\":\"$name\",\"type\":\"$type\",\"stream\":\"$stream\",\"attributes\":$attrs_json}" \
    -w "\n%{http_code}" "$TOXIPROXY_API/proxies/$proxy/toxics") || true
  code="${resp##*$'\n'}"
  if [[ "$code" != 2* ]]; then
    echo "Error adding toxic '$name' ($type/$stream) to proxy '$proxy':" >&2
    echo "HTTP $code" >&2
    echo "Response: ${resp%$'\n'*}" >&2
    return 1
  fi
}

# Enable/disable proxy (fallback to simulate abrupt disconnect)
set_proxy_enabled() {
  local proxy="$1" enabled="$2" resp code
  resp=$(curlx -X POST -H 'Content-Type: application/json' \
    --data "{\"enabled\":$enabled}" \
    -w "\n%{http_code}" "$TOXIPROXY_API/proxies/$proxy") || true
  code="${resp##*$'\n'}"
  if [[ "$code" != 2* ]]; then
    echo "Error toggling proxy '$proxy' enabled=$enabled" >&2
    echo "HTTP $code" >&2
    echo "Response: ${resp%$'\n'*}" >&2
    return 1
  fi
}

remove_toxic() {
  local proxy="$1" name="$2"
  curlx -X DELETE --fail "$TOXIPROXY_API/proxies/$proxy/toxics/$name" >/dev/null || true
}

cleanup_toxics() {
  local proxy="$1" name
  for name in force_rst_up force_rst_down blackhole_up blackhole_down odd_fin_up odd_fin_down odd_slow_close; do
    remove_toxic "$proxy" "$name"
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
    # Sends TCP RST to peer immediately (both directions)
    if add_toxic "$proxy" force_rst_down reset_peer downstream '{}' && \
       add_toxic "$proxy" force_rst_up reset_peer upstream '{}'; then
      echo "RST toxic applied. Press Enter to remove it and restore normal traffic."
      read -r _
      cleanup_toxics "$proxy"
    else
      echo "reset_peer toxic not supported; falling back to disabling proxy (abrupt disconnect)." >&2
      set_proxy_enabled "$proxy" false || { echo "Failed to disable proxy." >&2; exit 1; }
      echo "Proxy disabled. Existing connections should drop immediately. Press Enter to re-enable."
      read -r _
      set_proxy_enabled "$proxy" true || { echo "Failed to re-enable proxy." >&2; exit 1; }
    fi
    ;;
  2)
    proxy=$(prompt_proxy)
    echo "Applying Silent Black Hole on proxy '${proxy}'..."
    # timeout toxic with timeout=0 drops all traffic indefinitely (blackhole)
    add_toxic "$proxy" blackhole_down timeout downstream '{"timeout":0}'
    add_toxic "$proxy" blackhole_up timeout upstream '{"timeout":0}'
    echo "Blackhole toxics applied. Press Enter to remove them and restore normal traffic."
    read -r _
    cleanup_toxics "$proxy"
    ;;
  3)
    proxy=$(prompt_proxy)
    echo "Applying Odd/Graceful FIN on proxy '${proxy}'..."
    # Limit bytes to force mid-stream termination then close slowly (FIN)
    add_toxic "$proxy" odd_fin_down limit_data downstream '{"bytes":64}'
    add_toxic "$proxy" odd_fin_up limit_data upstream '{"bytes":64}'
    add_toxic "$proxy" odd_slow_close slow_close downstream '{"delay":1000}'
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
