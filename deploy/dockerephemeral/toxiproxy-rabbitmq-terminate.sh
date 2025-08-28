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
  local body="${resp%$'\n'*}"
  if [[ "$code" == 2* ]]; then
    echo "Applied toxic '$name' ($type/$stream) -> HTTP $code"
    echo "$body"
    return 0
  else
    echo "Error adding toxic '$name' ($type/$stream) to proxy '$proxy':" >&2
    echo "HTTP $code" >&2
    echo "Response: $body" >&2
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
  local body="${resp%$'\n'*}"
  echo "Toggled proxy '$proxy' enabled=$enabled -> HTTP $code"
  echo "$body"
  if [[ "$code" != 2* ]]; then
    echo "Error toggling proxy state." >&2
    return 1
  fi
}

remove_toxic() {
  local proxy="$1" name="$2" resp code body
  resp=$(curlx -X DELETE -w "\n%{http_code}" "$TOXIPROXY_API/proxies/$proxy/toxics/$name") || true
  code="${resp##*$'\n'}"
  body="${resp%$'\n'*}"
  echo "Remove toxic '$name' from proxy '$proxy' -> HTTP $code"
  [ -n "$body" ] && echo "$body"
  # don't fail hard on non-2xx; cleanup is best-effort
  return 0
}

cleanup_toxics() {
  local proxy="$1" name
  for name in down force_rst_up force_rst_down blackhole_up blackhole_down odd_fin_up odd_fin_down odd_slow_close; do
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
echo "  5) down"
echo "  6) Handshake choke (slice+bandwidth+jitter)"
echo "  7) Heartbeat killer (high latency both directions)"
echo "  8) Intermittent flaps (toggle proxy on/off)"
echo
read -r -p "Enter choice [1-8]: " choice

case "${choice}" in
  1)
    proxy=$(prompt_proxy)
    echo "Applying Abrupt RST on proxy '${proxy}'..."
    cleanup_toxics "$proxy" # ensure clean slate
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
    cleanup_toxics "$proxy" # ensure clean slate
    # timeout toxic with timeout=0 drops all traffic indefinitely (blackhole)
    if add_toxic "$proxy" blackhole_down timeout downstream '{"timeout":0}' && \
       add_toxic "$proxy" blackhole_up timeout upstream '{"timeout":0}'; then
      echo "Blackhole toxics applied. Press Enter to remove them and restore normal traffic."
      read -r _
      cleanup_toxics "$proxy"
    else
      echo "Failed to apply blackhole toxics." >&2
      cleanup_toxics "$proxy"
      exit 1
    fi
    ;;
  3)
    proxy=$(prompt_proxy)
    echo "Applying Odd/Graceful FIN on proxy '${proxy}'..."
    cleanup_toxics "$proxy" # ensure clean slate
    # Limit bytes to force mid-stream termination then close slowly (FIN)
    if add_toxic "$proxy" odd_fin_down limit_data downstream '{"bytes":64}' && \
       add_toxic "$proxy" odd_fin_up limit_data upstream '{"bytes":64}' && \
       add_toxic "$proxy" odd_slow_close slow_close downstream '{"delay":1000}'; then
      echo "Odd FIN toxics applied. Press Enter to remove them and restore normal traffic."
      read -r _
      cleanup_toxics "$proxy"
    else
      echo "Failed to apply odd FIN toxics." >&2
      cleanup_toxics "$proxy"
      exit 1
    fi
    ;;
  4)
    echo "Gracefully closing all RabbitMQ connections via rabbitmqctl..."
    # This requests the broker to gracefully close connections (FIN)
    docker compose -f "$COMPOSE_FILE" exec -T "$RABBITMQ_SVC" \
      rabbitmqctl close_all_connections "Closed by toxiproxy-rabbitmq-terminate.sh" || true
    echo "Requested graceful termination on the broker."
    ;;
  5)
    echo "down"
    proxy=$(prompt_proxy)
    echo "Applying Silent Black Hole on proxy '${proxy}'..."
    cleanup_toxics "$proxy" # ensure clean slate
    # timeout toxic with timeout=0 drops all traffic indefinitely (blackhole)
    if add_toxic "$proxy" down '{"timeout":0}';  then
      echo "down toxics applied. Press Enter to remove them and restore normal traffic."
      read -r _
      cleanup_toxics "$proxy"
    else
      echo "Failed to apply down toxics." >&2
      cleanup_toxics "$proxy"
      exit 1
    fi
    ;;
  6)
    proxy=$(prompt_proxy)
    echo "Applying handshake choke on proxy '${proxy}'..."
    cleanup_toxics "$proxy"
    if add_toxic "$proxy" hs_slice_dn slicer downstream '{"average_size":8,"size_variation":5,"delay":5}' && \
       add_toxic "$proxy" hs_bw_dn bandwidth downstream '{"rate":1500}' && \
       add_toxic "$proxy" hs_jit_dn latency downstream '{"latency":80,"jitter":60,"correlation":0.3}'; then
      echo "Handshake choke applied. Press Enter to remove and restore traffic."
      read -r _
      cleanup_toxics "$proxy"
    else
      echo "Failed to apply handshake choke toxics." >&2
      cleanup_toxics "$proxy"
      exit 1
    fi
    ;;
  7)
    proxy=$(prompt_proxy)
    echo "Applying heartbeat killer (latency both directions) on proxy '${proxy}'..."
    cleanup_toxics "$proxy"
    if add_toxic "$proxy" hb_lat_dn latency downstream '{"latency":65000,"jitter":15000,"correlation":0.2}' && \
       add_toxic "$proxy" hb_lat_up latency upstream '{"latency":65000,"jitter":15000,"correlation":0.2}'; then
      echo "Heartbeat killer applied. Press Enter to remove and restore traffic."
      read -r _
      cleanup_toxics "$proxy"
    else
      echo "Failed to apply heartbeat killer toxics." >&2
      cleanup_toxics "$proxy"
      exit 1
    fi
    ;;
  8)
    proxy=$(prompt_proxy)
    read -r -p "Flap duration seconds [5]: " flap_dur
    read -r -p "Flap cycles [3]: " flap_cycles
    flap_dur=${flap_dur:-5}
    flap_cycles=${flap_cycles:-3}
    echo "Flapping proxy '${proxy}' ${flap_cycles}x with ${flap_dur}s intervals..."
    i=1
    while [ "$i" -le "$flap_cycles" ]; do
      echo "Cycle $i: disabling..."
      set_proxy_enabled "$proxy" false || { echo "Failed to disable proxy." >&2; break; }
      sleep "$flap_dur"
      echo "Cycle $i: enabling..."
      set_proxy_enabled "$proxy" true || { echo "Failed to enable proxy." >&2; break; }
      sleep "$flap_dur"
      i=$((i+1))
    done
    echo "Ensuring proxy is enabled..."
    set_proxy_enabled "$proxy" true || true
    ;;
  *)
    echo "Invalid choice. Exiting." >&2
    exit 1
    ;;
esac

echo "Done."
