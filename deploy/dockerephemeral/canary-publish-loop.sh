#!/usr/bin/env bash
# Publishes a message to the canary queue every INTERVAL_S seconds, so an
# AMQP-path network cut (see network-outage-iptables.sh) has actual traffic
# to fail on instead of sitting on an idle connection that neither side
# notices is broken until a heartbeat times out. Publishes directly to the
# queue via RabbitMQ's default exchange (routing_key == full queue name),
# matching how BackendNotificationPusher.hs's ensureQueue/routingKey wire
# things up.
#
# Run this in a separate shell, in parallel with
# network-outage-iptables.sh, while background-worker's canary consumer
# (backend-notifications.canary.example.com) is active. Ctrl-C to stop.
set -euo pipefail

ADMIN_URL="${ADMIN_URL:-https://localhost:15671}"
QUEUE="${QUEUE:-backend-notifications.canary.example.com}"
INTERVAL_S="${INTERVAL_S:-5}"

publish_one() {
  curl -sk -u guest:alpaca-grapefruit -X POST \
    "$ADMIN_URL/api/exchanges/%2F/amq.default/publish" \
    -H "content-type: application/json" \
    -d '{"properties":{},"routing_key":"'"$QUEUE"'","payload":"{}","payload_encoding":"string"}'
  echo
}

main() {
  echo "Publishing to '$QUEUE' every ${INTERVAL_S}s. Ctrl-C to stop."
  while true; do
    publish_one
    sleep "$INTERVAL_S"
  done
}

main "$@"
