#!/usr/bin/env bash
# Removes any leftover DROP rules from network-outage-iptables.sh, in case a
# run was interrupted before it could restore both ports itself. Safe to
# run even if no rules are present (ignores "rule does not exist" errors).
set -uo pipefail

AMQP_PORT="${AMQP_PORT:-5671}"
ADMIN_PORT="${ADMIN_PORT:-15671}"

remove_all_matching() {
  local port=$1
  while sudo iptables -D OUTPUT -o lo -p tcp --dport "$port" -j DROP 2>/dev/null; do :; done
  while sudo iptables -D INPUT -i lo -p tcp --sport "$port" -j DROP 2>/dev/null; do :; done
}

main() {
  remove_all_matching "$AMQP_PORT"
  remove_all_matching "$ADMIN_PORT"
  echo "iptables reset: no DROP rules left for ports $AMQP_PORT/$ADMIN_PORT."
}

main "$@"
