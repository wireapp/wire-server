#!/bin/sh

# See the readme of the reaper chart.
#
# This is POSIX sh on purpose: the only actively maintained kubectl images that
# ship busybox ash, not bash.

# we loop forever, and on transient errors sleep and try again.
# setting -e would crash the pod on transient e.g. network errors, which isn't useful.
set -u
# shellcheck disable=SC3040 # busybox ash supports pipefail
set -o pipefail

USAGE="$0 <NAMESPACE> [INTERVAL_SECONDS]"
NAMESPACE="${1:?$USAGE}"
INTERVAL="${2:-15}"

echo "Using namespace: $NAMESPACE, check interval: ${INTERVAL}s"

kill_all_cannons() {
  echo "Killing all cannons"
  RAW_PODS=$(kubectl -n "$NAMESPACE" get pods 2>&1) || {
    echo "Failed to list cannon pods: $RAW_PODS. Skipping this iteration..."
    return
  }
  CANNON_PODS=$(echo "$RAW_PODS" | grep -e "cannon" | awk '{ print $1 }') || CANNON_PODS=""

  # A here-document rather than a pipeline, so the loop runs in the current
  # shell and the `exit 1` below actually terminates the script.
  while IFS= read -r cannon; do
    if [ -n "$cannon" ]; then
      echo "Deleting $cannon"
      # If a single delete fails, we skip it but keep going.
      kubectl -n "$NAMESPACE" delete pod "$cannon" || {
        echo "Failed to delete pod $cannon, crash reaper and try again"
        exit 1
      }
    fi
  done <<EOF
$CANNON_PODS
EOF
}

while true; do
  # List first, filter second. Folding both into one pipeline made an API failure.
  RAW_PODS=$(kubectl -n "$NAMESPACE" get pods --sort-by=.metadata.creationTimestamp 2>&1) || {
    echo "Failed to list pods: $RAW_PODS. Skipping this iteration..."
    sleep "$INTERVAL"
    continue
  }

  # Gather all pods that contain "cannon" or "redis-ephemeral", sorted by creation time
  ALL_PODS=$(echo "$RAW_PODS" | grep -e "cannon" -e "redis-ephemeral") || ALL_PODS=""

  # Check if we have any cannon pods at all
  if ! echo "$ALL_PODS" | grep -q "cannon"; then
    echo "No cannon pods found. Doing nothing..."
    sleep "$INTERVAL"
    continue
  fi

  # Check if we have any redis-ephemeral pods at all
  if ! echo "$ALL_PODS" | grep -q "redis-ephemeral"; then
    echo "No redis-ephemeral pod found. Doing nothing..."
    sleep "$INTERVAL"
    continue
  fi

  # At this point, we have both cannon and redis-ephemeral pods in ALL_PODS
  # Check which is oldest
  FIRST_POD=$(echo "$ALL_PODS" | head -n 1 | awk '{ print $1 }')

  if [ -z "$FIRST_POD" ]; then
    echo "Could not determine the oldest pod from the list. Doing nothing..."
    sleep "$INTERVAL"
    continue
  fi

  case "$FIRST_POD" in
    *redis-ephemeral*)
      echo "redis-ephemeral is the oldest pod, all good."
      ;;
    *)
      kill_all_cannons
      ;;
  esac

  sleep "$INTERVAL"
done
