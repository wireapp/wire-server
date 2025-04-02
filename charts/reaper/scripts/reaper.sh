#!/usr/bin/env bash

# See the readme of the reaper chart.

# we loop forever, and on transient errors sleep and try again.
# setting -e would crash the pod on transient e.g. network errors, which isn't useful.
set -uo pipefail

USAGE="$0 <NAMESPACE>"
NAMESPACE="${1:?$USAGE}"

echo "Using namespace: $NAMESPACE"

kill_all_cannons() {
  echo "Killing all cannons"
  CANNON_PODS=$(kubectl -n "$NAMESPACE" get pods 2>/dev/null \
    | grep -e "cannon" \
    | awk '{ print $1 }') || {
    echo "Failed to list cannon pods. Skipping this iteration..."
    return
  }

  while IFS= read -r cannon; do
    if [ -n "$cannon" ]; then
      echo "Deleting $cannon"
      # If a single delete fails, we skip it but keep going.
      kubectl -n "$NAMESPACE" delete pod "$cannon" || {
        echo "Failed to delete pod $cannon, crash reaper and try again"
        exit 1
      }
    fi
  done <<< "$CANNON_PODS"
}

while true; do
  # Gather all pods that contain "cannon" or "redis-ephemeral", sorted by creation time
  ALL_PODS=$(kubectl -n "$NAMESPACE" get pods --sort-by=.metadata.creationTimestamp 2>/dev/null \
    | grep -e "cannon" -e "redis-ephemeral") || {
      echo "Failed to list pods. Skipping this iteration..."
      sleep 60
      continue
  }

  # Check if we have any cannon pods at all
  if ! echo "$ALL_PODS" | grep -q "cannon"; then
    echo "No cannon pods found. Doing nothing..."
    sleep 60
    continue
  fi

  # Check if we have any redis-ephemeral pods at all
  if ! echo "$ALL_PODS" | grep -q "redis-ephemeral"; then
    echo "No redis-ephemeral pod found. Doing nothing..."
    sleep 60
    continue
  fi

  # At this point, we have both cannon and redis-ephemeral pods in ALL_PODS
  # Check which is oldest
  FIRST_POD=$(echo "$ALL_PODS" | head -n 1 | awk '{ print $1 }')

  if [ -z "$FIRST_POD" ]; then
    echo "Could not determine the oldest pod from the list. Doing nothing..."
    sleep 60
    continue
  fi

  if [[ "$FIRST_POD" =~ "redis-ephemeral" ]]; then
    echo "redis-ephemeral is the oldest pod, all good."
  else
    kill_all_cannons
  fi

  echo "Sleep 1"
  sleep 1
done

