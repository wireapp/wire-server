#!/usr/bin/env bash
#
# Script to delete any leftover ingress classes named `nginx-test-` older than 2 hours, which are deemed inactive
#
set -euo pipefail

# Get the current time in seconds since the epoch
now=$(date +%s)

# Create a temporary file to store names of IngressClasses to delete
tmpfile=$(mktemp)

# List all IngressClasses, filter for names starting with "nginx-test-" and that are older than 24 hours.
# This uses jq’s `fromdateiso8601` to convert the creationTimestamp to epoch seconds.
kubectl get ingressclasses -o json | jq -r --argjson now "$now" '
  .items[]
  | select(.metadata.name | startswith("nginx-test-"))
  | .metadata as $meta
  | ($meta.creationTimestamp | fromdateiso8601) as $created
  | if ($now - $created > 86400) then $meta.name else empty end
' > "$tmpfile"

if [ ! -s "$tmpfile" ]; then
  echo "No IngressClasses older than 24 hours found for deletion."
  rm "$tmpfile"
  exit 0
fi

echo "Found the following IngressClasses to delete:"
cat "$tmpfile"
echo

# Use GNU Parallel to delete the IngressClasses in batches of 20.
parallel -j20 --line-buffer '
  echo "Deleting IngressClass: {}"
  kubectl delete ingressclass {}
' < "$tmpfile"

# Clean up
rm "$tmpfile"
