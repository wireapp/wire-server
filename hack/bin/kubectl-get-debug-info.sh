#!/usr/bin/env bash

USAGE="$0 <NAMESPACE>"
NAMESPACE=${1:?$USAGE}

echo "Checking pods in namespace '${NAMESPACE}' that failed to schedule..."

# Get pods that failed to schedule
UNSCHEDULED_PODS=$(kubectl get pods --namespace "$NAMESPACE" -o json | jq -r '.items[] | select(.status.phase=="Pending") | .metadata.name')

for POD in $UNSCHEDULED_PODS; do
  echo "Pod $POD failed to schedule for the following reasons:"
  # Get events for pod
  kubectl describe pod "$POD" --namespace "$NAMESPACE" | grep -A 10 "Events:"
  echo ""
done

echo "Checking pods in namespace '${NAMESPACE}' that are crashlooping..."

# Get pods that are crashlooping
CRASHLOOPING_PODS=$(kubectl get pods --namespace "$NAMESPACE" -o json | jq -r '.items[] | select(.status.containerStatuses[]?.state.waiting.reason=="CrashLoopBackOff") | .metadata.name')

for POD in $CRASHLOOPING_PODS; do
  echo "Pod $POD is crashlooping for the following reasons:"
  # Get logs of previous run for pod
  kubectl logs "$POD" --namespace "$NAMESPACE" --previous
  echo ""
done

