#!/usr/bin/env bash
# Sets up a local kubernetes cluster using kind and sets the kubectl context to
# talk to it.
set -e

kind create cluster --config ./kind.yaml --image kindest/node:v1.15.0 || (echo "Try 'kind delete cluster' first." && exit 1)

# TODO parameter for how many instances of backend to run
# Scenario 1:
#   in kind:
#     wire-server + databases + tests
# Scenario 2 (Testing federation in CI):
#   in kind:
#      2 * (wire-server + databases)  + tests
# Scenario 3: (Run integration tests locally)
#   in kind:
#      1 * databases
#    locally:
#      1 * (wire-server + tests)
# Scenario 4: (Run federation integration tests locally)
#   in kind
#      1 * databases + 1 * (wire-server + databases)
#   locally:
#        1 * (wire-server + tests)

# TODO would be easier to just use one image IMO
services=(brig gundeck cargohold galley cannon proxy spar zauth nginz)
for i in ${services[@]}; do
  kind load docker-image quay.io/wire/$i:local
  kind load docker-image quay.io/wire/$i-integration:local || true # some components dont have integration tests? ignore: HACK
done


# Kind _should_ already set this but still to it to be sure
kubectl config use-context kind-kind
