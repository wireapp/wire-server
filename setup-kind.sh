#!/usr/bin/env bash
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

for i in $(seq 1 2); do
  kubectl create namespace "fed${i}" || true
done

for i in $(seq 1 2); do
  helm upgrade --install --namespace "fed${i}" fake-aws wire-develop/fake-aws --devel --values ./values.yaml
done
for i in $(seq 1 2); do
  helm upgrade --install --namespace "fed${i}" databases-ephemeral wire-develop/databases-ephemeral --devel --values ./values.yaml
done


# Needed because our services fail to start unless cassandra is up.
# Can't use condition=available on statefulsets due to upstream bug
# https://github.com/kubernetes/kubernetes/issues/79606
for i in $(seq 1 2); do
  kubectl --namespace "fed${i}" wait --for=condition=Ready pod --selector app=cassandra-ephemeral --timeout=90s
done

for i in $(seq 1 2); do
  helm upgrade \
    --install \
    --atomic \
    --namespace "fed${i}" \
    wire-server wire-develop/wire-server \
    --devel \
    --values ./values.yaml \
    --set brig.config.optSettings.setCookieDomain="fed${i}.svc.cluster.local"
    # TODO: Override federation-specific config here?
done

# More elegant way for the "resourcesReady" function. Waits tils fake-aws-sns is initialised
for i in $(seq 1 2); do
  kubectl wait --namespace "fed${i}" --for=condition=available deployment --selector release=wire-server
  kubectl wait --namespace "fed${i}" --for=condition=complete job --selector release=wire-server
  kubectl wait --namespace "fed${i}" --for=condition=Ready pod --selector app=fake-aws-sns
done

