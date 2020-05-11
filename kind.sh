#!/usr/bin/env bash
set -e

# NOTE: Currently I just copy paste the relevant things to test this interactively. This script needs rewriting

# Uncomment if you want to rebuild. Commented for now as building our docker containers is slow
# make docker-intermediate
# make docker-services

# higher not supported yet alas
# See  https://github.com/zinfra/backend-issues/issues/1231  https://github.com/zinfra/backend-issues/issues/1210
kind create cluster --config ./kind.yaml --image kindest/node:v1.15.0 || true


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
for i in $(seq 1 2); do
  kubectl -n "fed${i}" wait --for=condition=Ready pod --selector app=cassandra-ephemeral
done

for i in $(seq 1 2); do
  helm upgrade \
    --install \
    --atomic \
    --namespace "fed${i}" \
    wire-server wire-develop/wire-server \
    --devel \
    --wait \
    --values ./values.yaml \
    --set brig.config.optSettings.setCookieDomain="fed${i}.svc.cluster.local"


done

# More elegant way for the "resourcesReady" function. Waits tils fake-aws-sns is initialised
for i in $(seq 1 2); do
  kubectl wait -n "fed${i}" --for=condition=Ready pod --selector app=fake-aws-sns
done


helm test -n fed1 wire-server --logs


