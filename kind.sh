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
done

# TODO brig option domain

for i in $(seq 1 2); do
  kubectl create namespace "fed${i}" || true
done

for i in $(seq 1 2); do
  helm upgrade --install --namespace "fed${i}" fake-aws wire-develop/fake-aws --devel --wait --values ./values.yaml --wait
done
for i in $(seq 1 2); do
  helm upgrade --install --namespace "fed${i}" databases-ephemeral wire-develop/databases-ephemeral --devel --wait --values ./values.yaml --wait
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

  # wait for fakeSNS to create resources. TODO, cleaner: make
  # initiate-fake-aws-sns a post hook. See cassandra-migrations chart for an
  # example.
  resourcesReady() {
      SNS_POD=$(kubectl -n "fed${i}" get pods | grep fake-aws-sns | grep Running | awk '{print $1}')
      kubectl -n "fed${i}" logs "$SNS_POD" -c initiate-fake-aws-sns | grep created
  }
  until resourcesReady; do echo 'waiting for SNS resources'; sleep 1; done
done

