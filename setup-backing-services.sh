#!/usr/bin/env bash
set -e

echo "Deploying backing services to kubernetes cluster"
echo "Current context: $(kubectl config current-context)"
kubectl cluster-info


number_of_instances=2

for i in $(seq 1 $number_of_instances); do
  kubectl create namespace "fed${i}" || true
done

for i in $(seq 1 $number_of_instances); do
  helm upgrade \
    --install \
    --namespace "fed${i}" \
    --devel \
    --values ./values.yaml \
    fake-aws wire-develop/fake-aws
done

for i in $(seq 1 $number_of_instances); do
  helm upgrade \
    --install \
    --namespace "fed${i}" \
    --devel \
    --values ./values.yaml \
    databases-ephemeral wire-develop/databases-ephemeral
done


# Needed because our services fail to start unless cassandra is up.
# Can't use condition=available on statefulsets due to upstream bug
# https://github.com/kubernetes/kubernetes/issues/79606
# not using --wait above so that the two can start up in parallel
for i in $(seq 1 $number_of_instances); do
  kubectl --namespace "fed${i}" wait --for=condition=Ready pod --selector app=cassandra-ephemeral --timeout=90s
  kubectl --namespace "fed${i}" wait --for=condition=Ready pod --selector app=fake-aws-sns
done

echo "Backing services now up and running!"
