#!/usr/bin/env bash

# TODO: make configurable
number_of_instances=2

# TODO: Make configurable
# whether to expose this wire-server to the host. e.g when running federation
# integration tests locally; and you need a second copy of the backend to calk to
expose=0

# TODO: Make this usable both in integration tests and locally in kind; by
# overriding specific ./values.yaml for those twoscenarios
for i in $(seq 1 $number_of_instances); do
  helm upgrade \
    --install \
    --atomic \
    --namespace "fed${i}" \
    wire-server wire-develop/wire-server \
    --devel \
    --values ./values.yaml \
    --set brig.config.optSettings.setCookieDomain="fed${i}.svc.cluster.local" \
    --wait
done


# TODO always urn this?
helm test -n fed1 wire-server --logs --timeout=500s
