#!/usr/bin/env bash
set -e

# higher not supported yet alas
kind create cluster --image kindest/node:v1.15.0 || true

export KUBECONFIG="$(kind get kubeconfig-path)"

services=(brig gundeck galley cannon proxy spar zauth nginz)
for i in ${services[@]}; do
  kind load docker-image quay.io/wire/$i:local
done

