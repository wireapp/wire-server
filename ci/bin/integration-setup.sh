#!/usr/bin/env bash

USAGE="Usage: $0 [-i image-version]"

# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":i:" opt; do
  case ${opt} in
    i ) IMAGE_VERSION_OVERRIDE="$OPTARG"
      ;;
    : ) echo "-$OPTARG" requires an argument 1>&2
        exit 1
      ;;
    \? ) echo "$USAGE" 1>&2
         exit 1
      ;;
  esac
done
shift $((OPTIND -1))

if [ "$#" -ne 0 ]; then
  echo "$USAGE" 1>&2
  exit 1
fi;

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

NAMESPACE=${NAMESPACE:-test-integration}

kubectl create namespace "${NAMESPACE}" > /dev/null 2>&1 || true

set -e

${DIR}/integration-cleanup.sh


REPOSITORY="${DIR}/../../deploy/charts"
charts=( fake-aws databases-ephemeral wire-server )

echo "updating recursive dependencies ..."
for chart in "${charts[@]}"; do
  "$DIR/update.sh" "$REPOSITORY/$chart"
done

echo "Installing charts..."

function printLogs() {
    kubectl -n ${NAMESPACE} get pods | grep -v Running | grep -v Pending | grep -v Completed | grep -v STATUS | grep -v ContainerCreating | awk '{print $1}' | xargs -n 1 -I{} bash -c "printf '\n\n----LOGS FROM {}:\n'; kubectl -n ${NAMESPACE} logs --tail=30 {}" || true
    kubectl -n ${NAMESPACE} get pods | grep Pending | awk '{print $1}' | xargs -n 1 -I{} bash -c "printf '\n\n----DESCRIBE 'pending' {}:\n'; kubectl -n ${NAMESPACE} describe pod {}" || true
}

# construct override options if we want to run using a specific docker image version for our services
imageVersionOverrideOptions=""
if [ -n "$IMAGE_VERSION_OVERRIDE" ]; then
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set brig.image.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set cannon.image.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set gundeck.image.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set galley.image.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set spar.image.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set cargohold.image.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set proxy.image.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set elasticsearch-index.image.tag=$IMAGE_VERSION_OVERRIDE"

    # Some services have the tag at a slightly different location:
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set nginz.images.nginz.tag=$IMAGE_VERSION_OVERRIDE"
    imageVersionOverrideOptions="$imageVersionOverrideOptions --set cassandra-migrations.images.tag=$IMAGE_VERSION_OVERRIDE"
fi;

set -x

for chart in "${charts[@]}"; do
    kubectl -n ${NAMESPACE} get pods
    valuesfile="${DIR}/../helm_vars/${chart}/values.yaml"
    if [ -f "$valuesfile" ]; then
        option="-f $valuesfile"
    else
        option=""
    fi
    helm upgrade --install --namespace "${NAMESPACE}" "${NAMESPACE}-${chart}" "${REPOSITORY}/${chart}" \
        $option \
        $imageVersionOverrideOptions \
        --wait
done

kubectl -n ${NAMESPACE} get pods
printLogs

# wait for fakeSNS to create resources. TODO, cleaner: make initiate-fake-aws-sns a post hook. See cassandra-migrations chart for an example.
resourcesReady() {
    SNS_POD=$(kubectl -n "${NAMESPACE}" get pods | grep fake-aws-sns | grep Running | awk '{print $1}')
    kubectl -n "${NAMESPACE}" logs "$SNS_POD" -c initiate-fake-aws-sns | grep created
}
until resourcesReady; do echo 'waiting for SNS resources'; sleep 1; done

kubectl -n ${NAMESPACE} get pods
printLogs

echo "done"
