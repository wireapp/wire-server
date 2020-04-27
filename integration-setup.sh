#!/usr/bin/env bash

USAGE="Usage: $0 [-c chart-version] [-i image-version] [ -r repository (default: wire-develop) ]"

REPOSITORY="wire-develop"

# https://sookocheff.com/post/bash/parsing-bash-script-arguments-with-shopts/
while getopts ":i:c:r:" opt; do
  case ${opt} in
    i ) IMAGE_VERSION_OVERRIDE="$OPTARG"
      ;;
    c ) chartversion="$OPTARG"
      ;;
    r ) REPOSITORY="$OPTARG"
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

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."

NAMESPACE=${NAMESPACE:-test-integration}
PARENT_PID=$$

function kill_all() {
    # kill the process tree of the PARENT_PID
    kill -9 -${PARENT_PID} &> /dev/null
}
function list_descendants () {
  local children
  children="$(pgrep -P "$1")"
  for pid in $children
  do
    list_descendants "$pid"
  done
  echo "$children"
}
function kill_gracefully() {
    kill $(list_descendants "$PARENT_PID") &> /dev/null
}

trap "kill_gracefully; kill_all" INT TERM ERR

set -e

# ${DIR}/bin/integration-cleanup.sh

charts=( fake-aws databases-ephemeral wire-server )

helm repo add incubator https://kubernetes-charts-incubator.storage.googleapis.com

# nothing serves on localhost, remove that repo
helm repo remove local 2&> /dev/null || true

# Add the requested repository. We only support wire and wire-deveop
case "$REPOSITORY" in
    "wire" )
        helm repo add wire https://s3-eu-west-1.amazonaws.com/public.wire.com/charts
        ;;
    "wire-develop" )
        helm repo add wire-develop https://s3-eu-west-1.amazonaws.com/public.wire.com/charts-develop
        ;;
    *)
        echo "repository must be one of wire, wire-develop" 1>&2
        exit 1
esac

helm repo update

kubectl create namespace $NAMESPACE || true
echo "Installing charts..."

function printLogs() {
    while true; do
        sleep 20
        kubectl -n ${NAMESPACE} get pods | grep -v Running | grep -v Pending | grep -v Completed | grep -v STATUS | grep -v ContainerCreating | awk '{print $1}' | xargs -n 1 -I{} bash -c "printf '\n\n----LOGS FROM {}:\n'; kubectl -n ${NAMESPACE} logs --tail=30 {}" || true
        kubectl -n ${NAMESPACE} get pods | grep Pending | awk '{print $1}' | xargs -n 1 -I{} bash -c "printf '\n\n----DESCRIBE 'pending' {}:\n'; kubectl -n ${NAMESPACE} describe pod {}" || true
    done
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


if [ -n "$chartversion" ]; then
    optionVersion="--version $chartversion"
fi
optionForBrig="--set brig.config.optSettings.setCookieDomain=$NAMESPACE.svc.cluster.local"
set -x
for chart in "${charts[@]}"; do
    valuesfile="./values.yaml"
    if [ -f "$valuesfile" ]; then
        option="-f $valuesfile"
    fi
    helm upgrade --install --namespace "${NAMESPACE}" "${NAMESPACE}-${chart}" "${REPOSITORY}/${chart}" \
        $option \
        $optionVersion \
        $imageVersionOverrideOptions \
        $optionForBrig \
        --devel \
        --wait
done

# wait for fakeSNS to create resources. TODO, cleaner: make initiate-fake-aws-sns a post hook. See cassandra-migrations chart for an example.
resourcesReady() {
    SNS_POD=$(kubectl -n "${NAMESPACE}" get pods | grep fake-aws-sns | grep Running | awk '{print $1}')
    kubectl -n "${NAMESPACE}" logs "$SNS_POD" -c initiate-fake-aws-sns | grep created
}
until resourcesReady; do echo 'waiting for SNS resources'; sleep 1; done

echo done
