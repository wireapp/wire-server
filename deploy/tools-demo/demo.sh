#!/usr/bin/env bash

# You can use this script to simply start the tools demo ./demo.sh

set -eo pipefail

USAGE="$0 [docker]"
MODE="$1"
docker_deployment="false"
if [ "$MODE" = "docker" ]; then
    docker_deployment="true"
fi
TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"
DIR="${TOP_LEVEL}/tools"
PARENT_PID=$$
rm -f /tmp/demo.* # remove previous temp files, if any

function kill_all() {
    # kill the process tree of the PARENT_PID
    kill -9 -${PARENT_PID} &> /dev/null
}

function list_descendants () {
  local children=$(pgrep -P "$1")
  for pid in $children
  do
    list_descendants "$pid"
  done
  echo "$children"
}

function kill_gracefully() {
    pkill "stern"
    sleep 1
    kill $(list_descendants $PARENT_PID) &> /dev/null
}

trap "kill_gracefully; kill_all" INT TERM ERR

function check_prerequisites() {
    # Let's check for dependent services
    nc -z 127.0.0.1 8082 \
        && nc -z 127.0.0.1 8085 \
        && nc -z 127.0.0.1 8086 \
        || { echo "Some dependent services not up. Maybe run 'deploy/services-demo/demo.sh' in a separate terminal first?";  exit 1; }
    if [ "$docker_deployment" = "false" ]; then
        test -f ${DIR}/../dist/stern \
        || { echo "Not all tools are compiled. How about you run 'cd ${TOP_LEVEL} && make services' first?"; exit 1; }
    fi
}

blue=6
white=7
green=10
orange=3
yellow=11
purpleish=13
redish=1
blueish=4

function run_haskell_service() {
    service=$1
    colour=$2
    (cd ${SCRIPT_DIR} && ${DIR}/../dist/${service} -c ${SCRIPT_DIR}/conf/${service}.demo.yaml || kill_all) \
        | sed -e "s/^/$(tput setaf ${colour})[${service}] /" -e "s/$/$(tput sgr0)/" &
}

function get_backoffice () {
    curl -L https://s3-eu-west-1.amazonaws.com/public.wire.com/artifacts/backoffice-0.1.7%2B14.tar.gz -o backoffice.tar.gz
    mkdir -p conf/nginx/swagger-ui/api-docs
    tar zxvf backoffice.tar.gz -C conf/nginx/swagger-ui --strip-components=1
    rm backoffice.tar.gz
    cp -r conf /tmp
    cp conf/nginx/resources.json /tmp/conf/nginx/swagger-ui/api-docs
}

# Let's front this with nginx running on docker only
function run_nginx() {
    colour=$1
    prefix=$([ -w /usr/local ] && echo /usr/local || echo "${HOME}/.wire-dev")
    (cd ${SCRIPT_DIR} && LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${prefix}/lib/ /usr/local/bin/nginx -p ${SCRIPT_DIR} -c ${SCRIPT_DIR}/conf/nginx/nginx.conf -g 'daemon off;' || kill_all) \
        | sed -e "s/^/$(tput setaf ${colour})[nginz] /" -e "s/$/$(tput sgr0)/" &
}

check_prerequisites
get_backoffice

if [ "$docker_deployment" = "false" ]; then
    run_haskell_service stern ${blue}
    run_nginx ${green}
else
    docker-compose --file "$DOCKER_FILE" up
fi

sleep 3 # wait a moment for services to start before continuing

echo "All services up & running, Wire away!";

wait
