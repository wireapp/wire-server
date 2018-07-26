#!/usr/bin/env bash

# You can use this script to simply start all services ./demo.sh

set -eo pipefail

USAGE="$0 [test-executable args...]"
EXE=$1
TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIR="${TOP_LEVEL}/services"
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
    pkill "gundeck|brig|galley|cargohold|cannon|spar"
    sleep 1
    kill $(list_descendants $PARENT_PID) &> /dev/null
}

trap "kill_gracefully; kill_all" INT TERM ERR

function check_secrets() {
    test -f ${DIR}/../dist/zauth || { echo "zauth is not compiled. How about you run 'cd ${TOP_LEVEL} && make services' first?"; exit 1; }
    
    if [[ ! -f ${SCRIPT_DIR}/resources/turn/secret.txt ]]; then
        echo "Generate a secret for the TURN servers (must match the turn.secret key in brig's config)..."
        openssl rand -base64 64 | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 42 > ${SCRIPT_DIR}/resources/turn/secret.txt
    else
        echo "re-using existing TURN secret"
    fi
    if [[ ! -f ${SCRIPT_DIR}/resources/zauth/privkeys.txt || ! -f ${SCRIPT_DIR}/resources/zauth/pubkeys.txt ]]; then
        echo "Generate private and public keys (used both by brig and nginz)..."
        TMP_KEYS=$(mktemp "/tmp/demo.keys.XXXXXXXXXXX")
        ${DIR}/../dist/zauth -m gen-keypair -i 1 > $TMP_KEYS
        cat $TMP_KEYS | sed -n 's/public: \(.*\)/\1/p' > ${SCRIPT_DIR}/resources/zauth/pubkeys.txt
        cat $TMP_KEYS | sed -n 's/secret: \(.*\)/\1/p' > ${SCRIPT_DIR}/resources/zauth/privkeys.txt
    else
        echo "re-using existing public/private keys"
    fi

    if [[ ! -f ${SCRIPT_DIR}/conf/restund/restund.conf ]]; then
        echo "updating restund config..."
        cp ${SCRIPT_DIR}/conf/restund/restund.conf.example ${SCRIPT_DIR}/conf/restund/restund.conf
        TURN_SECRET=$(cat ${SCRIPT_DIR}/resources/turn/secret.txt)
        sed -n -i "s/restund_zrest_secret/${TURN_SECRET}/p" ${SCRIPT_DIR}/conf/restund/restund.conf
    else
        echo "re-using existing restund config file"
    fi
    if [[ ! -f ${SCRIPT_DIR}/conf/restund/restund.pem ]]; then
        echo "Generate a self-signed certificate for restund..."
        openssl req -x509 -nodes -newkey rsa:4096 -keyout ${SCRIPT_DIR}/conf/restund/key.pem -out ${SCRIPT_DIR}/conf/restund/cert.pem -days 365 -subj '/CN=localhost'
        mv ${SCRIPT_DIR}/conf/restund/cert.pem ${SCRIPT_DIR}/conf/restund/restund.pem
        cat ${SCRIPT_DIR}/conf/restund/key.pem >> ${SCRIPT_DIR}/conf/restund/restund.pem
        rm -f ${SCRIPT_DIR}/conf/restund/key.pem
    else
        echo "re-using existing restund pem file"
    fi
}

function check_prerequisites() {
    nc -z 127.0.0.1 9042 \
        && nc -z 127.0.0.1 9200 \
        && nc -z 127.0.0.1 6379 \
        || { echo "Databases not up. Maybe run 'deploy/docker-ephemeral/run.sh' in a separate terminal first?";  exit 1; }
    test -f ${DIR}/../dist/brig \
        && test -f ${DIR}/../dist/galley \
        && test -f ${DIR}/../dist/cannon \
        && test -f ${DIR}/../dist/gundeck \
        && test -f ${DIR}/../dist/cargohold \
        && test -f ${DIR}/../dist/proxy \
        && test -f ${DIR}/../dist/spar \
        && test -f ${DIR}/../dist/nginx \
        || { echo "Not all services are compiled. How about you run 'cd ${TOP_LEVEL} && make services' first?"; exit 1; }
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
    export LOG_LEVEL=$3
    (cd ${SCRIPT_DIR} && ${DIR}/../dist/${service} -c ${SCRIPT_DIR}/conf/${service}.demo.yaml || kill_all) \
        | sed -e "s/^/$(tput setaf ${colour})[${service}] /" -e "s/$/$(tput sgr0)/" &
}

function run_nginz() {
    colour=$1
    prefix=$([ -w /usr/local ] && echo /usr/local || echo "${HOME}/.wire-dev")
    (cd ${SCRIPT_DIR} && LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${prefix}/lib/ ${DIR}/../dist/nginx -p ${SCRIPT_DIR} -c ${SCRIPT_DIR}/conf/nginz/nginx.conf -g 'daemon off;' || kill_all) \
        | sed -e "s/^/$(tput setaf ${colour})[nginz] /" -e "s/$/$(tput sgr0)/" &
}

# brig,gundeck,galley use the amazonka library's 'Discover', which expects AWS credentials
# even if those are not used/can be dummy values with the fake sqs/ses/etc containers used (see deploy/docker-ephemeral/docker-compose.yaml)
export AWS_REGION=${AWS_REGION:-eu-west-1}
export AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID:-dummy}
export AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY:-dummy}

check_secrets
check_prerequisites

run_haskell_service brig ${green} Debug
run_haskell_service galley ${yellow} Info
run_haskell_service gundeck ${blue} Info
run_haskell_service cannon ${orange} Info
run_haskell_service cargohold ${purpleish} Info
run_haskell_service proxy ${redish} Info
run_haskell_service spar ${orange} Info
run_nginz ${blueish}

sleep 3 # wait a moment for services to start before continuing

echo "All services up & running, Wire away!";

wait
