#!/usr/bin/env bash

# You can use this script to simply start all services ./demo.sh

set -eo pipefail

USAGE="$0 [docker] [--run-backoffice]"
docker_deployment="false"
if [ "$1" = "docker" ] || [ "$2" = "docker" ] ; then
    docker_deployment="true"
fi
run_backoffice="false"
if [ "$1" = "--run-backoffice" ] || [ "$2" = "--run-backoffice" ] ; then
    run_backoffice="true"
fi
TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKER_FILE="$SCRIPT_DIR/docker-compose.yaml"
DOCKER_FILE_BACKOFFICE="$SCRIPT_DIR/docker-compose-backoffice.yaml"
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
    pkill "gundeck|brig|galley|cargohold|cannon|spar|stern"
    sleep 1
    kill $(list_descendants $PARENT_PID) &> /dev/null
}

function run_zauth() {
    if [ "$docker_deployment" = "false" ]; then
        ${DIR}/../dist/zauth "$@"
    else
        docker run --entrypoint "/usr/bin/zauth" ${docker_zauth_image:-quay.io/wire/zauth} $@
    fi
}

trap "kill_gracefully; kill_all" INT TERM ERR

function check_secrets() {
    if [ "$docker_deployment" = "false" ]; then
        test -f ${DIR}/../dist/zauth || { echo "zauth is not compiled. How about you run 'cd ${TOP_LEVEL} && make services' first?"; exit 1; }
    fi

    if [[ ! -f ${SCRIPT_DIR}/resources/turn/secret.txt ]]; then
        echo "Generate a secret for the TURN servers (must match the turn.secret key in brig's config)..."
        openssl rand -base64 64 | env LC_CTYPE=C tr -dc a-zA-Z0-9 | head -c 42 > ${SCRIPT_DIR}/resources/turn/secret.txt
    else
        echo "re-using existing TURN secret"
    fi
    if [[ ! -f ${SCRIPT_DIR}/resources/zauth/privkeys.txt || ! -f ${SCRIPT_DIR}/resources/zauth/pubkeys.txt ]]; then
        echo "Generate private and public keys (used both by brig and nginz)..."
        mkdir -p ${SCRIPT_DIR}/resources/zauth/
        TMP_KEYS=$(mktemp "/tmp/demo.keys.XXXXXXXXXXX")
        run_zauth -m gen-keypair -i 1 > $TMP_KEYS
        cat $TMP_KEYS | sed -n 's/public: \(.*\)/\1/p' > ${SCRIPT_DIR}/resources/zauth/pubkeys.txt
        cat $TMP_KEYS | sed -n 's/secret: \(.*\)/\1/p' > ${SCRIPT_DIR}/resources/zauth/privkeys.txt
    else
        echo "re-using existing public/private keys"
    fi
}

function check_prerequisites() {
    nc -z 127.0.0.1 9042 \
        && nc -z 127.0.0.1 9200 \
        && nc -z 127.0.0.1 6379 \
        || { echo "Databases not up. Maybe run 'deploy/dockerephemeral/run.sh' in a separate terminal first?";  exit 1; }
    if [ "$docker_deployment" = "false" ]; then
        test -f ${DIR}/../dist/brig \
            && test -f ${DIR}/../dist/galley \
            && test -f ${DIR}/../dist/cannon \
            && test -f ${DIR}/../dist/gundeck \
            && test -f ${DIR}/../dist/cargohold \
            && test -f ${DIR}/../dist/proxy \
            && test -f ${DIR}/../dist/spar \
            && test -f ${DIR}/../dist/stern \
            && ( test -f ${DIR}/../dist/nginx || which nix-build ) \
            || { echo "Not all services are compiled. How about you run 'cd ${TOP_LEVEL} && make services' first?"; exit 1; }
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

function run_nginz() {
    colour=$1
    prefix=$([ -w /usr/local ] && echo /usr/local || echo "${HOME}/.wire-dev")

    # For nix we dont need LD_LIBRARY_PATH; we link against libzauth directly.
    # nix-build will put a symlink to ./result with the nginx artifact
    if which nix-build; then
      nginz=$(nix-build "${DIR}/../nix" -A nginz --no-out-link )
      (cd ${SCRIPT_DIR} && ${nginz}/bin/nginx -p ${SCRIPT_DIR} -c ${SCRIPT_DIR}/conf/nginz/nginx.conf -g 'daemon off;' || kill_all) \
          | sed -e "s/^/$(tput setaf ${colour})[nginz] /" -e "s/$/$(tput sgr0)/" &
    else
      prefix=$([ -w /usr/local ] && echo /usr/local || echo "${HOME}/.wire-dev")
      (cd ${SCRIPT_DIR} && LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${prefix}/lib/ ${DIR}/../dist/nginx -p ${SCRIPT_DIR} -c ${SCRIPT_DIR}/conf/nginz/nginx.conf -g 'daemon off;' || kill_all) \
          | sed -e "s/^/$(tput setaf ${colour})[nginz] /" -e "s/$/$(tput sgr0)/" &
    fi
}

function copy_brig_templates() {
    # Need to copy over the templates from Brig since symlinking does not
    # work with Docker
    mkdir -p "${SCRIPT_DIR}/resources/templates"
    cp -r "${SCRIPT_DIR}/../../services/brig/deb/opt/brig/templates/"* "${SCRIPT_DIR}/resources/templates/"
}

function copy_nginz_configs() {
    # Need to copy over the configs from Nginz since symlinking does not
    # work with Docker
    # ensure swagger UI files are downloaded befory copying
    make -C "${TOP_LEVEL}/services/nginz" zwagger-ui/swagger-ui
    mkdir -p "${SCRIPT_DIR}/conf/nginz/zwagger-ui"
    cp -r "${SCRIPT_DIR}/../../services/nginz/zwagger-ui/"* "${SCRIPT_DIR}/conf/nginz/zwagger-ui/"
}

# brig,gundeck,galley use the amazonka library's 'Discover', which expects AWS credentials
# even if those are not used/can be dummy values with the fake sqs/ses/etc containers used (see deploy/dockerephemeral/docker-compose.yaml)
export AWS_REGION=${AWS_REGION:-eu-west-1}
export AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID:-dummy}
export AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY:-dummy}

check_secrets
check_prerequisites
copy_brig_templates
copy_nginz_configs

if [ "$docker_deployment" = "false" ]; then
    run_haskell_service brig ${green}
    run_haskell_service galley ${yellow}
    run_haskell_service gundeck ${blue}
    run_haskell_service cannon ${orange}
    run_haskell_service cargohold ${purpleish}
    run_haskell_service proxy ${redish}
    run_haskell_service spar ${orange}
    if [ "$run_backoffice" = "true" ]; then
        run_haskell_service stern ${orange}
    fi
    run_nginz ${blueish}
else
    if [ "$run_backoffice" = "true" ]; then
        docker-compose --file "$DOCKER_FILE" --file "$DOCKER_FILE_BACKOFFICE" up
    else
        docker-compose --file "$DOCKER_FILE" up
    fi
fi

sleep 3 # wait a moment for services to start before continuing

echo "All services up & running, Wire away!";

wait
