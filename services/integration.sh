#!/usr/bin/env bash
set -eo pipefail

USAGE="$0 <test-executable> [args...]"
EXE=${1:?$USAGE}
TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
DIR="${TOP_LEVEL}/services"
PARENT_PID=$$
rm -f /tmp/integration.* # remove previous temp files, if any
EXIT_STATUS_LOCATION=$(mktemp "/tmp/integration.XXXXXXXXXXX")
echo 1 >${EXIT_STATUS_LOCATION}

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
    pkill "gundeck|brig|galley|cargohold|cannon"
    sleep 1
    kill $(list_descendants $PARENT_PID) &> /dev/null
}

trap "kill_gracefully; kill_all" INT TERM ERR

function check_prerequisites() {
    nc -z 127.0.0.1 9042 \
        && nc -z 127.0.0.1 9200 \
        && nc -z 127.0.0.1 6379 \
        || { echo "Databases not up. Maybe run 'cd deploy/docker-ephemeral && docker-compose up' in a separate terminal first?";  exit 1; }
    test -f ${DIR}/../dist/brig \
        && test -f ${DIR}/../dist/galley \
        && test -f ${DIR}/../dist/cannon \
        && test -f ${DIR}/../dist/gundeck \
        && test -f ${DIR}/../dist/cargohold \
        || { echo "Not all services are compiled. How about you run 'cd ${TOP_LEVEL} && make' first?"; exit 1; }
}

blue=6
white=7
green=10
orange=3
yellow=11
purpleish=13

function run() {
    service=$1
    colour=$2
    export LOG_LEVEL=$3
    (cd ${DIR}/${service} && ${DIR}/../dist/${service} -c ${service}.integration.yaml || kill_all) \
        | sed -e "s/^/$(tput setaf ${colour})[${service}] /" -e "s/$/$(tput sgr0)/" &
}

# brig,gundeck,galley use the amazonka library's 'Discover', which expects AWS credentials
# even if those are not used/can be dummy values with the fake sqs/ses/etc containers used (see deploy/docker-ephemeral/docker-compose.yaml )
# TODO: If we want to use real AWS services, we should ignore these
export AWS_REGION=eu-west-1
# export AWS_ACCESS_KEY_ID=dummy
# export AWS_SECRET_ACCESS_KEY=dummy

check_prerequisites

run brig ${green} Warn
run galley ${yellow} Info
run gundeck ${blue} Info
run cannon ${orange} Info
run cargohold ${purpleish} Info

sleep 3 # wait for services to start before starting integration executable

${EXE} "${@:2}" && echo 0 > ${EXIT_STATUS_LOCATION} && kill_gracefully || kill_gracefully &

wait
exit $(<${EXIT_STATUS_LOCATION})
