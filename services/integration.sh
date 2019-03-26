#!/usr/bin/env bash
set -eo pipefail

USAGE="$0 <test-executable> [args...]"
EXE=${1:?$USAGE}
TOP_LEVEL="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
DIR="${TOP_LEVEL}/services"
PARENT_PID=$$
rm -f /tmp/integration.* # remove previous temp files, if any
EXIT_STATUS_LOCATION=$(mktemp "/tmp/integration.XXXXXXXXXXX")
echo 1 >"${EXIT_STATUS_LOCATION}"

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
    pkill "gundeck|brig|galley|cargohold|cannon|spar"
    sleep 1
    kill $(list_descendants "$PARENT_PID") &> /dev/null
}

trap "kill_gracefully; kill_all" INT TERM ERR

function check_prerequisites() {
    if ! ( nc -z 127.0.0.1 9042 \
        && nc -z 127.0.0.1 9200 \
        && nc -z 127.0.0.1 6379 ); then
        echo "Databases not up. Maybe run 'deploy/docker-ephemeral/run.sh' in a separate terminal first?";  exit 1;
    fi
    if   [ ! -f "${TOP_LEVEL}/dist/brig" ] \
      && [ ! -f "${TOP_LEVEL}/dist/galley" ] \
      && [ ! -f "${TOP_LEVEL}/dist/cannon" ] \
      && [ ! -f "${TOP_LEVEL}/dist/gundeck" ] \
      && [ ! -f "${TOP_LEVEL}/dist/cargohold" ] \
      && [ ! -f "${TOP_LEVEL}/dist/spar" ]; then
        echo "Not all services are compiled. How about you run 'cd ${TOP_LEVEL} && make' first?"; exit 1;
    fi
}

blue=6
green=10
orange=3
yellow=11
purpleish=13

if [[ $INTEGRATION_USE_REAL_AWS -eq 1 ]]; then
    echo 'Attempting to run integration tests using real AWS services!'
    [ -z "$AWS_REGION" ] && echo "Need to set AWS_REGION in your environment" && exit 1;
    [ -z "$AWS_ACCESS_KEY_ID" ] && echo "Need to set AWS_ACCESS_KEY_ID in your environment" && exit 1;
    [ -z "$AWS_SECRET_ACCESS_KEY" ] && echo "Need to set AWS_SECRET_ACCESS_KEY in your environment" && exit 1;
    "${TOP_LEVEL}"/services/gen-aws-conf.sh
    integration_file_extension='-aws.yaml'
else
    # brig,gundeck,galley use the amazonka library's 'Discover', which expects AWS credentials
    # even if those are not used/can be dummy values with the fake sqs/ses/etc containers used
    # (see deploy/docker-ephemeral/docker-compose.yaml )
    echo 'Running tests using mocked AWS services'
    export AWS_REGION=eu-west-1
    export AWS_ACCESS_KEY_ID=dummykey
    export AWS_SECRET_ACCESS_KEY=dummysecret
    integration_file_extension='.yaml'
fi

function run() {
    service=$1
    instance=$2
    colour=$3
    ( ( cd "${DIR}/${service}" && "${TOP_LEVEL}/dist/${service}" -c "${service}${instance}.integration${integration_file_extension}" ) || kill_all) \
        | sed -e "s/^/$(tput setaf ${colour})[${service}] /" -e "s/$/$(tput sgr0)/" &
}

check_prerequisites

run brig "" ${green}
run galley "" ${yellow}
run gundeck "" ${blue}
run cannon "" ${orange}
run cannon "2" ${orange}
run cargohold "" ${purpleish}
run spar "" ${orange}

# the ports are copied from ./integration.yaml
while [ "$all_services_are_up" == "" ]; do
    export all_services_are_up="1"
    for port in $(seq 8082 8086) 8088; do
        ( curl --write-out '%{http_code}' --silent --output /dev/null http://localhost:"$port"/i/status \
                | grep -q '^20[04]' ) \
            || export all_services_are_up=""
    done
    sleep 1
done
echo "all services are up!"

( ${EXE} "${@:2}" && echo 0 > "${EXIT_STATUS_LOCATION}" && kill_gracefully ) || kill_gracefully &

wait
exit $(<"${EXIT_STATUS_LOCATION}")
