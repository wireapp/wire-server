#!/usr/bin/env bash
set -eo pipefail

USAGE="$0 <test-executable> [args...]"
EXE=${1:?$USAGE}
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PID=$$

function kill_all() {
    kill -9 -$(ps -o pgid= $PID | grep -o '[0-9]*')
}

function stop_nicely() {
    trap "kill_all" INT EXIT TERM ERR
    kill -2 $(pgrep -f integration.yaml) &> /dev/null
    sleep 1
}

trap "stop_nicely" INT EXIT TERM ERR

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
    (cd ${DIR}/${service} && exec dist/${service} -c ${service}.integration.yaml \
        | sed -e "s/^/$(tput setaf ${colour})[${service}] /" -e "s/$/$(tput sgr0)/" || stop_nicely) &
}

# brig,gundeck,galley use the amazonka library's 'Discover', which expects AWS credentials
# even if those are not used/can be dummy values with the fake sqs/ses/etc containers used (see deploy/docker-ephemeral/docker-compose.yaml )
export AWS_REGION=eu-west-1
export AWS_ACCESS_KEY_ID=dummy
export AWS_SECRET_ACCESS_KEY=dummy

run brig ${green} Warn
run galley ${yellow} Info
run gundeck ${blue} Info
run cannon ${orange} Info
#run cargohold ${purpleish} Info

sleep 3

(${EXE} "${@:2}" && stop_nicely || stop_nicely) &

wait
