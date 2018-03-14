#!/usr/bin/env bash

set -eo pipefail

USAGE="$0 <test-executable>"

EXE=${1:?$USAGE}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

PID=$$

function kill_all() {
    kill -9 -$(ps -o pgid= $PID | grep -o '[0-9]*')
}

function stop_nicely() {
    trap "kill_all" INT
    kill -2 $(pgrep -f integration.yaml) $(pgrep -f ${EXE}) &> /dev/null
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


run brig ${green} Warn
run galley ${yellow} Info
run gundeck ${blue} Info
run cannon ${orange} Info
run cargohold ${purpleish} Info

sleep 3

(${EXE} "${@:2}" && stop_nicely || stop_nicely) &

wait
