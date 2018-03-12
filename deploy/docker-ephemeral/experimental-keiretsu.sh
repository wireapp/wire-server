#!/usr/bin/bash

trap "exit" INT TERM ERR
trap "kill 0" EXIT

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../.."

export LOG_LEVEL=Info

function run() {
    service=$1
    (cd ${DIR}/services/${service} && exec dist/${service} -c ${service}.integration.yaml | sed -e "s/^/[${service}] /") &
}

run brig
run galley
run gundeck
run cannon
run proxy
run cargohold


wait
