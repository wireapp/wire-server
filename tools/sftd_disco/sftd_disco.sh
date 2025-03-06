#!/usr/bin/env bash

set -eo pipefail
exec 2>&1

# Assumes /etc/wire/sftd-disco/ directory exists.

USAGE="example usage: $0 _sft._tcp.wire-server-sftd.wire.svc.cluster.local"
srv_name=${1?$USAGE}

old="/etc/wire/sftd-disco/sft_servers_all.json"
new="${old}.new"

function valid_entry() {
    # TODO sanity check that this is real dig output
    return 0
}

function valid_url() {
    #TODO basic sanity check
    return 0
}

# for a given SRV record
# 1. lookup the record
# 2. for each entry: extract host and port and call 'curl host:port/sft/url'
# 4. save the resulting URLs as a json array to a file
# this file can then be served from nginx running besides sft
function upstream() {
    name=$1
    entries=$(dig +short +retries=3 +search SRV "${name}" | sort)
    unset servers
    comma=""
    IFS=$'\t\n'
    for entry in $entries; do
        if valid_entry "$entry"; then
            sft_host_port=$(echo "$entry" | awk '{print $4":"$3}')
            sft_url=$(curl -s http://"$sft_host_port"/sft/url | xargs)
            if valid_url "$sft_url"; then
                servers+=("$comma"'"'"$sft_url"'"')
                comma=","
            fi
        fi
    done
    # shellcheck disable=SC2128
    if [ -n "$servers" ]; then
        echo '{"sft_servers_all": ['"${servers[*]}"']}' | jq >${new}
    else
        printf "" >>${new}
    fi
}

function routing_disco() {
    srv_name=$1
    ivl=$(echo | awk '{ srand(); printf("%f", 2.5 + rand() * 1.5) }')

    [[ -f $old ]] || touch -d "1970-01-01" $old

    echo "" >${new}
    upstream "$srv_name"

    diff -q $old $new || {
        echo upstream change found, replacing $old with $new
        mv $new $old
    }

    rm -f $new

    echo done, sleeping "$ivl"
    sleep "$ivl"
}

while true; do
    routing_disco "$srv_name"
done
