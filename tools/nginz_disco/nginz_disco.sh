#!/usr/bin/env bash

set -e
exec 2>&1

# Assumes /etc/wire/nginz/conf and /etc/wire/nginz/upstreams directories exist
# Assumes a file upstreams.txt containing space-separated dns names.

upstream_list="/etc/wire/nginz/conf/upstreams.txt"
old="/etc/wire/nginz/upstreams/upstreams.conf"
new="${old}.new"

function upstream() {
    name=$1
    port=${2:-'8080'}
    ips=$(dig +short +retries=3 +search ${name} | sort)
    unset servers
    for ip in ${ips[@]}; do
        servers+=("\n\t server ${ip}:${port} max_fails=3 weight=100;")
    done;
    if [ -n "$ips" ]; then
        printf "upstream ${name} { \n\t least_conn; \n\t keepalive 32; $(echo ${servers[@]}) \n}\n" >> ${new}
    else
        printf "upstream ${name} { \n\t least_conn; \n\t keepalive 32; \n\t server localhost:${port} down;\n}\n" >> ${new}
    fi
}

function routing_disco() {
    ivl=$(echo | awk '{ srand(); printf("%f", 1.5 + rand() * 1.5) }')

    upstreams=$(cat "$upstream_list")
    upstreams=( $upstreams )

    [[ -f $old ]] || touch -d "1970-01-01" $old

    echo "" > ${new}
    for u in "${upstreams[@]}"; do
        upstream "$u"
    done

    diff -q $old $new || {
      echo new backends found, replacing $old with $new
      mv $new $old
    }

    rm -f $new

    echo done, sleeping $ivl
    sleep $ivl
}

while true; do
    routing_disco
done;
