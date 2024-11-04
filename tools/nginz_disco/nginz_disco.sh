#!/usr/bin/env bash

set -e
exec 2>&1

# Assumes /etc/wire/nginz/conf and /etc/wire/nginz/upstreams directories exist
# Assumes a file upstreams.txt containing space-separated dns names.

upstream_list="/etc/wire/nginz/conf/upstreams.txt"
old="/etc/wire/nginz/upstreams/upstreams.conf"
new="${old}.new"

# The following bash-based regex parsers for ipv4/6 are not perfect; they are designed to avoid writing garbage to the upstreams.txt file whenever 'dig' returns some textual output (some error) instead of a list of IP addresses.
# If the need arises, this could be improved by Option A) improving the bash parser or (preferred: Option B) rewriting this in Haskell.
function valid_ipv4() {
    local  ip=$1
    local  stat=1

    if [[ $ip =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]; then
        OIFS=$IFS
        IFS='.'
        # shellcheck disable=SC2206
        ip=($ip)
        IFS=$OIFS
        [[ ${ip[0]} -le 255 && ${ip[1]} -le 255 \
            && ${ip[2]} -le 255 && ${ip[3]} -le 255 ]]
        stat=$?
    fi
    return $stat
}

function valid_ipv6() {
    regex='^([0-9a-fA-F]{0,4}:){1,7}[0-9a-fA-F]{0,4}$'
    input="$1"

    [[ $input =~ $regex ]]
    stat=$?
    return $stat
}

function upstream() {
    name=$1
    port=${2:-'8080'}
    ips=$(dig +short +retries=3 +search "${name}" | sort)
    unset servers
    IFS=$' \t\n'
    for ip in $ips; do
        if valid_ipv4 "$ip" || valid_ipv6 "$ip"; then
            servers+=("\n\t server ${ip}:${port} max_fails=3 weight=100;")
        fi
    done;
    # shellcheck disable=SC2128
    if [ -n "$servers" ]; then
        # shellcheck disable=SC2068,SC2116,SC2059
        printf "upstream ${name} { \n\t least_conn; \n\t keepalive 32; $(echo ${servers[@]}) \n}\n" >> ${new}
    else
        # shellcheck disable=SC2059
        printf "upstream ${name} { \n\t least_conn; \n\t keepalive 32; \n\t server localhost:${port} down;\n}\n" >> ${new}
    fi
}

function routing_disco() {
    ivl=$(echo | awk '{ srand(); printf("%f", 1.5 + rand() * 1.5) }')

    upstreams=$(cat "$upstream_list")
    # shellcheck disable=SC2206
    upstreams=( $upstreams )

    [[ -f $old ]] || touch -d "1970-01-01" $old

    echo "" > ${new}
    for u in "${upstreams[@]}"; do
        upstream "$u"
    done

    diff -q $old $new || {
      echo upstream change found, replacing $old with $new
      mv $new $old
    }

    rm -f $new

    echo done, sleeping "$ivl"
    sleep "$ivl"
}

while true; do
    routing_disco
done;
