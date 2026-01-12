#!/usr/bin/env sh

until_ready() {
    until $1; do echo 'service not ready yet'; sleep 5; done
}

# Uses the brig.yaml configuration file for elasticsearch connection settings
until_ready "brig-index -c /etc/wire/brig/conf/brig.yaml reset --elasticsearch-index-prefix directory"
