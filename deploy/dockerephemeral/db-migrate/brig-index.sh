#!/usr/bin/env sh

until_ready() {
    cmd=$1
    until $cmd; do echo 'service not ready yet'; sleep 5; done
    return 0
}

until_ready "brig-index reset --elasticsearch-server https://localhost:9200 --elasticsearch-insecure-skip-tls-verify"
