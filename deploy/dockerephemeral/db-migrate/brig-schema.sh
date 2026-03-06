#!/usr/bin/env sh

until_ready() {
    cmd=$1
    until $cmd; do echo 'service not ready yet'; sleep 5; done
    return 0
}

until_ready "brig-schema --host cassandra --keyspace brig_test --replication-factor 1"
