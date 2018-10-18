#!/usr/bin/env sh

until_ready() {
    until $1; do echo 'service not ready yet'; sleep 5; done
}

until_ready "brig-schema --host cassandra --keyspace brig_test --replication-factor 1"
