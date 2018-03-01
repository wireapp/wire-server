#!/usr/bin/env sh

until_ready() {
    until $1; do echo 'service not ready yet'; sleep 5; done
}

until_ready "brig-index reset --elasticsearch-server http://elasticsearch:9200"

until_ready "brig-schema --host cassandra --keyspace brig_test --replication-factor 1"
until_ready "galley-schema --host cassandra --keyspace galley_test --replication-factor 1"
until_ready "gundeck-schema --host cassandra --keyspace gundeck_test --replication-factor 1"
