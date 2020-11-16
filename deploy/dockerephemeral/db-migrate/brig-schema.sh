#!/usr/bin/env sh

until_ready() {
    until $1; do echo 'service not ready yet'; sleep 5; done
}

# podman-compose runs with --transform_policy 1podfw by default, which means everything is available on localhost
# docker-compose, on the other hand, names each container and other containers can refer to e.g. http://sns
# This only checks whether some podman process is running. If you use docker with wire-server but podman elsewhere, this will not work.
IN_PODMAN=$(pgrep -f podman)
if [ -n "$IN_PODMAN" ]; then
    CASSANDRA=localhost
else
    CASSANDRA=cassandra
fi

until_ready "brig-schema --host $CASSANDRA --keyspace brig_test --replication-factor 1"
