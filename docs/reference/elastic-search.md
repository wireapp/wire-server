# Maintaining ElasticSearch

## Update mapping

```bash
ES_HOST=<YOUR_HOST>
ES_PORT=<YOUR_PORT> # default is 9200
ES_INDEX=<YOUR_INDEX_NAME> # default is directory
WIRE_VERSION=<VERSION_YOU_ARE_DEPLOYING>

docker run "quay.io/wire/brig-index:$WIRE_VERSION" update-mapping \
  --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
  --elasticsearch-index "$ES_INDEX"
```

Instead of running this in docker, this can also be done by building the `brig-index` binary from `services/brig` and executing it like this:

```bash
brig-index update-mapping \
  --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
  --elasticsearch-index "$ES_INDEX"
```

## Migrate Data

```bash
ES_HOST=<YOUR_HOST>
ES_PORT=<YOUR_PORT> # default is 9200
ES_INDEX=<YOUR_INDEX_NAME> # default is directory
BRIG_CASSANDRA_HOST=<YOUR_C*_HOST>
BRIG_CASSANDRA_PORT=<YOUR_C*_PORT>
BRIG_CASSANDRA_KEYSPACE=<YOUR_C*_KEYSPACE>
WIRE_VERSION=<VERSION_YOU_ARE_DEPLOYING>

docker run "quay.io/wire/brig-index:$WIRE_VERSION" migrate-data \
  --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
  --elasticsearch-index "$ES_INDEX" \
  --cassandra-host "$BRIG_CASSANDRA_HOST" \
  --cassandra-port "$BRIG_CASSANDRA_PORT" \
  --cassandra-keyspace "$BRIG_CASSANDRA_KEYSPACE"
```

(Or, as above, you can also do the same thing without docker.)

## Refill ES documents from Cassandra

This is needed if the information we keep in elastic search increases.
Also update the indices.

```bash
ES_HOST=<YOUR_HOST>
ES_PORT=<YOUR_PORT> # default is 9200
ES_INDEX=<YOUR_INDEX_NAME> # default is directory
BRIG_CASSANDRA_HOST=<YOUR_C*_HOST>
BRIG_CASSANDRA_PORT=<YOUR_C*_PORT>
BRIG_CASSANDRA_KEYSPACE=<YOUR_C*_KEYSPACE>
WIRE_VERSION=<VERSION_YOU_ARE_DEPLOYING>

docker run "quay.io/wire/brig-index:$WIRE_VERSION" reindex \
  --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
  --elasticsearch-index "$ES_INDEX" \
  --cassandra-host "$BRIG_CASSANDRA_HOST" \
  --cassandra-port "$BRIG_CASSANDRA_PORT" \
  --cassandra-keyspace "$BRIG_CASSANDRA_KEYSPACE"
```

Subcommand `reindex-if-same-or-newer` can be used instead of `reindex`, if you want to recreate the documents in elasticsearch regardless of their version.

(Or, as above, you can also do the same thing without docker.)
