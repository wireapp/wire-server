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

## Migrate to a new index

This is needed if we want to migrate to a new index. It could be for updating
analysis settings or to change any other settings on the index which cannot be
done without restarting the index.

This can be done in 4 steps:

Before starting, please set these environment variables
```bash
ES_HOST=<YOUR_HOST>
ES_PORT=<YOUR_PORT> # default is 9200
ES_SRC_INDEX=<INDEX_NAME_ALREADY_IN_USE>
ES_DEST_INDEX=<NEW_INDEX_NAME>
WIRE_VERSION=<VERSION_YOU_HAVE_DEPLOYING>
```

1. Create the new index (please fill out values in `<>` as required)
   ```bash
   docker run "quay.io/wire/brig-index:$WIRE_VERSION" create \
       --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
       --elasticsearch-index "$ES_DEST_INDEX" \
       --elastcsearch-shards <SHARDS> \
       --elastcsearch-replicas <REPLICAS> \
       --elastcsearch-refresh-interval <REFRESH_INTERVAL> \
   ```
1. Redeploy brig with `elasticsearch.additionalWriteIndex` set to the name of new index. Make sure no old brigs are runing.
1. Reindex data to the new index
   ```bash
   docker run "quay.io/wire/brig-index:$WIRE_VERSION" reindex-from-another-index \
       --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
       --source-index "$ES_SRC_INDEX" \
       --destination-index "$ES_DEST_INDEX"
   ```
   Optionally, `--timeout <NUMBER_OF_SECONDS>` can be added to increase/decrease from the default timeout of 10 minutes.
1. Redeploy brig without `elasticsearch.additionalWriteIndex` and with `elasticsearch.index` set to the name of new index

Now you can delete the old index.
