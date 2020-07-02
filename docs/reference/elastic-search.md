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
done without restarting the index. Analysis settings can also be updated by
recreating the index. Recreating the index is simpler to do, but requires
downtime, the process is documented [below](#recreate-an-index-requires-downtime)

This can be done in 4 steps:

Before starting, please set these environment variables
```bash
ES_HOST=<YOUR_HOST>
ES_PORT=<YOUR_PORT> # default is 9200
ES_SRC_INDEX=<INDEX_NAME_ALREADY_IN_USE>
ES_DEST_INDEX=<NEW_INDEX_NAME>
WIRE_VERSION=<VERSION_YOU_ARE_DEPLOYING>
SHARDS=<NUMBER_OF_SHARDS_FOR_THE_INDEX>
REPLICAS=<NUMBER_OF_REPLICAS_FOR_THE_INDEX>
REFRESH_INTERVAL=<REFRESH_INTERVAL_IN_SECONDS>
```

1. Create the new index (please fill out values in `<>` as required)
   ```bash
   docker run "quay.io/wire/brig-index:$WIRE_VERSION" create \
       --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
       --elasticsearch-index "$ES_DEST_INDEX" \
       --elasticsearch-shards "$SHARDS" \
       --elasticsearch-replicas "$REPLICAS" \
       --elasticsearch-refresh-interval "$REFRESH_INTERVAL"
   ```
1. Redeploy brig with `elasticsearch.additionalWriteIndex` set to the name of new index. Make sure no old brigs are running.
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

**NOTE**: There is a bug hidden when using this way. Sometimes a user won't get
deleted from the index. Attempts at reproducing this issue in a simpler
environment have failed. As a workaround, there is a tool in
[tools/db/find-undead](../../tools/db/find-undead) which can be used to find the
undead users right after the migration. If they exist, please run refill the ES
documents from cassandra as described [above](#refill-es-documents-from-cassandra)

## Recreate an index (Requires downtime)

When analysis settings of an index need to be changed, e.g. for changes
introduced in [#1052](https://github.com/wireapp/wire-server/pull/1052),
it is not possible to keep the index running while the changes are applied.

To tackle this, a wire-server operator must either migrate to a new index as
documented [above](#migrate-to-a-new-index) or allow for some downtime. One
might want to choose downtime for simplicity. These steps are especially simple
to do when using [wire-server-deploy](https://github.com/wireapp/wire-server-deploy/).

Here are the steps:

Before starting, please set these environment variables
```bash
ES_HOST=<YOUR_HOST>
ES_PORT=<YOUR_PORT> # default is 9200
ES_INDEX=<INDEX_NAME_ALREADY_IN_USE>
```

### Step 1: Delete the old index
```bash
curl -XDELETE http://$ES_HOST:$ES_PORT/$ES_INDEX
curl -XDELETE http://$ES_HOST:$ES_PORT/wire_brig_migrations
```

### Step 2: Recreate the index

#### When using helm charts from [wire-server-deploy](https://github.com/wireapp/wire-server-deploy)

Just redeploy the helm chart, new index will be created and after the deployment
data migrations will refill the index with users.


#### When not using helm charts from [wire-server-deploy](https://github.com/wireapp/wire-server-deploy)

Set these extra environment variables:
```bash
WIRE_VERSION=<VERSION_YOU_ARE_DEPLOYING>
SHARDS=<NUMBER_OF_SHARDS_FOR_THE_INDEX>
REPLICAS=<NUMBER_OF_REPLICAS_FOR_THE_INDEX>
REFRESH_INTERVAL=<REFRESH_INTERVAL_FOR_THE_INDEX>
```
1. Create the index
   ```bash
   docker run "quay.io/wire/brig-index:$WIRE_VERSION" create \
       --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
       --elasticsearch-index "$ES_INDEX" \
       --elastcsearch-shards "$SHARDS" \
       --elastcsearch-replicas "$REPLICAS" \
       --elastcsearch-refresh-interval "$REFRESH_INTERVAL"
   ```
1. Refill the index as documented [above](#refill-es-documents-from-cassandra)
