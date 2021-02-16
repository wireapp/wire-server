# ElasticSearch migration instructions for release 2021-02-15

Release `2021-02-15` of `wire-server` requires creating a new ElasticSearch index for `brig` _before_ deploying the release. Without this new index the user search in TeamSettings will be defunct.

The index that brig is using, is defined in the `brig` chart's config at `elasticsearch.index`. This config value of the previous deployment is referred to as `<OLD_INDEX>` in the following.

These following instructions describe how to:

1. Create the new index `<NEW_INDEX>` (you can choose any name that is different from `<OLD_INDEX>`)
2. Populate `<NEW_INDEX>` with brig's data.
3. Configure brig to use the new index.

Note: The following steps require downtime of brig. If downtime is not acceptable then please skip to section [Create the new index without downtime](#create-the-new-index-without-downtime)

1. Update config for the `elasticsearch-index` chart:
    - Set `elasticsearch.index` to `<NEW_INDEX>`
2. Update config for the `brig` chart:
    - Set `config.elasticsearch.directory` to `<NEW_INDEX>`
3. Redeploy `wire-server`.
   The `elasticsearch-index` chart will automatically create and populate `<NEW_INDEX>`
   before all other services are started. This might take several minutes depending on the
   number of users.
   
You can delete `<OLD_INDEX>` index from ES now.

## Create the new index without downtime

In the following we assume the following env vars are set.

```bash
ES_HOST=<YOUR_HOST>
ES_PORT=<YOUR_PORT> # default is 9200
OLD_INDEX=<OLD_INDEX> # (Same as `elasticsearch.index` previous deployment)
NEW_INDEX=<NEW_INDEX> # new index name, different from OLD 
BRIG_CASSANDRA_HOST=<YOUR_C*_HOST>
BRIG_CASSANDRA_PORT=<YOUR_C*_PORT>
BRIG_CASSANDRA_KEYSPACE=<YOUR_C*_KEYSPACE> # probably "brig"
WIRE_VERSION=<NEW_VERSION_YOU_ARE_DEPLOYING>
SHARDS=<NUMBER_OF_SHARDS_FOR_THE_INDEX> # 5 if you are unsure
REPLICAS=<NUMBER_OF_REPLICAS_FOR_THE_INDEX> # 2 if you are unsure
REFRESH_INTERVAL=<REFRESH_INTERVAL_IN_SECONDS> # 5 if you are unsure
```

1. Create the new index by running

```sh
docker run "quay.io/wire/brig-index:$WIRE_VERSION" create \
    --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
    --elasticsearch-index "$NEW_INDEX" \
    --elastcsearch-shards "$SHARDS" \
    --elastcsearch-replicas "$REPLICAS" \
    --elastcsearch-refresh-interval "$REFRESH_INTERVAL"
    --delete-template "directory"
```

The `--delete-template` option will delete the index template named `directory` if it exists.
This index template might have been created by previous releases and can cause creating a
new index to fail. If this template doesn't exist in your ES cluster you can omit the
`--delete-template` option.

2. Redeploy brig with `elasticsearch.additionalWriteIndex` set to `<NEW_INDEX>`.
3. Make sure no old instances of brig are running.
4. Populate the new index by running
```sh
docker run "quay.io/wire/brig-index:$WIRE_VERSION" migrate-data \
  --elasticsearch-server "http://$ES_HOST:$ES_PORT" \
  --elasticsearch-index "$NEW_INDEX" \
  --cassandra-host "$BRIG_CASSANDRA_HOST" \
  --cassandra-port "$BRIG_CASSANDRA_PORT" \
  --cassandra-keyspace "$BRIG_CASSANDRA_KEYSPACE"
```
This may take sevaral minutes depending on number of users.

5. Redeploy brig with these config updates:
    - unset `elasticsearch.additionalWriteIndex`
    - set `elasticsearch.index` to `<NEW_INDEX>`

6. Update config for the `elasticsearch-index` chart:
    - Set `elasticsearch-index` to `<NEW_INDEX>`
   This prevents the `elasticsearch-index` from automatically recreating the `<OLD_INDEX>`
   when `wire-server`is deployed.

You can delete `<OLD_INDEX>` index from ES now.

## Troubleshooting

### Problem: Brig was deployed without creating the new index beforehand.
Solution: The index can be created after deployment with same instructions as above.

### Problem: `brig-index migrate-data` failed and refuses to migrate again.
Solution: Delete the migration information from Elasticsearch:

  ```bash
  curl -XDELETE http://$ES_HOST:$ES_PORT/wire_brig_migrations
  ```
