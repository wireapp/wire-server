# ElasticSearch migration instructions for release 2021-02-16

Release `2021-02-16` of `wire-server` requires an update of the ElasticSearch index of `brig`.
During the update the team member search in TeamSettings will be defunct.

The update is triggered automatically on upgrade by the `elasticsearch-index-create` and `brig-index-migrate-data` jobs. If these jobs finish sucessfully the update is complete.

## Troubleshooting

In case the `elasticsearch-index-create` job fails this document describes how to create a new index.

The index that brig is using is defined at `brig.config.elasticsearch.index` of the `wire-server` chart. We will refer to its current setting as `<OLD_INDEX>`.

1. Choose a new index name that is different from `<OLD_INDEX>`.
   We will refer to this name as `<NEW_INDEX>`.
2. Upgrade the release with these config changes:
    - Set `brig.config.elasticsearch.additionalWriteIndex` to `<NEW_INDEX>`
    - Set `elasticsearch-index.elasticsearch.additionalWriteIndex` to `<NEW_INDEX>`
   and wait for completion.
3. Upgrade the release again with these config changes:
    - Unset `brig.config.elasticsearch.additionalWriteIndex`
    - Unset `elasticsearch-index.elasticsearch.additionalWriteIndex`
    - Set `brig.config.elasticsearch.index` to `<NEW_INDEX>`
    - Set `elasticsearch-index.elasticsearch.index` to `<NEW_INDEX>`
