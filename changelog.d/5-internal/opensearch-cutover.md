ElasticSearch has been replaced by OpenSearch 1.3.20 as the search backend in
all development, CI, and Helm chart deployments (WPB-12109):

- `elasticsearch-ephemeral`, `elasticsearch-index`, `elasticsearch-external`,
  and `elasticsearch-curator` charts are renamed to `opensearch-*` and run
  `opensearchproject/opensearch:1.3.20`.
- The `kibana` chart is replaced by `opensearch-dashboards`
  (`opensearchproject/opensearch-dashboards:1`).
- The dockerephemeral compose setup drops the `elasticsearch:6.8.23` service;
  OpenSearch is now the only search service, exposed on `localhost:9200`.
- Brig's `elasticsearch:` configuration keys are unchanged; existing
  Elasticsearch clients remain source-compatible since OpenSearch 1.x exposes
  the same untyped REST API surface used by the bloodhound client.

## Migration instructions

For operators upgrading an existing deployment (no OpenSearch upgrade is
required if you already run OpenSearch; the chart renames are the breaking
part):

1. Update chart dependencies: all references to `elasticsearch-ephemeral`,
   `elasticsearch-index`, `elasticsearch-external`, and `elasticsearch-curator`
   in your deployment values and umbrella charts must be renamed to
   `opensearch-ephemeral`, `opensearch-index`, `opensearch-external`, and
   `opensearch-curator` (both the dependency names and the values keys).
   Replace the `kibana` chart with `opensearch-dashboards`; note its values
   changed from the elastic `kibana:` wrapper values to a native values file
   (`opensearchHosts` replaces `kibana.elasticsearchHosts`). The
   `kibana-basic-auth` secret behavior is unchanged.
2. Run `helm dependency update` for `wire-server` and `databases-ephemeral`
   and re-render; the subcharts are vendored from `file://../opensearch-*`.
3. Migrate data into the new-index layout if you are coming from
   Elasticsearch 6.8 or earlier: deploy `opensearch-index` and run the
   `brig-index-migrate-data` job (or `brig-index migrate-data` directly) after
   creating the index with the `opensearch-index-create` job. Configure Brig
   dual-writing during the transition with the existing
   `elasticsearch.additionalWriteIndex*` options, then cut over and drop the
   old endpoint.
4. Point Brig at the OpenSearch endpoint
   (`brig.config.elasticsearch.url`, e.g. `https://opensearch:9200`).
   Keep the `elasticsearch:` config key spelling — it is unchanged.
5. Kibana index patterns (`pod-*`, `node-*`) are no longer auto-imported via a
   postStart hook: OpenSearch Dashboards 1.x removed the
   `/api/kibana/dashboards/import` endpoint. Import the patterns manually via
   the UI or `POST /api/saved_objects/_import`.
6. For the ephemeral chart, the security plugin is disabled when
   `tls.enabled=false`; with TLS enabled, OpenSearch credentials come from
   OpenSearch security configuration (default `admin`/`admin` in 1.x, or a
   mounted securityconfig), not from the removed `secrets.password` value. ##
