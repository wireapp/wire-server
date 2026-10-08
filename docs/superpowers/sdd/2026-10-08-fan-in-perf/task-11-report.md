# Task 11 report
Status: DONE
- compose: extra_hosts on otel-collector; fan-in-perf.json mounted in grafana-lgtm next to postgres-exporter.json.
- otel-collector-config: scrape job fan-in-perf -> host.docker.internal:9400, 5s.
- dashboard: brief JSON used verbatim; all metric names verified against Metrics.hs (no changes needed). wire_hasql_pool_* from libs/extended.
- Validation: jq parses dashboard; yq parses both YAMLs (python blocked by hook).
- Concerns: OTLP round-trip may rename _total / job->service_name; check in Explore (Task 12). User: restart otel-collector + grafana-lgtm, allow docker bridge -> host:9400.
