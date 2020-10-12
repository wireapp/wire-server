# Rex

Restund-exporer: A service scraping metrics from `restund` and exposing them in a format understood by the `prometheus` monitoring system.

For instance it can show the number of current allocations (ongoing calls):

```
curl -s localhost:9200/metrics | grep allocs_cur
# TYPE allocs_cur gauge
allocs_cur{app="restund",srv="rex",tier="staging"} 15.0
```
