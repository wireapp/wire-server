wire-server-metrics
-------------------

 This is mostly a wrapper over https://github.com/helm/charts/tree/master/stable/prometheus-operator
For a full list of overrides, please check the appropriate chart version and its options.

 How to use this chart?
----------------------

 In its simplest form, install the chart with:
```
helm upgrade --install --namespace <namespace> <name> wire/wire-server-metrics [-f <optional-path-to-overrides> ]
```

For more detailed information on how to set up monitoring on your cluster, go to the [monitoring page](https://docs.wire.com/how-to/install/monitoring.html)
