(monitoring)=

# Monitoring wire-server using Prometheus and Grafana

All wire-server helm charts offering prometheus metrics expose a
`metrics.serviceMonitor.enabled` option.

If these are set to true, the helm charts will install `ServiceMonitor`
resources, which can be used to mark services for scraping by
\[Prometheus Operator\](<https://prometheus-operator.dev/>),
\[Grafana Agent Operator\](<https://grafana.com/docs/grafana-cloud/kubernetes-monitoring/agent-k8s/>),
or similar prometheus-compatible tools.

Refer to their documentation for installation.

## Dashboards

Grafana dashboard configurations are included as JSON inside the `dashboards`
directory. You may import these via Grafana's web UI.
