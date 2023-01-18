# k8ssandra-test-cluster Helm chart

`k8ssandra-test-cluster` provides a `K8ssandraCluster` object to create a
*Cassandra* database with
[`k8ssandra-operator`](https://artifacthub.io/packages/helm/k8ssandra/k8ssandra-operator).
**It does not install `k8ssandra-operator` itself!** This configuration is meant
to be used in test environments: **It lacks crucial parts like backups
(`medusa`)!**

## Usage in Helmfile

### Prerequisites

You need a *storage class* that can automatically request storage volumes. For
Hetzner's cloud see: [Container Storage Interface driver for Hetzner
Cloud](https://github.com/hetznercloud/csi-driver)

### Usage

These entries are used in the `helfile` file:

``` yaml
...

repositories:
  - name: wire
    url: 'https://s3-eu-west-1.amazonaws.com/public.wire.com/charts'
  - name: k8ssandra-stable
    url: https://helm.k8ssandra.io/stable

...

releases:
  - name: k8ssandra-operator
    chart: 'k8ssandra-stable/k8ssandra-operator'
    namespace: databases
    version: 0.39.2
    values:
      # Use a cass-operator image that is compatible to the K8s cluster version
      - cass-operator:
            image:
              tag: v1.10.5

  # Installs CDRs automatically
  - name: k8ssandra-test-cluster
    chart: "wire/k8ssandra-test-cluster"
    namespace: "databases"
    version: {{ .Values.wireChartVersion | quote }}
    needs:
      - 'databases/k8ssandra-operator'
    wait: true
    waitForJobs: true

  - name: 'wire-server'
    namespace: 'wire'
    chart: 'wire/wire-server'
    version: {{ .Values.wireChartVersion | quote }}
    values:
      - './helm_vars/wire-server/values.yaml.gotmpl'
    secrets:
      - './helm_vars/wire-server/secrets.yaml'
    needs:
      - 'databases/k8ssandra-test-cluster'

...
```

Please note the `needs` relations of the releases: `wire-server` *needs*
`k8ssandra-test-cluster` which *needs* `k8ssandra-operator`.

`wait` and `waitForJobs` are mandatory for `k8ssandra-test-cluster` in this
setup: These settings ensure that the database really exists before resuming
with the deployment.

## Implementation details

### k8ssandra-cluster.yaml

Contains the `K8ssandraCluster` object. Its schema is described in the [CRD
reference](https://docs-v2.k8ssandra.io/reference/crd/k8ssandra-operator-crds-latest/#k8ssandracluster)

The specified *Cassandra* cluster runs on a single Node with reasonable
resources for test environments.

### check-cluster-job.yaml

Defines a job that tries to connect to the final *Cassandra* database. Other
deployments can wait on this. This is useful because `wire-server` needs a
working database right from the beginning of it's deployment.
