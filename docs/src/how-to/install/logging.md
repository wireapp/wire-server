(logging)=

# Installing centralized logging dashboards using Kibana

## Introduction

This page shows you how to install Elasticsearch, Kibana, and fluent-bit to aggregate and visualize the logs from wire-server components.

## Status

Logging support is in active development as of September 2019, some logs may not be visible yet, and certain parts are not fully automated yet.

## Prerequisites

You need to have wire-server installed, see either of

- {ref}`helm`
- {ref}`helm-prod`.

## Installing required helm charts

### Deploying Elasticsearch

Elasticsearch indexes the logs and makes them searchable. The following elasticsearch-ephemeral chart makes use of the disk space the pod happens to run on.

```
$ helm install --namespace <namespace> wire/elasticsearch-ephemeral
```

Note that since we are not specifying a release name during helm
install, it generates a 'verb-noun' pair, and uses it. Elasticsearch's
chart does not use the release name of the helm chart in the pod name,
sadly.

### Deploying Elasticsearch-Curator

Elasticsearch-curator trims the logs that are contained in elasticsearch, so
that your elasticsearch pod does not get too large, crash, and need to be
re-built.

```
$ helm install --namespace <namespace> wire/elasticsearch-curator
```

Note that since we are not specifying a release name during helm
install, it generates a 'verb-noun' pair, and uses it. If you look at
your pod names, you can see this name prepended to your pods in 'kubectl
-n get pods'.

### Deploying Kibana

```
$ helm install --namespace <namespace> wire/kibana
```

Note that since we are not specifying a release name during helm
install, it generates a 'verb-noun' pair, and uses it. If you look at
your pod names, you can see this name prepended to your pods in 'kubectl
-n get pods'.

### Deploying fluent-bit

```
$ helm install --namespace <namespace> wire/fluent-bit
```

## Configuring fluent-bit

```{note}
The following makes use of overrides for helm charts. You may wish to read {ref}`understand-helm-overrides` first.
```

Per pod-template, you can specify what parsers `fluent-bit` needs to
use to interpret the pod's logs in a structured way. By default, it just
parses them as plain text. But, you can change this using a pod
annotation. E.g.:

```
apiVersion: v1
kind: Pod
metadata:
  name: brig
  labels:
    app: brig
  annotations:
    fluentbit.io/parser: json
spec:
  containers:
  - name: apache
    image: edsiper/apache_logs
```

You can also define your own custom parsers in our `fluent-bit`
chart's `values.yml`. For example, we have one defined for `nginz`.
For more info, see :
<https://github.com/fluent/fluent-bit-docs/blob/master/filter/kubernetes.md#kubernetes-annotations>

Alternately, if there is already fluent-bit deployed in your
environment, get the helm name for the deployment (verb-noun prepended
to the pod name), and

```
$ helm upgrade <helm-name> --namespace <namespace> wire/fluent-bit
```

Note that since we are not specifying a release name during helm
install, it generates a 'verb-noun' pair, and uses it. if you look at
your pod names, you can see this name prepended to your pods in 'kubectl
-n get pods'.

(post-install-kibana-setup)=

## Post-install kibana setup

Get the pod name for your kibana instance (not the one set up with
fluent-bit), and

```
$ kubectl -n <namespace> port-forward <pod_name> 5601:5601
```

go to 127.0.0.1:5601 in your web browser.

1. Click on 'discover'.
2. Use `kubernetes_cluster-*` as the Index pattern.
3. Click on 'Next step'
4. Click on the 'Time Filter field name' dropdown, and select
   <mailto:'@timestamp>'.
5. Click on 'create index patern'.

## Usage after installation

Get the pod name for your kibana instance (not the one set up with
fluent-bit), and

```
$ kubectl -n <namespace> port-forward <pod_name> 5601:5601
```

Go to 127.0.0.1:5601 in your web browser.

Click on 'discover' to view data.

(nuking-it-all)=

## Nuking it all.

Find the names of the helm releases for your pods (look at `helm ls --all`
and `kubectl -n <namespace> get pods` , and run
`helm del <helm_deploy_name> --purge` for each of them.

Note: Elasticsearch does not use the name of the helm chart, and
therefore is harder to identify.

## Debugging

```
kubectl -n <namespace> logs <host>
```

### How this was developed

First, we deployed elasticsearch with the elasticsearch-ephemeral chart,
then kibana. then we deployed fluent-bit, which set up a kibana of it's
own that looks broken. It had a kibana .tgz in an incorrect location. It
also set up way more VMs than I thought, AND consumed the logs for the
entire cluster, Rather than for the namespace it's contained in, as I
expected.

For kibana and fluent-bit, we created a shell of overrides, with a
dependency on the actual chart, so that when we helm dep update, helm
grabs the chart from upstream, instead of bringing the source of the
chart into our repository. There were only three files to modify, which
we copied from the fake-aws-s3 chart and modified: Chart.yaml,
requirements.yaml, and values.yaml.

For elasticsearch, we bumped the version number, because kibana was
refusing to start, citing too old of a version of elasticsearch. it
wants a 6.x, we use 5.x for brig, and for our kibana/logserver setup.
later, we forced integration tests against the new elasticsearch in
confluence.
