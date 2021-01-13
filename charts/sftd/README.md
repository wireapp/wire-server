# SFTD Chart


## Deploy

Replace `example.com` with your own domain here.

Using your own certificates:

```
helm install sftd wire/sftd  \
  --set host=sftd.example.com \
  --set allowOrigin=https://webapp.example.com \
  --set-file tls.crt=/path/to/tls.crt \
  --set-file tls.key=/path/to/tls.key
```

Using Cert-manager:
```
helm install sftd wire/sftd \
  --set host=sftd.example.com \
  --set allowOrigin=https://webapp.example.com \
  --set tls.issuerRef.name=letsencrypt-staging
```

the `host` option will be used to set up an `Ingress` object.

The domain in `host` must point to the public IP you have deployed to handle
incoming traffic to your cluster. This is environment-specific.

You can switch between `cert-manager` and own-provided certificates at any
time. Helm will delete the `sftd` secret automatically and then cert-manager
will create it instead.

It is important that `allowOrigin` is synced with the domain where the web app is hosted
as configured in the `wire-server` chart or the webapp will not be able to contact the SFT
server.

You should configure `brig` to hand out the SFT server to clients by setting
`brig.optSettings.setSftStaticUrl=https://sftd.example.com:443` on the `wire-server` chart

## Parameters

Please see [values.yaml](./values.yaml) for an overview of parameters that can be configured.


## Rollout

Kubernetes will shut down pods and start new ones when rolling out a release. Any calls
that were in progress on said pod will be terminated and will cause the call to drop.

Kubernetes can be configured to wait for a certain amount of seconds before
stopping the pod. During this timeframe new calls wil not be initiated on the
pod, but existing calls will also not be disrupted.  If you want to roll out a
release with minimal impact you can set the
[`terminationGracePeriodSeconds`](./values.yaml#L18) option to the maximum
length you want to wait before cutting off calls.

Currently due to the fact we're using a `StatefulSet` to orchestrate update
rollouts, and `StatefulSet`s will not replace all pods at once but instead
one-for-one, a rollout of a release will take `oldReplicas * terminationGracePeriodSeconds`
to complete.

We might switch to using a `Deployment` for `sftd` in the future, to reduce this time to just `terminationGracePeriodSeconds`.



## Multiple sftd deployments in a single cluster
Because sftd uses the `hostNetwork` and binds to the public IP of the node,
there can only be one `sftd` pod running per node in the cluster.  Within a
single `StatefulSet` kubernetes will make sure no two pods are scheduled on the
same machine automatically. However, if you have multiple `sftd` deployments under
different releases names or in a different namespace more care has to be taken.

You can set the `nodeSelector` option; to make sure your sftd releases run on disjoint sets of nodes.

For example, consider the following inventory of nodes, where there are two groups
annotated with

```
[sftd-prod:vars]
node_labels="wire.com/role=sftd-prod"
[sftd-staging:vars]
node_labels="wire.com/role=sftd-staging"

[sftd-prod]
node0
node1
node3

[sftd-staging]
node4
```

Then we can make two `sftd` deployments and make sure Kubernetes schedules them on distinct set of nodes:

```
helm install sftd-prod charts/sftd    --set 'nodeSelector.wire\.com/role=sftd-prod' ...other-flags
helm install sftd-staging charts/sftd --set 'nodeSelector.wire\.com/role=sftd-staging' ...other-flags
```

## No public IP on default interface

Often on-prem or at certain cloud providers your nodes will not have directly routable public IP addresses
but are deployed in 1:1 NAT.   This chart is able to auto-detect this scenario if your cloud providers adds
an `ExternalIP` field to your kubernetes node objects.

On on-prem you should set an `wire.com/external-ip` annotation on your kubernetes nodes so that sftd is aware
of its external IP when it gets scheduled on a node.

If you use our kubespray playbooks to bootstrap kubernetes, you simply have to
set the `external_ip` field in your `group_vars`
```yaml
# inventory/group_vars/k8s-cluster
node_annotations:
  wire.com/external-ip: {{ external_ip }}
```
And the `external_ip` is set in the inventory per node:
```
node0 ansible_host=.... ip=...  external_ip=aaa.xxx.yyy.zzz
```

If you are hosting Kubernetes through other means you can annotate your nodes manually:
```
$ kubectl annotate node $HOSTNAME wire.com/external-ip=$EXTERNAL_IP
```

## Port conflicts and `hostNetwork`

Kubernetes by default allocates node ports in the `30000-32768` range. This can
be adjusted with the `--service-nodeport-range` flag.
https://kubernetes.io/docs/concepts/services-networking/service/ SFTD asks the
kernel for free ports, which by default are in the `32768-61000` range
(https://ma.ttias.be/linux-increase-ip_local_port_range-tcp-port-range/).

On a default installation these ranges do not overlap and sftd should never have
conflicts with kubernetes components. You should however check that on your OS
these ranges aren't configured differently.
