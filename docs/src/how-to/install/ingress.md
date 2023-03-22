# Ingress traffic to wire-server (ingress-nginx-controller)

*at the time of writing (2023-03), this section assumes you use a kubernetes
version 1.23 or above (tested with 1.26)*

## Installing in a cloud-like environment

Install the ingress controller chart in your helmfile with the defaults, simply
like this:

```yaml
# helmfile.yaml
repositories:
  - name: wire
    url: 'https://s3-eu-west-1.amazonaws.com/public.wire.com/charts'

releases:
  - name: 'ingress-nginx-controller'
    namespace: 'wire'
    chart: 'wire/ingress-nginx-controller'
    version: 'CHANGE_ME'

# charts wire-server and nginx-ingress-services also need to be installed, see other
# documentation
#  - name: ...
#    chart: ...
```

By default, the `wire/ingress-nginx-controller` chart will create a `Deployment`
with services of type `LoadBalancer`, where your kubernetes installation needs
to support dynamic LoadBalancers. If this is not possible, read the next section.

By default three pods will come up and external traffic will be load balanced into these
three pods, which will also do TLS termination and forward traffic to upstream
services (`nginz` and others).

To inspect default TLS settings, see [defaults in the latest code](https://github.com/wireapp/wire-server/blob/develop/charts/ingress-nginx-controller/values.yaml) and also see {ref}`tls`.

## Installing on bare-metal without dynamic load balancer support

In case you cannot create a `kind: service` of `type: LoadBalancer`, then you
can fall back to manually ensure traffic reaches your installation:

```yaml
# helmfile.yaml
releases:
  - name: 'ingress-nginx-controller'
    namespace: 'wire'
    chart: 'wire/ingress-nginx-controller'
    version: 'CHANGE_ME'
    values:
      - './helm_vars/ingress-nginx-controller/values.yaml'
```

Create this file with the following override values:

```yaml
# helm_vars/ingress-nginx-controller/values.yaml
ingress-nginx:
  controller:
    kind: DaemonSet
    service:
      type: NodePort
```

Then, on each of your kubernetes worker nodes, two ports are exposed: ports
31773 (https) and 31772 (http)

You should add a port-forwarding rule on the node or on the loadbalancer that
forwards ports 443 and 80 to these respective ports. Any traffic hitting the http port is simply getting a http 30x redirect to https.

Downsides of this approach: The NodePort approach always requires manual configuration of some external load balancer/firewall to round-robin between node IPs and is error-prone. It's also a bit annoying to have to decide on some global ports that may not be used otherwise.

Most managed K8s clusters have support for LoadBalancers, you can also get this for your own clusters in hcloud etc. It's even possible to do it for pure bare metal, without any "load balancer hardware", by using BGP or some leadership election over who's announcing the "load balancer ip" via ARP (https://metallb.universe.tf/configuration/_advanced_l2_configuration/).

### Using NodePort (not the default) with externalTrafficPolicy=Local (the default)

Normally, NodePort will listen to traffic on all nodes, and uses kube-proxy
to redirect to the node that actually runs ingress-nginx-controller. However
one problem with this is that this traffic is NAT'ed. This means that nginx
will not have access to the source IP address from which the request
originated.  We want to have this source IP address for potentially logging
and rate-limiting based on it. By setting externalTrafficPolicy: local,
nodes will no longer forward requests to other nodes if they receive a
request that they themselves can not handle. Upside is that the traffic is
now not NAT'ed anymore, and we get access to the source IP address. Downside
is that you need to know beforehand which nodes run a certain pod. However,
with kubernetes a pod can be rescheduled to any node at any time so we can
not trust this.  We could do something with node affinities to decide apriori
on what set of nodes will be publicly reachable and make sure the nginx
controller pods are only ran on there but for now that sounds a bit overkill.
Instead, we just simply run the ingress controller on each node using a
daemonset. This means that any node in the cluster can receive requests and
redirect them to the correct service, whilst maintaining the source ip
address. The ingress controller is sort of taking over the role of what
kube-proxy was doing before.
More information:
- https://kubernetes.io/docs/tutorials/services/source-ip/#source-ip-for-services-with-typenodeport
- https://kubernetes.github.io/ingress-nginx/deploy/baremetal/

There are also downsides to setting `externalTrafficPolicy: Local`, please look at the [following blog post](https://www.asykim.com/blog/deep-dive-into-kubernetes-external-traffic-policies), which very clearly explains the upsides and
downsides of this setting
