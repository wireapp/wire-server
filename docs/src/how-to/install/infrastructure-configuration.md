<a id="configuration-options"></a>

# Infrastructure configuration options

This contains instructions to configure specific aspects of your production setup depending on your needs.

Depending on your use-case and requirements, you may need to
configure none, or only a subset of the following sections.

## Redirect some traffic through a http(s) proxy

In case you wish to use http(s) proxies, you can add a configuration like this to the wire-server services in question:

Assuming your proxy can be reached from within Kubernetes at `http://proxy:8080`, add the following for each affected service (e.g. `gundeck`) to your Helm overrides in `values/wire-server/values.yaml` :

```yaml
gundeck:
  # ...
  config:
    # ...
    proxy:
      httpProxy: "http://proxy:8080"
      httpsProxy: "http://proxy:8080"
      noProxyList:
        - "localhost"
        - "127.0.0.1"
        - "10.0.0.0/8"
        - "elasticsearch-external"
        - "cassandra-external"
        - "redis-ephemeral"
        - "fake-aws-sqs"
        - "fake-aws-dynamodb"
        - "fake-aws-sns"
        - "brig"
        - "cargohold"
        - "galley"
        - "gundeck"
        - "proxy"
        - "spar"
        - "federator"
        - "cannon"
        - "cannon-0.cannon.default"
        - "cannon-1.cannon.default"
        - "cannon-2.cannon.default"
```

Depending on your setup, you may need to repeat this for the other services like `brig` as well.

<a id="push-sns"></a>

## Enable push notifications using the public appstore / playstore mobile Wire clients

1. You need to get in touch with us. Please talk to sales or customer support - see [https://wire.com](https://wire.com)
2. If a contract agreement has been reached, we can set up a separate AWS account for you containing the necessary AWS SQS/SNS setup to route push notifications through to the mobile apps. We will then forward some configuration / access credentials that looks like:

```yaml
gundeck:
  config:
    aws:
      account: "<REDACTED>"
      arnEnv: "<REDACTED>"
      queueName: "<REDACTED>-gundeck-events"
      region: "<REDACTED>"
      snsEndpoint: "https://sns.<REDACTED>.amazonaws.com"
      sqsEndpoint: "https://sqs.<REDACTED>.amazonaws.com"
  secrets:
    awsKeyId: "<REDACTED>"
    awsSecretKey: "<REDACTED>"
```

To make use of those, first test the credentials are correct, e.g. using the `aws` command-line tool (for more information on how to configure credentials, please refer to the [official docs](https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-quickstart.html#cli-configure-quickstart-precedence)):

```default
AWS_REGION=<region>
AWS_ACCESS_KEY_ID=<...>
AWS_SECRET_ACCESS_KEY=<...>
ENV=<environment> #e.g staging

aws sqs get-queue-url --queue-name "$ENV-gundeck-events"
```

You should get a result like this:

```default
{
    "QueueUrl": "https://<region>.queue.amazonaws.com/<aws-account-id>/<environment>-gundeck-events"
}
```

Then add them to your gundeck configuration overrides.

Keys below `gundeck.config` belong into `values/wire-server/values.yaml`:

```yaml
gundeck:
  # ...
  config:
    aws:
      queueName: # e.g. staging-gundeck-events
      account: # <aws-account-id>, e.g. 123456789
      region: # e.g. eu-central-1
      snsEndpoint: # e.g. https://sns.eu-central-1.amazonaws.com
      sqsEndpoint: # e.g. https://sqs.eu-central-1.amazonaws.com
      arnEnv: # e.g. staging - this must match the environment name (first part of queueName)
```

Keys below `gundeck.secrets` belong into `values/wire-server/secrets.yaml`:

```yaml
gundeck:
  # ...
  secrets:
    awsKeyId: CHANGE-ME
    awsSecretKey: CHANGE-ME
```

After making this change and applying it to gundeck (ensure gundeck pods have restarted to make use of the updated configuration - that should happen automatically), make sure to reset the push token on any mobile devices that you may have in use.

Next, if you want, you can stop using the `fake-aws-sns` pods in case you ran them before:

```yaml
# inside override values/fake-aws/values.yaml
fake-aws-sns:
  enabled: false
```

## Controlling the speed of websocket draining during cannon pod replacement

The ‘cannon’ component is responsible for persistent websocket connections.
Normally the default options would slowly and gracefully drain active websocket
connections over a maximum of `(amount of cannon replicas * 30 seconds)` during
the deployment of a new wire-server version. This will lead to a very brief
interruption for Wire clients when their client has to re-connect on the
websocket.

You’re not expected to need to change these settings.

The following options are only relevant during the restart of cannon itself.
During a restart of nginz or ingress-controller, all websockets will get
severed. If this is to be avoided, see section [Separate incoming websocket network traffic from the rest of the https traffic](#separate-websocket-traffic)

`drainOpts`: Drain websockets in a controlled fashion when cannon receives a
SIGTERM or SIGINT (this happens when a pod is terminated e.g. during rollout
of a new version). Instead of waiting for connections to close on their own,
the websockets are now severed at a controlled pace. This allows for quicker
rollouts of new versions.

There is no way to entirely disable this behaviour, two extreme examples below

- the quickest way to kill cannon is to set `gracePeriodSeconds: 1` and
  `minBatchSize: 100000` which would sever all connections immediately; but it’s
  not recommended as you could DDoS yourself by forcing all active clients to
  reconnect at the same time. With this, cannon pod replacement takes only 1
  second per pod.
- the slowest way to roll out a new version of cannon without severing websocket
  connections for a long time is to set `minBatchSize: 1`,
  `millisecondsBetweenBatches: 86400000` and `gracePeriodSeconds: 86400`
  which would lead to one single websocket connection being closed immediately,
  and all others only after 1 day. With this, cannon pod replacement takes a
  full day per pod.

```yaml
# overrides for wire-server/values.yaml
cannon:
  drainOpts:
    # The following defaults drain a minimum of 400 connections/second
    # for a total of 10000 over 25 seconds
    # (if cannon holds more connections, draining will happen at a faster pace)
    gracePeriodSeconds: 25
    millisecondsBetweenBatches: 50
    minBatchSize: 20
```

## Control nginz upstreams (routes) into the Kubernetes cluster

Open unterminated upstreams (routes) into the Kubernetes cluster are a potential
security issue. To prevent this, there are fine-grained settings in the nginz
configuration defining which upstreams should exist.

### Default upstreams

Upstreams for services that exist in (almost) every Wire installation are
enabled by default. These are:

- `brig`
- `cannon`
- `cargohold`
- `galley`
- `gundeck`
- `spar`

For special setups (as e.g. described in [separate-websocket-traffic]) the
upstreams of these services can be ignored (disabled) with the setting
`nginz.nginx_conf.ignored_upstreams`.

The most common example is to disable the upstream of `cannon`:

```yaml
nginz:
  nginx_conf:
    ignored_upstreams: ["cannon"]
```

### Optional upstreams

There are some services that are usually not deployed on most Wire installations
or are specific to the Wire cloud:

- `ibis`
- `galeb`
- `calling-test`
- `proxy`

The upstreams for those are disabled by default and can be enabled by the
setting `nginz.nginx_conf.enabled_extra_upstreams`.

The most common example is to enable the (extra) upstream of `proxy`:

```yaml
nginz:
  nginx_conf:
    enabled_extra_upstreams: ["proxy"]
```

### Combining default and extra upstream configurations

Default and extra upstream configurations are independent of each other. I.e.
`nginz.nginx_conf.ignored_upstreams` and
`nginz.nginx_conf.enabled_extra_upstreams` can be combined in the same
configuration:

```yaml
nginz:
  nginx_conf:
    ignored_upstreams: ["cannon"]
    enabled_extra_upstreams: ["proxy"]
```

<a id="separate-websocket-traffic"></a>

## Separate incoming websocket network traffic from the rest of the https traffic

By default, incoming network traffic for websockets comes through these network
hops:

Internet -> LoadBalancer -> kube-proxy -> nginx-ingress-controller -> nginz -> cannon

In order to have graceful draining of websockets when something gets restarted, as it is not easily
possible to implement the graceful draining on nginx-ingress-controller or nginz by itself, there is
a configuration option to get the following network hops:

Internet -> separate LoadBalancer for cannon only -> kube-proxy -> [nginz->cannon (2 containers in the same pod)]

```yaml
# example on AWS when using cert-manager for TLS certificates and external-dns for DNS records
# (see wire-server/charts/cannon/values.yaml for more possible options)

# in your wire-server/values.yaml overrides:
cannon:
  service:
    nginz:
      enabled: true
      hostname: "nginz-ssl.example.com"
      externalDNS:
        enabled: true
      certManager:
        enabled: true
      annotations:
        service.beta.kubernetes.io/aws-load-balancer-type: "nlb"
        service.beta.kubernetes.io/aws-load-balancer-scheme: "internet-facing"
nginz:
  nginx_conf:
    ignored_upstreams: ["cannon"]
```

```yaml
# in your wire-server/secrets.yaml overrides:
cannon:
  secrets:
    nginz:
      zAuth:
        publicKeys: ... # same values as in nginz.secrets.zAuth.publicKeys
```

```yaml
# in your nginx-ingress-services/values.yaml overrides:
websockets:
  enabled: false
```

## You may want

- more server resources to ensure
  [high-availability]()
- an email/SMTP server to send out registration emails
- depending on your required functionality, you may or may not need an
  [AWS account](https://aws.amazon.com/). See details about
  limitations without an AWS account in the following sections.
- one or more people able to maintain the installation
- official support by Wire ([contact us](https://wire.com/pricing/))

#### WARNING
As of 2020-08-10, the documentation sections below are partially out of date and need to be updated.

## Metrics/logging

- [Monitoring wire-server using Prometheus and Grafana](monitoring.md#monitoring)
- [Installing centralized logging dashboards using Kibana](logging.md#logging)

## SMTP server

**Assumptions**: none

**Provides**:

- full control over email sending

**You need**:

- SMTP credentials (to allow for email sending; prerequisite for
  registering users and running the smoketest)

**How to configure**:

- *if using a gmail account, ensure to enable* [‘less secure
  apps’](https://support.google.com/accounts/answer/6010255?hl=en)
- Add user, SMTP server, connection type to `values/wire-server`’s
  values file under `brig.config.smtp`
- Add password in `secrets/wire-server`’s secrets file under
  `brig.secrets.smtpPassword`

## Load balancer on bare metal servers

**Assumptions**:

- You installed kubernetes on bare metal servers or virtual machines
  that can bind to a public IP address.
- **If you are using AWS or another cloud provider, see**[Creating a
  cloudprovider-based load
  balancer]()**instead**

**Provides**:

- Allows using a provided Load balancer for incoming traffic
- SSL termination is done on the ingress controller
- You can access your wire-server backend with given DNS names, over
  SSL and from anywhere in the internet

**You need**:

- A kubernetes node with a *public* IP address (or internal, if you do
  not plan to expose the Wire backend over the Internet but we will
  assume you are using a public IP address)
- DNS records for the different exposed addresses (the ingress depends
  on the usage of virtual hosts), namely:
  - `nginz-https.<domain>`
  - `nginz-ssl.<domain>`
  - `assets.<domain>`
  - `webapp.<domain>`
  - `account.<domain>`
  - `teams.<domain>`
- A wildcard certificate for the different hosts (`*.<domain>`) - we
  assume you want to do SSL termination on the ingress controller

**Caveats**:

- Note that there can be only a *single* load balancer, otherwise your
  cluster might become
  [unstable](https://metallb.universe.tf/installation/)

**How to configure**:

```default
cp values/metallb/demo-values.example.yaml values/metallb/demo-values.yaml
cp values/nginx-ingress-services/demo-values.example.yaml values/nginx-ingress-services/demo-values.yaml
cp values/nginx-ingress-services/demo-secrets.example.yaml values/nginx-ingress-services/demo-secrets.yaml
```

- Adapt `values/metallb/demo-values.yaml` to provide a list of public
  IP address CIDRs that your kubernetes nodes can bind to.
- Adapt `values/nginx-ingress-services/demo-values.yaml` with correct URLs
- Put your TLS cert and key into
  `values/nginx-ingress-services/demo-secrets.yaml`.

Install `metallb` (for more information see the
[docs](https://metallb.universe.tf)):

```sh
helm upgrade --install --namespace metallb-system metallb wire/metallb \
    -f values/metallb/demo-values.yaml \
    --wait --timeout 1800
```

Install `nginx-ingress-[controller,services]`:

::
: helm upgrade –install –namespace demo demo-nginx-ingress-controller wire/nginx-ingress-controller

: –wait

helm upgrade –install –namespace demo demo-nginx-ingress-services wire/nginx-ingress-services

: -f values/nginx-ingress-services/demo-values.yaml -f values/nginx-ingress-services/demo-secrets.yaml –wait

Now, create DNS records for the URLs configured above.

## Load Balancer on cloud-provider

### AWS

[Upload the required
certificates](https://aws.amazon.com/premiumsupport/knowledge-center/import-ssl-certificate-to-iam/).
Create and configure `values/aws-ingress/demo-values.yaml` from the
examples.

```default
helm upgrade --install --namespace demo demo-aws-ingress wire/aws-ingress \
    -f values/aws-ingress/demo-values.yaml \
    --wait
```

To give your load balancers public DNS names, create and edit
`values/external-dns/demo-values.yaml`, then run
[external-dns](https://github.com/helm/charts/tree/master/stable/external-dns):

```default
helm repo update
helm upgrade --install --namespace demo demo-external-dns stable/external-dns \
    --version 1.7.3 \
    -f values/external-dns/demo-values.yaml \
    --wait
```

Things to note about external-dns:

- There can only be a single external-dns chart installed (one per
  kubernetes cluster, not one per namespace). So if you already have
  one running for another namespace you probably don’t need to do
  anything.
- You have to add the appropriate IAM permissions to your cluster (see
  the
  [README](https://github.com/helm/charts/tree/master/stable/external-dns)).
- Alternatively, use the AWS route53 console.

### Other cloud providers

This information is not yet available. If you’d like to contribute by
adding this information for your cloud provider, feel free to read the
[contributing guidelines](https://github.com/wireapp/wire-server-deploy/blob/master/CONTRIBUTING.md) and open a PR.

## Real AWS services

**Assumptions**:

- You installed kubernetes and wire-server on AWS

**Provides**:

- Better availability guarantees and possibly better functionality of
  AWS services such as SQS and dynamoDB.
- You can use ELBs in front of nginz for higher availability.
- instead of using a smtp server and connect with SMTP, you may use
  SES. See configuration of brig and the `useSES` toggle.

**You need**:

- An AWS account

**How to configure**:

- Instead of using fake-aws charts, you need to set up the respective
  services in your account, create queues, tables etc. Have a look at
  the fake-aws-\* charts; you’ll need to replicate a similar setup.
  - Once real AWS resources are created, adapt the configuration in
    the values and secrets files for wire-server to use real endpoints
    and real AWS keys. Look for comments including
    `if using real AWS`.
- Creating AWS resources in a way that is easy to create and delete
  could be done using either [terraform](https://www.terraform.io/)
  or [pulumi](https://pulumi.io/). If you’d like to contribute by
  creating such automation, feel free to read the [contributing
  guidelines](https://github.com/wireapp/wire-server-deploy/blob/master/CONTRIBUTING.md) and open a PR.

## Persistence and high-availability

Currently, due to the way kubernetes and cassandra
[interact](https://github.com/kubernetes/kubernetes/issues/28969),
cassandra cannot reliably be installed on kubernetes. Some people have
tried, e.g. [this
project](https://github.com/instaclustr/cassandra-operator) though at
the time of writing (Nov 2018), this does not yet work as advertised. We
recommend therefore to install cassandra, (possibly also elasticsearch
and redis) separately, i.e. outside of kubernetes (using 3 nodes each).

For further higher-availability:

- scale your kubernetes cluster to have separate etcd and master nodes
  (3 nodes each)
- use 3 instead of 1 replica of each wire-server chart

## Security

For a production deployment, you should, as a minimum:

- Ensure traffic between kubernetes nodes, etcd and databases are
  confined to a private network
- Ensure kubernetes API is unreachable from the public internet (e.g.
  put behind VPN/bastion host or restrict IP range) to prevent
  [kubernetes
  vulnerabilities](https://www.cvedetails.com/vulnerability-list/vendor_id-15867/product_id-34016/Kubernetes-Kubernetes.html)
  from affecting you
- Ensure your operating systems get security updates automatically
- Restrict ssh access / harden sshd configuration
- Ensure no other pods with public access than the main ingress are
  deployed on your cluster, since, in the current setup, pods have
  access to etcd values (and thus any secrets stored there, including
  secrets from other pods)
- Ensure developers encrypt any secrets.yaml files

Additionally, you may wish to build, sign, and host your own docker
images to have increased confidence in those images. We haved “signed
container images” on our roadmap.

## 3rd-party proxying

You need Giphy/Google/Spotify/Soundcloud API keys (if you want to
support previews by proxying these services)

See the `proxy` chart for configuration.

## Routing traffic to other namespaces via nginz

If you have some components running in namespaces different from nginz. For
instance, the billing service (`ibis`) could be deployed to a separate
namespace, say `integrations`. But it still needs to get traffic via
`nginz`. When this is needed, the helm config can be adjusted like this:

```yaml
# in your wire-server/values.yaml overrides:
nginz:
  nginx_conf:
    upstream_namespace:
      ibis: integrations
```

## Marking an installation as self-hosted

In case your wire installation is self-hosted (on-premise, demo installs), it needs to be aware that it is through a configuration option.  As of release chart 4.15.0, `"true"` is the default behavior, and nothing needs to be done.

If that option is not set, team-settings will prompt users about “wire for free” and associated functions.

With that option set, all payment related functionality is disabled.

The option is `IS_SELF_HOSTED`, and you set it in your `values.yaml` file (originally a copy of `prod-values.example.yaml` found in `wire-server-deploy/values/wire-server/`).

In case of a demo install, replace `prod` with `demo`.

First set the option under the `team-settings` section, `envVars` sub-section:

```yaml
envVars:
  IS_SELF_HOSTED: "true"
```

Second, also set the option for `account-pages` helm chart:

```yaml
envVars:
  IS_SELF_HOSTED: "true"
```

<a id="auth-cookie-config"></a>

## Configuring authentication cookie throttling

Authentication cookies and the related throttling mechanism is described in the *API documentation*:
[Cookies](../../understand/api-client-perspective/authentication.md#login-cookies)

The maximum number of cookies per account and type is defined by the brig option
`setUserCookieLimit`. Its default is `32`.

Throttling is configured by the brig option `setUserCookieThrottle`. It is an
object that contains two fields:

`stdDev`

: The minimal standard deviation of cookie creation timestamps in
Seconds. (Default: `3000`,
[Wikipedia: Standard deviation](https://en.wikipedia.org/wiki/Standard_deviation))

`retryAfter`

: Wait time in Seconds when `stdDev` is violated. (Default: `86400`)

The default values are fine for most use cases. (Generally, you don’t have to
configure them for your installation.)

Condensed example:

```yaml
brig:
    optSettings:
        setUserCookieLimit: 32
        setUserCookieThrottle:
            stdDev: 3000
            retryAfter: 86400
```

## S3 Addressing Style

S3 can either by addressed in path style, i.e.
`https://<s3-endpoint>/<bucket-name>/<object>`, or vhost style, i.e.
`https://<bucket-name>.<s3-endpoint>/<object>`. AWS’s S3 offering has deprecated
path style addressing for S3 and completely disabled it for buckets created
after 30 Sep 2020:
[https://aws.amazon.com/blogs/aws/amazon-s3-path-deprecation-plan-the-rest-of-the-story/](https://aws.amazon.com/blogs/aws/amazon-s3-path-deprecation-plan-the-rest-of-the-story/)

However other object storage providers (specially self-deployed ones like MinIO)
may not support vhost style addressing yet (or ever?). Users of such buckets
should configure this option to “path”:

```yaml
cargohold:
  aws:
    s3AddressingStyle: path
```

Installations using S3 service provided by AWS, should use “auto”, this option
will ensure that vhost style is only used when it is possible to construct a
valid hostname from the bucket name and the bucket name doesn’t contain a ‘.’.
Having a ‘.’ in the bucket name causes TLS validation to fail, hence it is not
used by default:

```yaml
cargohold:
  aws:
    s3AddressingStyle: auto
```

Using “virtual” as an option is only useful in situations where vhost style
addressing must be used even if it is not possible to construct a valid hostname
from the bucket name or the S3 service provider can ensure correct certificate
is issued for bucket which contain one or more ‘.’s in the name:

```yaml
cargohold:
  aws:
    s3AddressingStyle: virtual
```

When this option is unspecified, wire-server defaults to path style addressing
to ensure smooth transition for older deployments.

## I have a team larger than 500 users

By default, the maximum number of users in a team is set at 500.

This can be changed in the Brig config, with this option:

```yaml

    optSettings:
      setMaxTeamSize: 501

```

#### NOTE
If you create a team with more than 2000 members then clients won’t receive certain team update events (e.g. new member joining) live via websocket anymore, but most of the app will still function normally.

Irrespective of team size conversations can have at most 2000 members. This limit cannot be overridden.

Wire’s backend currently only supports fanning out a single message to at most 2000 recipients, hence the limitations on conversation size. Increasing this limit is work in progress.
