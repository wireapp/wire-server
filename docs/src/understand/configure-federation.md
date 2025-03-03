<a id="configure-federation"></a>

# Federation

See also [Wire Federation](federation/README.md#federation-understand), which explains the architecture and concepts.

#### NOTE
The Federation development is work in progress.

## Summary of necessary steps to configure federation

The steps needed to configure federation are as follows and they will be
detailed in the sections below:

- Choose a backend domain name
- DNS setup for federation (including an `SRV` record)
- Generate and configure TLS certificates:
  - server certificates
  - client certificates
  - a selection of CA certificates you trust when interacting with
    other backends
- Configure helm charts : federator and ingress and webapp subcharts
- Test that your configurations work as expected.

<a id="choose-backend-domain"></a>

## Choose a Backend Domain

As of the release [helm chart 0.129.0, Wire docker version 2.94.0] from
2020-12-15, `federationDomain` is a mandatory configuration setting, which
defines the [backend domain](federation/architecture.md#glossary-backend-domain) of your
installation. Regardless of whether you want to enable federation for a backend
or not, you must decide what its domain is going to be. This helps in keeping
things simpler across all components of Wire and also enables to turn on
federation in the future if required.

It is highly recommended that this domain is configured as something
that is controlled by the administrator/operator(s). The actual servers
do not need to be available on this domain, but you MUST be able to set
an SRV record for `_wire-server-federator._tcp.<Backend Domain>` that
informs other wire-server backends where to find your actual servers.

**IMPORTANT**: Once this option is set, it cannot be changed without
breaking experience for all the users which are already using the
backend.

<a id="consequences-backend-domain"></a>

## Consequences of the choice of a backend domain

- You need control over a specific subdomain of this backend domain
  (to set an SRV DNS record as explained in the next section). Without
  this control, you cannot federate with anyone.
- This backend domain becomes part of the underlying identity of all
  users on your servers.

  Example: Let’s say you choose `example.com` as your backend
  domain. Your user known to you as Alice, and known on your
  server with ID `ac41a202-2555-11ec-9341-00163e5e6c00` will
  become known for other servers you federate with as
  ```json
  {
    "user": {
      "id": "ac41a202-2555-11ec-9341-00163e5e6c00",
      "domain": "example.com"
    }
  }
  ```
- This domain is shown in the User Interface
  alongside user information.

  Example: Using the same example as above, for backends you
  federate with, Alice would be displayed with the
  human-readable username `@alice@example.com` for users on
  other backends.

#### WARNING
*Changing* the backend domain after existing user
activity with a client version (versions later than May/June 2021)
will lead to undefined behaviour (untested, not accounted for during
development) on some or all client platforms (Web, Android, iOS) for
those users: It is possible your clients could crash, or lose part of
their data about themselves or other users and conversations, or
otherwise exhibit unexpected behaviour. If at all possible, do not
change this backend domain. We do not intend to provide support if you
change the backend domain.

<a id="dns-configure-federation"></a>

## DNS setup for federation

### SRV record

One prerequisite to enable federation is an [SRV
record](https://en.wikipedia.org/wiki/SRV_record) as defined in [RFC
2782](https://datatracker.ietf.org/doc/html/rfc2782) that needs to be
set up to allow the wire-server to be discovered by other Wire backends.
See the documentation on
[discovery in federation](federation/backend-communication.md#discovery) for
more information on the role of discovery in federation.

The fields of the SRV record need to be populated as follows

- `service`: `wire-server-federator`
- `proto`: `tcp`
- `name`: <backend-domain>
- `TTL`: e.g. 600 (10 minutes) in an initial phase. This can be set to
  a higher value (e.g. 86400) if your systems are stable and DNS
  records don’t change a lot.
- `priority`: anything. A good default value would be 0
- `weight`: >0 for your server to be reachable. A good default value
  could be 10
- `port`: `443`
- `target`: the infrastructure domain

To give an example, assuming

- your federation
  [Backend Domain](federation/architecture.md#glossary-backend-domain) is `example.com`
- your domains for other services already set up follow the convention
  `<service>.wire.example.org`

then your federation
[Infrastructure Domain](federation/architecture.md#glossary-infra-domain)
would be `federator.wire.example.org`.

The SRV record would look as follows:

```bash
# _service._proto.name.                  ttl IN SRV priority weight port target.
_wire-server-federator._tcp.example.com. 600 IN SRV 0        10     443  federator.wire.example.org.
```

### DNS A record for the federator

Background: `federator` is the server component responsible for incoming
and outgoing requests to other backend; but it is proxied on the
incoming requests by the ingress component on kubernetes as shown in
[Federation Architecture](federation/architecture.md#federation-architecture)

As mentioned in [DNS setup for Helm](../how-to/install/helm-prod.md#helmdns), you also need a `federator.<domain>` record, which,
alongside your other DNS records that point to the ingress component,
also needs to point to the IP of your ingress, i.e. the IP you want to
provide services on.

<a id="federation-certificate-setup"></a>

## Generate and configure TLS server and client certificates

Are your servers on the public internet? Then you have the option of
using TLS certificates from [Let’s encrypt](https://letsencrypt.org/).
In such a case go to subsection (A). If your servers are not on the
public internet or you would like to use your own CA, go to subsection
(B).

### (A) Let’s encrypt TLS server and client certificate generation and renewal

The following will make use of [Let’s
encrypt](https://letsencrypt.org/) for both server certificates (used
when someone sends a request to your `federator.<domain-name>`) and
client certificates (used for making outgoing requests to other
backends).

For that, you need to have
[jetstack/cert-manager](https://github.com/jetstack/cert-manager)
installed. You can follow the helm chart installation
[here](https://cert-manager.io/docs/installation/helm/).

Once you have cert-manager, adjust the email address below, then set the
following in the nginx-ingress-services overrides:

```yaml
# override values for nginx-ingress-services
# (e.g. under ./helm_vars/nginx-ingress-services/values.yaml)
tls:
  useCertManager: true

certManager:
  inTestMode: false
  certmasterEmail: "certificates@example.com"
```

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
federator:
  tls:
    useSharedFederatorSecret: true
```

You can now skip section (B) and go to Configure CA certificates you
trust when interacting with other backends.

### (B) Manual server and client certificates

Use your usual method of obtaining X.509 certificates for your [federation infrastructure domain](federation/architecture.md#glossary-infra-domain) (alongside the other domains needed for a
wire-server installation).

You can use one single certificate and key for both server and client
certificate use.

#### NOTE
Due to a limitation of the TLS library in use
for federation ([hs-tls](https://github.com/vincenthz/hs-tls)), only
some ciphers are supported. Moving to an openssl-based library is
planned, which will provide support for a wider range of ciphers.

Your certificates need to have the “Server” and “Client” key usage
listed among the X509 extensions:

```bash
# inspect your certificate:
openssl x509 -inform pem -noout -text < your-certificate.pem
```

```bash
X509v3 extensions:
    X509v3 Key Usage: critical
        Digital Signature, Key Encipherment
    X509v3 Extended Key Usage:
        TLS Web Server Authentication, TLS Web Client Authentication
```

And your [federation infrastructure domain](federation/architecture.md#glossary-infra-domain) (e.g.
`federator.wire.example.com` from the running example) needs to either figure
explictly in the list of your SAN (Subject Alternative Name):

```bash
X509v3 Subject Alternative Name:
    DNS:federator.wire.example.com, DNS:nginz-https.wire.example.com, ...
```

Or you need to have a wildcard certificate that includes it:

```bash
X509v3 Subject Alternative Name: critical
    DNS:*.wire.example.com
```

Configure the *client certificate* and *private key* inside
wire-server/federator:

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml or helm_vars/wire-server/secrets.yaml)
federator:
  clientCertificateContents: |
    -----BEGIN CERTIFICATE-----
    .....
    -----END CERTIFICATE-----
  clientPrivateKeyContents: |
    -----BEGIN RSA PRIVATE KEY-----
    .....
    -----END RSA PRIVATE KEY-----
```

The *server certificate* and *private key* need to be configured in
`nginx-ingress-services`. Those are used for all of the services, not
just the federator component. If you have installed wire-server before
without federation, server certificates may already be configured
 *(though you probably need to create new certificates to include the
federation infrastructure domain if you’re not making use of wildcard
certificates)*. Server certificates go here:

```yaml
# override values for nginx-ingress-services
# (e.g. under ./helm_vars/nginx-ingress-services/secrets.yaml)
secrets:
  tlsWildcardCert: |
    -----BEGIN CERTIFICATE-----
    ... <cert goes here>
    -----END CERTIFICATE-----

  tlsWildcardKey: |
    -----BEGIN RSA PRIVATE KEY -----
    ... <private key goes here>
    -----END RSA PRIVATE KEY-----
```

### Configure CA certificates you trust when interacting with other backends

If you want to federate with servers at `othercompany.example.com`, then
you need to trust the CA (Certificate Authority) certificate that
`othercompany.example.com` has used to sign its client certificates.

They need to be set both for the nginx-ingress-services and the
wire-server chart.

```yaml
# override values for nginx-ingress-services
# (e.g. under ./helm_vars/nginx-ingress-services/values.yaml)
secrets:
  tlsClientCA: |
    -----BEGIN CERTIFICATE-----
    ... <CA in PEM format goes here>
    -----END CERTIFICATE-----
    -----BEGIN CERTIFICATE-----
    ... <another CA in PEM format goes here>
    -----END CERTIFICATE-----
```

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
federator:
  remoteCAContents: |
    -----BEGIN CERTIFICATE-----
    ... <CA in PEM format goes here>
    -----END CERTIFICATE-----
    -----BEGIN CERTIFICATE-----
    ... <another CA in PEM format goes here>
    -----END CERTIFICATE-----
```

### Tell parties you intend to federate with about your certificates

The backends you want to federate with should add your (or Let’s
Encrypt’s) CA to their store, so you should give them your CA
certificate, or tell them to use the appropriate Let’s Encrypt root
certificate.

## Configure helm charts: federator and ingress and webapp subcharts

### Set your chosen backend domain

Read [Choose a Backend Domain](#choose-backend-domain) again, then
set the backend domain three times to the same value in the subcharts
cargohold, galley and brig. You also need to set `enableFederation` to
`true` in background-worker in addition to those charts.

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
galley:
  config:
    enableFederation: true
    settings:
      federationDomain: example.com # your chosen "backend domain"

brig:
  config:
    enableFederation: true
    optSettings:
      setFederationDomain: example.com # your chosen "backend domain"

cargohold:
  config:
    enableFederation: true
    settings:
      federationDomain: example.com # your chosen "backend domain"

background-worker:
  config:
    enableFederation: true
```

<a id="configure-federation-strategy-in-brig"></a>

### Configure federation strategy (whom to federate with) in brig

**Since [PR#3260](https://github.com/wireapp/wire-server/pull/3260).**

Also see [Keeping track of federator remotes](../developer/developer/federation-design-aspects.md#configuring-remote-connections-dev-perspective) for the
developer’s point of view on this topic.

You also need to define the federation strategy (whom to federate
with), and the frequency with which the other backend services will
refresh their cache of this configuration.

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
brig:
  config:
    optSettings:
      setFederationStrategy: allowNone # [allowAll | allowDynamic | allowNone]
      setFederationDomainConfigsUpdateFreq: 10 # seconds
```

The default strategy of `allowNone` effectively disables federation
(and probably isn’t what you want if you are reading this).
`allowAll` federates with any backend that requests contact or that a
user uses in a search.  `allowDynamic` only federates with known
remote backends listed in cassandra.

The update frequency determines how often other services will refresh
the information about remote connections from brig.

More information about individual remote connections is stored in
brig’s cassandra, and maintained via internal brig api end-points by
the sysadmin:

* [`POST`](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig/#/brig/post_i_federation_remotes)
  - after adding a new remote backend, wait for the other end to do
    the same with you, and then wait a few moments for things to
    stabilize (at least `update_interval * 2`; see below).
* [`GET`](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig/#/brig/get_i_federation_remotes)
  - this serves an object with 3 fields:
    - `remotes` (from cassandra): the list of remote domains with search policy (and
      possibly other information in the future);
    - `strategy` (from config): federation strategy; one of `allowNone`, `allowDynamic`, `allowAll` (see above)
    - `update_interval` (from config): the suggested update frequency with which calling
      services should refresh their information.
  - It doesn’t serve the local domain, which needs to be configured
    for every service that needs to know it individually.  This may
    change in the future.
  - This end-point enjoys a comparably high amount of traffic.  If you
    have many pods (a large instance with say, >100 pods), *and* you set a very
    short update interval (<10s), you should monitor brig’s service and
    database load closely in the beginning.
* [`PUT`](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig/#/brig/put_i_federation_remotes__domain_)
* **NOTE:** De-federating (`DELETE`) has been removed from the API to
  avoid a scalability issue.  Watch out for a fix in the changelog!

The `remotes` list looks like this:

```default
[
  {
    "domain": "wire.example.com",
    "search_policy": "full_search"
  },
  {
    "domain": "evil.example.com"
    "search_policy": "no_search"
  },
  ...
]
```

It serves two purposes:

1. If federation strategy is `allowDynamic`, only backends that are
   listed can be reached by us and can reach us;
2. Independently of the federation strategy, the list provides
   information about remote backends that may change dynamically (at
   the time of writing this: search policy, see
   [Searching users on another federated backend](searchability.md#searching-users-on-another-federated-backend) and
   [User Searchability](searchability.md#user-searchability) for more context)

The search policy for a remote backend can be:

- `no_search`: No users are returned by federated searches.  default.
- `exact_handle_search`: Only users where the handle exactly matches are returned.
- `full_search`: Additionally to `exact_handle_search`, users are found by a freetext search on handle and display name.

If federation strategy is `allowAll`, and there is no entry for a
domain in the database, default is `no_search`.  The field in
cassandra is not nullable, ie., you always have to explicitly name a
search policy if you create an entry.

#### If your instance has been federating before

You only need to read this section if your instance has been
federating with other instances prior to
[PR#3260](https://github.com/wireapp/wire-server/pull/3260), and you
are upgrading to the release containing that PR.

From now on the federation policy set in the federator config under
`federationStrategy` is ignored.  Instead, the federation strategy is
pulled by all services from brig, who in turn gets it from a
combination of config file and database (see
[Configure federation strategy (whom to federate with) in brig](#configure-federation-strategy-in-brig) above).

In order to achieve a zero-downtime upgrade, follow these steps:

1. Update the brig config values file as described above.
2. If you have chosen `brig.config.optSettings.setFederationStrategy: allowDynamic` you need to make sure the list of all domains you want
   to allow federation with is complete (before, there was a search
   policy default; now wire will stop federating with removes that are
   not listed here).  Example:
   ```yaml
   brig:
     config:
       optSettings:
         setFederationDomainConfigs:
         - domain: red.example.com
           search_policy: full_search
         - domain: blue.example.com
           search_policy: no_search
   ```

   This change is to cover the time window between upgrading the brig
   pods and populating cassandra with the information needed (see
   Step 3 below).

   Any later lookup of this information will return the union of what
   is in cassandra and what is in the config file.  Any attempt to
   write data to cassandra that contradicts data in the config file
   will result in an error.  Before you change any remote domain
   config, remove it from the config file.
3. Populate cassandra with remote domain configs as described above.
4. At any time after you are done with the upgrade and have convinced
   yourself everything went smoothly, remove outdated brig and
   federator config values, in particular:
   - `brig.config.optSettings.setFederationDomainConfigs`
   - `federator.config.optSettings.federationStrategy`

   At a later point, wire-server will start ignoring
   `setFederationDomainConfigs` altogether (follow future entries in
   the changelog to learn when that happens).

### Configure federator process to run and allow incoming traffic

For federation to work, the `federator` subchart of wire-server has to
be enabled:

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
tags:
  federator: true
```

You also need to enable ingress->federator proxying and configure the
charts to use the DNS you configured as a target in
[DNS setup for federation](#dns-configure-federation) above

```yaml
# override values for nginx-ingress-services
# (e.g. under ./helm_vars/nginx-ingress-services/values.yaml)
federator:
  enabled: true

config:
  dns:
    federator: federator.wire.example.org # set this to your "infra" domain
```

### Configure the validation depth when handling client certificates

By default, `verify_depth` is `1`, meaning that in order to validate an
incoming request from another backend, this backend needs to have a
client certificate that is directly (without any intermediate
certificates) signed by a CA certificate from the trust store.

Example: If you trust a CA `root` which signs an intermediate
`intermediate-1` which in turn signs `intermediate-2` which finally
signs `leaf`, and `leaf` is used during mutual TLS when validating
incoming requests, then `verify_depth` would need to be set to `3`.

```yaml
# nginx-ingress-services/values.yaml
tls:
  # the validation depth between a federator client certificate and tlsClientCA
  verify_depth: 3 # default: 1
```

<a id="configure-federation-allow-list"></a>

### Configure the allow list

By default, federation is turned off (allow list set to the empty list):

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
federator:
  config:
    optSettings:
      federationStrategy:
        allowedDomains: []
```

You can choose to federate with a specific list of allowed backends:

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
federator:
  config:
    optSettings:
      federationStrategy:
        allowedDomains:
         - example.com
         - example.org
```

Alternatively, you can federate with everyone:

```yaml
# override values for wire-server
# (e.g. under ./helm_vars/wire-server/values.yaml)
federator:
  config:
    optSettings:
      federationStrategy:
        allowAll: true
```

## Applying all configuration changes

Depending on your installation method and time you initially installed
your first version of wire-server, commands to run to apply all of the
above configrations may vary. You want to ensure that you upgrade the
`nginx-ingress-services` and `wire-server` helm charts at a minimum.

## Manually test that your configurations work as expected

### Manually test DNS

If you use `dig` to check for SRV records, use e.g.:

```none
dig +short SRV _wire-server-federator._tcp.wire.example.com
```

Should yield something like:

```none
0 10 443 federator.wire.example.com.
```

The actual target:

```none
dig +short federator.wire.example.com
```

should also point to an IP address:

```none
1.2.3.4 # of course you should get a valid IP here
```

Ensure that the IP matches where your backend ingress runs.

### Manually test certificates

Refer to [How can I see if my TLS certificates are configured the way I expect?](../how-to/administrate/general-linux.md#how-to-see-tls-certs) and set
DOMAIN to your
[federation infrastructure domain](federation/architecture.md#glossary-infra-domain). They should include your domain as part of the SAN (Subject
Alternative Names) and not have expired.

### Manually test that federation works

Prerequisites:

- You need two backends with federation configured and enabled.
- They both need to have each other in the allow list.
- They both need to trust each other’s CA certificate.

Create user accounts on both backends.

With one user, search for the other user using the
`@username-1@example.com` syntax in the UI search field of the webapp.
