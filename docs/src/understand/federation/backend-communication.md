(backend-to-backend-communication)=

# Backend to backend communication

We require communication between the {ref}`federator` of one (sending)
backend and the {ref}`federation_ingress` of another (receiving) backend to be both
mutually authenticated and authorized. More specifically, both backends
need to ensure the following:

- **Authentication**

    Determine the identity (infrastructure domain name) of the other backend.

- **Discovery**

    Ensure that the other backend is authorized to represent the backend
    domain claimed by the other backend.

- **Authorization**

    Ensure that this backend is authorized to federate with the other backend.

(authentication)=

## Authentication

Authentication between Wire backends is achieved using the mutual
authentication feature of TLS as defined in [RFC
8556](https://tools.ietf.org/html/rfc8446).

In particular, this means that the ingress of each backend needs to be
provisioned with one or more trusted root certificates to authenticate
certificates provided by other backends when accepting incoming connections.

Conversely, every *Federator* needs to be provisioned with a client
certificate which it uses to authenticate itself towards other backends.

Note that the client certificate is required to be issued with the backend\'s
infrastructure domain as one of the subject alternative names (SAN), which is defined in
[RFC 5280](https://tools.ietf.org/html/rfc5280).

See {ref}`federation-certificate-setup` for technical instructions.

If a receiving backend fails to authenticate the client certificate, it fails the request
with an `AuthenticationFailure` error.

(discovery)=

## Discovery

The discovery process allows a backend to determine the infrastructure domain of
a given backend domain.

This step is necessary in two scenarios:

-   A backend would like to establish a connection to another backend
    that it only knows the backend domain of. This is the case, for
    example, when a user of a local backend searches for a
    {ref}`qualified username <qualified-identifiers-and-names>`, which only includes the backend domain of that user's backend.
-   When receiving a message from another backend that authenticates
    with a given infrastructure domain and claims to represent a given backend
    domain, a backend would like to ensure the backend domain owner
    authorized the owner of the infrastructure domain to run their Wire backend.

To make discovery possible, any party hosting a Wire backend has to
announce the infrastructure domain via a DNS *SRV* record as defined in [RFC
2782](https://tools.ietf.org/html/rfc2782) with
`service = wire-server-federator, proto = tcp` and with `name` pointing
to the backend\'s domain and *target* to the backend\'s infrastructure domain.

For example, Company A with backend domain *company-a.com* and infrastructure domain *wire.company-a.com* could publish

``` bash
_wire-server-federator._tcp.company-a.com. 600  IN  SRV 10 5 443 federator.wire.company-a.com.
```

A backend can then be discovered, given its domain, by issuing a DNS
query for the SRV record specifying the *wire-server-federator* service.

In case this process fails the Federator fails to forward the request with a `DiscoveryFailure` error.

(dns-scope)=


(srv-ttl-and-caching)=

### SRV TTL and Caching

After retrieving the SRV record for a given domain, the local backend
caches the *backend domain \<\--\> infrastructure domain* mapping for the
duration indicated in the TTL field of the record.

Due to this caching behavior, the TTL value of the SRV record dictates
at which intervals remote backends will refresh their mapping of the
local backend\'s backend domain to infrastructure domain. As a consequence a
value in the order of magnitude of 24 hours will reduce the amount of
overhead for remote backends.

On the other hand in the setup phase of a backend, or when a change of infrastructure
domain is required, a TTL value in the magnitude of a few minutes allows remote
backends to recover more quickly from a change of the infrastructure domain.

(authorization)=

(allow-list)=

## Authorization

After an incoming connection is authenticated the backend authorizes the
request. It does so by verifying that the backend domain of the sender is
contained in the {ref}`domain allow list <configure-federation-allow-list>`.

Since the request is authenticated only by the infrastructure domain the sending backend
is required to add its backend domain as a `Wire-Origin-Domain` header to the
request. The receiving backend follows the process described in {ref}`discovery`
and verifies that the discovered infrastructure domain for the backend domain indicated
in the `Wire-Origin-Domain` header is one of the Subject Alternative Names
contained in the client certificate used to sign the request. If this is not the
case, the receiving backend fails the request with a `ValidationError`.

(per-request-authorization)=

### Per-request authorization

In addition to the general authorization step that is performed by the
federator when a new, mutually authenticated TLS connection is
established, the component processing the request performs an
additional, per-request authorization step.

How this step is performed depends on the API endpoint, the contents of
the request and the context in which it is made.

See the documentation of the individual {ref}`API endpoints <api-endpoints>` for
details.

(federation-back2back-example)=

## Example

The following is an example for the message and information flow between
a backend with backend domain `a.com` and infrastructure domain `infra.a.com` and
another backend with backend domain `b.com` and infrastructure domain
`infra.b.com`.

The content and format of the message is meant to be representative. For
the definitions of the actual payloads, please see the {ref}`federation
API<federation-api>` section.

The scenario is that the brig at `infra.a.com` has received a user
search request from *Alice*, one of its clients.

```{image} img/federation-flow.png
:width: 100%
:align: center
```

(configuring-remote-connections)=

## Configuring Remote Connections

Up to the release containing
[PR#3260](https://github.com/wireapp/wire-server/pull/3260), the
config files of the individual services statically contained the
domains of remote connections.  Starting with this release, this and
all information about remote connections is stored in the database,
and there is an internal REST API for adding and removing remotes:

* [`POST`](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig/#/brig/post_i_federation_remotes)
* [`GET`](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig/#/brig/get_i_federation_remotes)
* [`PUT`](TODO)
* [`DELETE`](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig/#/brig/delete_i_federation_remotes__domain_)

**WARNING:** If you delete a connection, all users from that remote
will be removed from local conversations, and all conversations hosted
by that remote will be removed from the local backend.  Connections
between local and remote users that are removed will be archived, and
can be re-established should you decide to add the same backend later.

{-
TODO: this paragraph still annoys me.  move strategy to brig, too?  or
at least to a different syntax, and force admin to use both old and
new syntax until transition period is over?  just to avoid the
confusing bogus `:` at the end of the flag.

The federation strategy (allow all or allow list) is still configured
in federator, only the list of allowed hosts is ignored; if you select
"allow all" (or if you disable federation), the list of known backends
maintained by brig is mostly ignored, but e.g., search policy is still
considered by brig itself.
-}

{-

TODO: explain how brig doesn't cache, but always read from the
database, and that if you have update cycles of <10? secs, and/or
clusters with >100? pods, you should monitor the load a little after
upgrade.

-}

TODO: explain how things need a while to stabilize (configurable), but
that the other backend also needs to know us in order to be reachable.
(how do we handle one backend being known to the other first for a few
minutes / hours?)


Update intervals are currently supplied by Brig in same response that
carries the federation domain lists. This allows for simplified control
of the update times and minimises changes to both services and their
configuration files.


See {ref}`configuring-remote-connections-dev-perspective` for the
developer's point of view on this topic.

### Transitioning from config file to database state

TODO: you need to update config files!
  - complete list of search policies, no more defaults
  - new fed strategy syntax (keep the old, just copy)
  - later, remove the old syntax in brig, federator.

As of the release containing
[PR#3260](https://github.com/wireapp/wire-server/pull/3260),
[`federationStrategy`](https://github.com/wireapp/wire-server/blob/4a4ba8dd54586e1d85fe4af609990d79ae3d8cc2/charts/federator/values.yaml#L44-L45)
in the federation config file is ignored, and brig's cassandra is used
instead.  Furthermore, for a transition period,
[`setFederationDomainConfigs`](https://github.com/wireapp/wire-server/blob/4a4ba8dd54586e1d85fe4af609990d79ae3d8cc2/charts/brig/templates/configmap.yaml#L250-L252)
from the brig config file also remains being honored.  Attempting to
delete entries that occur in the config file will trigger an error;
delete from the config file first, then from cassandra.

In the future, wire-server will stop honoring the config file data,
and solely rely on brig's cassandra.  From that point onward, you can
delete any connection, whether listed in the config file or not.
Watch out for the release notes to learn when this will happen.
(Something like *"[Federation only] support for remote configuration
in config file is discontinued.  Before upgrading to this release,
upgrade to the release containing
[PR#3260](https://github.com/wireapp/wire-server/pull/3260) first.
After upgrading to this release, `setFederationDomainConfigs` in brig's
config file will be ignored, and you should remove it at your
convenience.  See
[docs](https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections)
for details."*)



### noise!!!



### Federation allow list

As of 2021-07, federation (whatever is implemented by the time you read this) is turned off by default by means of having an empty allow list:

```yaml
# federator.yaml
optSettings:
  federationStrategy:
    allowedDomains: []
```

You can choose to federate with a specific list of allowed servers:


```yaml
# federator.yaml
optSettings:
  federationStrategy:
    allowedDomains:
      - server1.example.com
      - server2.example.com
```

or, you can federate with everyone:

```yaml
# federator.yaml
optSettings:
  federationStrategy:
    # note the 'empty' value after 'allowAll'
    allowAll:

# when configuring helm charts, this becomes (note 'true' after 'allowAll')
# inside helm_vars/wire-server:
federator:
  optSettings:
    federationStrategy:
      allowAll: true
```




this is deprecated:

```
  setFederationDomainConfigs:
    - domain: example.com
      search_policy: no_search

```




**This section is deprecated .  See
https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections
for details.**

#### Restrict user search

TODO: deprecate this, also rename this section.  it's about federation now.

TODO: should we consider the federation strategy from federator in the
union returned by brig for a transition period as well?  (if not, we
need to insist on updating brig's config before this upgrade.  no
remote backend may be unlisted and use the search policy default.  we
should also crash on startup when somebody tries that.)

The lookup and search of users on a wire instance can be configured. This can be done per federated domain.

```yaml
# [brig.yaml]
optSettings:
  setFederationDomainConfigs:
    - domain: example.com
      search_policy: no_search
```

Valid values for `search_policy` are:
- `no_search`: No users are returned by federated searches.
- `exact_handle_search`: Only users where the handle exactly matches are returned.
- `full_search`: Additionally to `exact_handle_search`, users are found by a freetext search on handle and display name.

If there is no configuration for a domain, it's defaulted to `no_search`.




does anybody know off the top of their heads: is [this section](https://wearezeta.atlassian.net/wiki/spaces/BAC/pages/288620677/Processes+shared+with+CS#Different-search-visibility-per-team) still up to date?  and is stern?  [this page](https://docs.wire.com/developer/reference/config-options.html#federated-domain-specific-configuration-settings) tells a different story...
