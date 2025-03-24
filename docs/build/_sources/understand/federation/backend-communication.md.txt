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
