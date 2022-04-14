# API versioning

This document details the versioning scheme used for wire-server's HTTP APIs.
This applies equally to:

 - the public-facing API (defined in `wire-api`)
 - the federation API (defined in `wire-api-federation`).

## Supported and development versions

An *API version* is a natural number, represented as `vN`, where `N` is the
version. For example, version `5` is denoted `v5`.

A backend advertises a set of *supported* API versions and a set of
*development* API versions via the `GET /api-version` endpoint, which returns a
JSON object of the form:

```json
{ "supported": [0, 1, 2, 3],
  "development": [4],
  ...
}
```

where the `...` stands for other fields, which are irrelevant to the purposes
of API versioning.

Supported versions map to well-defined and fixed API *contracts*, which will
not change over time. That is to say, if two different instances of the backend
both list version `3` in `supported`, a client can assume that they will accept
the same exact requests when version `3` is used.

On the other hand, `development` versions give no such guarantees. When making
requests using a development version on some backend, the client needs to be
aware of the corresponding API contract used by that specific backend.

Of course, `development` versions are useful while building a new API, but are
not suitable for production. Backends deployed to production environments
should disable development versions (and not advertise them in `/api-version`).

The `/api-version` endpoint returns information about the public-facing
(client) API by default. However, the API can be selected using the `api` query
parameter, which can take the following values:

 - `api=client` for the client API (default) 
 - `api=federation` for the server-to-server API.

## Making requests to a particular version

An API at version `N` can be accessed by prepending `/vN/` to endpoint paths.
So, for example, to access the endpoint `/conversations` for version `3`, the
correct path is `/v3/conversations`.

To support clients that have not yet implemented versioning, backends that
support version `0` will also accept unversioned requests, which will
automatically be rewritten to `/v0/` requests.

Some endpoints are unversioned. The backend will also accept versioned requests
to them, but they all behave identically regardless of the version prefix. The
unversioned endpoints are:

 - the `/api-version` endpoint itself; this is so that a client can dynamically
   determine which version to use based on the information returned by the
   backend;
 - the `/access` endpoint; this is so that access cookie paths can be set to
   the same value regardless of the version, which avoids invalidating logins
   across version upgrades.

## Server implementation

On the server side, we normally have a single development version and multiple
deprecated supported versions. When we make a change to the development
version, no further actions are needed.

However, the development version is disabled in production, so whenever we need
those changes to actually be deployed, we have to perform a version bump. That
means a few things:

 - freezing the current contract of the development version (see below);
 - turning the development version into a supported version;
 - creating a new development version, which is an exact copy of the previous
   development version at the time of the bump.

To make this process easier, and to avoid unreasonable code duplication in the
definition of the endpoints, API versions are maintained as a single routing
table, appropriately tagged with version information, as described below.

For each endpoint, we record a *range* of versions in which this endpoint
exists. Note that an endpoint is assumed to behave identically regardless of
the version it is being invoked at.

This might seem like a very severe limitation, but in practice most endpoints
evolve by adding optional fields, which means that the implementation can
simply assume that it being called with the most recent version, and that
implicitly will provide support for all older versions as well.

Endpoint changing significantly between two versions are not directly supported
by this system. It is still possible to implement them, but they will appear as
different endpoints (and have different names) on the same path, and with
non-overlapping version ranges.

### Examples of endpoint evolution

In the following, we present some examples of API changes and how they might be
realised in practice. In all examples, we assume a supported API version 1, and
a development version 2, and an endpoint which is identical in both versions
that now needs to be updated.

#### Adding an optional field or parameter

This is the most common scenario, and the system is optimised to handle this
efficiently. We simply go ahead and implement the new version of the endpoint,
while leaving the version ranges unaffected. This works because the new
implementation is also able to function as an implementation of the older
version.

#### Adding a new endpoint

We add the new endpoint to the routing table, and set its version range to only
include the development version. The supported version is unaffected.

#### Removing the endpoint

We keep the endpoint in the routing table, but reduce its version range to
exclude the development version. The implementation is unaffected.

#### Making an incompatible change (e.g. removing a field)

This is the most complicated scenario, but hopefully the rarest. It can only be
done indirectly, as a combination of the two scenarios above: we add a new
endpoint with the same path, set its version range to only the development
version, and then reduce the version range of the existing endpoint.

The new and old endpoint can share code in most cases, but this needs to be
arranged manually. One can use type families to handle differences in version.
It is advised to use global versions to index the handlers and related types in
these cases, rather than making up an endpoint-specific numbering.

For example, the `GET /foo` endpoint at version 1 changes the format of its
response. Before the change, the routing entry looks something like:

```haskell
Named "get-foo" ("foo" :> Get '[JSON] Foo)
```

No version range is specified, resulting in the endpoint being available on all
API versions.

Afterwards, we will have two entries:

```haskell
Named "get-foo@v1" (Until V1 :> "foo" :> Get '[JSON] (Foo V1))
:<|>
Named "get-foo" (From V2 :> "foo" :> Get '[JSON] (Foo V2))
```

where `Foo` has now been turned into a type (or data) family with appropriate
JSON instances to reflect the change in data format. The handler implementation
can be polymorphic in the version and therefore shared, but it does have to be
specified twice when instantiating the server.

### Swagger documentation

Since backwards-compatible changes in endpoints are lumped into single
definitions, it is not possible to automatically generate accurate Swagger
documentation for older versions from the global API routing table.

Therefore, we keep pre-generated Swagger descriptions for all supported
versions in the source, and serve them directly.

When a development version `N` is frozen into a supported one, its
automatically generated Swagger description has to be manually saved to
`services/brig/docs/vN.json`, and kept around for as long as version `N`
remains supported.

The Swagger description for the development version is generated by
pre-processing the global routing table and removing all entries that do not
contain the development version in their range.

## Client implementation

Like servers, clients define a set of versions that they are able to make
requests with. However, since they ultimately make the decision of which
version to use, there is no distinction in clients between supported and
development versions.

Of course, using a version which is marked as `development` on the backend
means utilising an unspecified API contract, so it should not be done in
production.

### Version negotiation

Before making a request to the server, the client needs to have negotiated
which version to use. The recommended flow is as follows:

 - query `GET /api-version` and retrieve the set of supported and development
   versions;
 - decide whether using a development version is unacceptable, and
   if so, take it out of consideration;
 - use the latest (i.e. numerically larger) version that the client supports;
 - if no backend-supported version is supported by the client, return a
   versioning error (either ask the user or the backend administrator to
   upgrade, depending on which versions are higher).
