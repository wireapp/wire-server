# API versioning

This document details the versioning scheme used for wire-server's HTTP APIs.
This applies equally to:

 - the public-facing API (defined in `wire-api`)
 - the federation API (defined in `wire-api-federation`).

## Supported and development versions

An *API version* is a natural number, represented as `vN`, where `N` is the
version. For example, version `5` is denoted `v5`.

A backend advertises a set of *supported* API versions, divided into a set of
*stable* API versions and a set of *development* API versions. These sets can
be discovered via the `GET /api-version` endpoint, which returns a JSON object
of the form:

```
{ "supported": [0, 1, 2, 3, 4],
  "development": [4],
  ...
}
```

where the `...` stands for other fields, which are irrelevant to the purposes
of API versioning. Development versions (usually one), are listed explicitly.
Stable versions are all supported versions that are not development.

Stable versions map to well-defined and fixed API *contracts*, which will not
change over time. That is to say, if two different instances of the backend
both list version `3` in `supported`, a client can assume that they will accept
the same exact requests when version `3` is used, and handle them in exactly
the same way.

On the other hand, development versions give no such guarantees. When making
requests using a development version on some backend, the client needs to be
aware of the corresponding API contract used by that specific backend.

Of course, development versions are useful while building a new API, but are
not suitable for production. Backends deployed to production environments
should disable development versions (and not advertise them in `/api-version`).

Similarly, clients that are meant for production use can decide to ignore
development versions on their backend. This is not strictly necessary, but it
can be used as a safeguard against mistakes in deployment.

The `/api-version` endpoint returns information about the public-facing
(client) API. The corresponding information for the federation API is available
at `/federation/api-version`.

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
stable versions. When we make a change to the development version, no further
actions are needed.

However, the development version is disabled in production, so whenever we need
those changes to actually be deployed, we have to perform a version bump. That
means a few things:

 - freezing the current contract of the development version (see below);
 - turning the development version into a stable version;
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

### Version bump checklist

When making the client API version bump, i.e., when finalising a version, there
are several steps to make apart from deciding what endpoint changes are part of
the version. In these example we assume that version `V6` should be finalized and `V7` should be created:

- Run wire-server and download the `swagger.json` of the current development version, e.g. with the following command: `curl localhost:8080/v6/api/swagger.json | jq > swagger-v6.json` and copy the file to `services/brig/docs/swagger-v6.json`.
- In `wire-api` extend the `Version` type with a new version by appending the
  new version to the end.
- In the same `Version` module update the `developmentVersions` value to list
  only the new version.
- In `services/brig/src/Brig/API/Public.hs`
  - update `versionedSwaggerDocsAPI` so that the finalized version points to the pregenerated swagger, and the dynamically generated swagger spits out swagger for the new `V7`.
- Set the version for `gDefaultAPIVersion` in `integration/test/Testlib/Env.hs` to 7.
- Consider updating the `backendApiVersion` value in Stern, which is
  unit-tested by checking if it is listed as supported in the response to `GET
  /api-version`.

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

#### Removing an endpoint

We keep the endpoint in the routing table, but reduce its version range to
exclude the development version. The implementation is unaffected.

#### Making an incompatible change (e.g. removing a field)

This is the most complicated scenario, but hopefully the rarest. It can only be
done indirectly, as a combination of the two scenarios above: we add a new
endpoint on the same path, set its version range to only the development
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
Named "get-foo@v1" (Until V2 :> "foo" :> Get '[JSON] (Foo V1))
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
 - use the latest (i.e. numerically largest) version that the client supports;
 - if no backend-supported version is supported by the client, return a
   versioning error (either ask the user or the backend administrator to
   upgrade, depending on which versions are higher).

### Examples of endpoint evolution

Just like for the server above, we present some examples of changes in
endpoints, and to go about implementing the corresponding client changes. In
the following, we again assume that the changed endpoint was initially equal
in both version 1 and 2, with 2 being a development version.

#### Adding an optional field or parameter

Client only need to implement version 2, since the change is backwards- and
forwards-compatible. Of course, clients might need to be written in such a way
as to handle the scenario in which the server ignores the extra field or
parameter.

#### Adding a new endpoint

Clients can use the new endpoint, but need to handle the case where the
endpoint is not available, either by catching a 404 error, or by pre-emptively
using the old endpoint when the negotiated version is known not to contain it.

#### Removing an endpoint

Clients would just stop using the removed endpoint. Of course, this usually
means refactoring whatever code was using it so that some other combination of
endpoints is used instead.

#### Incompatible changes to an endpoint

Again, this is a combination of the previous two scenarios. After negotiation,
clients need to determine which version of the endpoint can be used, and act
accordingly.

### Federation client in wire-server

In wire-server itself, changes in the federation API have to be reflected not
only in the implementation of the corresponding handlers, but also in client
invocations.

To that end, the module `Wire.API.Federation.Client` provides a
`FederatorClient` monad which is integrated with the client functionality of
Servant. To create an action in the `FederatorClient` monad, we use `fedClient`
in `Wire.API.Federation.API`, e.g.:

```haskell
fedClient @'Brig @"get-user-clients"
```

returns a function of type `A -> FederatorClient 'Brig B`, where `A` and `B`
are respectively the input and output of the `get-user-clients` endpoint.
Running such an action will automatically perform version negotiation and then
send the corresponding request.

When invoking an endpoint as a federation client, we need to make sure that all
supported versions are covered. The `FederatorClient` monad has an
`Alternative` instance which can be useful for this purpose: an action will fail
(before even performing any request) if it refers to an endpoint whose version
range does not contain the version that was negotiatted.

For example, suppose that `get-user-clients` disappears in version 2, and
clients are now supposed to use an endpoint called `get-clients-ng`, with
slightly different input and output types. Then the client invocation will look
something like:

```haskell
fedClient @'Brig @"get-user-clients" input <|>
(adaptOutput <$> fedClient @'Brig @"get-clients-ng" (adaptInput input)
```

where `adaptInput` and `adaptOutput` are (pure) functions that convert the input
of the old endpoint into the input of the new, and the output of the new
endpoint into the output of the old, respectively.

Many variations on this theme are possible. For example, one could choose to
write adapting functions in terms of the new input/output types, or even use a
mixed approach. The adapting functions need not be pure in general, and they
might even perform further RPC calls.

## Versioning changes in events

Making incompatible changes to events is also sometimes necessary, or at least
desirable. Unfortunately, there is no direct way to make API versioning
preserve compatibility with older clients when the format of events changes.
This is because the format of events is decided when they are generated, which
is of course before they are fetched by the clients. By the time the backend is
made aware of the version supported by a client, it is too late to change any
logic of event generation or serialisation.

However, there are ways to alter the event API in incompatible ways without
breaking older clients. Namely, we can tie a change X in the format of an event
to a specific api version N. This means that in order for a client to support
version N or later, it has to be able to consume events in any format,
before or after X.

If clients respect this constraint, then the backend only needs to keep the old
format around for as long as version N-1 is supported, and can apply change X as
soon as version N-1 is dropped.

Conversely, clients need to be advised on when it is ok for them to drop their
legacy event parsing code. Unfortunately, determining this point in time is
complicated by the fact that legacy events may be retained by a backend for
some time after it has been upgraded to a version that emits events in the new
format. Therefore, this has to be worked out on a case by case basis.

More precisely: When a new version Q of a backend is released, *if* we can
ensure that no version lower than N is running anywhere in production, and
hasn't been for a time at least as long as the maximum event retention time,
*then* we can drop the requirement for clients to be able to read events in the
legacy format, *as long as they support only versions larger or equal to Q*.

### Example timeline

- While version 3 is in development: a new format for an event is introduced in
  the code base, but not yet used for output events, the new format is
  documented for clients.
- Version 3 is finalised: events are still produced using the old format;
  clients that implement v3 are able to parse both event formats.
- Versions 4 to 6 are finalised. No changes to events.
- Support for version 2 is dropped while version 7 is in development. The old
  format can be removed from the code base, and the backend can start producing
  events in the new format. No changes in clients are required.
- Version 7 to 9 are finalised. No further changes to events.
- Version 2 or lower is not used in production anymore. Version 10 is currently
  in development. The old event format is removed from the documentation.
  Clients that support only version 10 or above are not required to understand
  the old format anymore.
