# Servant

We currently use Servant for the public (i.e. client-facing) API in brig, galley and spar, as well as for their federation (i.e. server-to-server) and internal API.

Client-facing APIs are defined in `Wire.API.Routes.Public.{Brig,Galley}`. Internal APIs are all over the place at the moment. Federation APIs are in `Wire.API.Federation.API.{Brig,Galley}`.

Our APIs are able to generate Swagger documentation semi-automatically using `servant-swagger2`. The `schema-profunctor` library (see [`README.md`](https://github.com/wireapp/wire-server/blob/develop/libs/schema-profunctor/README.md) in `libs/schema-profunctor`) is used to create "schemas" for the input and output types used in the Servant APIs. A schema contains all the information needed to serialise/deserialise JSON values, as well as the documentation and metadata needed to generate Swagger.

## Combinators

We have employed a few custom combinators to try to keep HTTP concerns and vocabulary out of the API handlers that actually implement the functionality of the API.

### `ZAuth`

This is a family of combinators to handle the headers that nginx adds to requests. We currently have:

  - `ZUser`: extracts the `UserId` in the `Z-User` header.
  - `ZLocalUser`: same as `ZUser`, but as a `Local` object (i.e. qualified by the local domain); this is useful when writing federation-aware handlers.
  - `ZConn`: extracts the `ConnId` in the `Z-Connection` header.
  - `ZConversation`: extracts the `ConvId` in the `Z-Conversation` header.

### `MultiVerb`

This is an alternative to `UVerb`, designed to prevent any HTTP-specific information from leaking into the type of the handler. Use this for endpoints that can return multiple responses.

### `CanThrow`

This can be used to add an error response to the Swagger documentation. In services that use polysemy for error handling (currently only Galley), it also adds a corresponding error effect to the type of the handler. The argument of `CanThrow` can be of a custom kind, usually a service-specific error kind (such as `GalleyError`, `BrigError`, etc...), but kind `*` can also be used.

Note that error types can also be turned into `MultiVerb` responses using the `ErrorResponse` combinator. This is useful for handlers that can return errors as part of their return type, instead of simply throwing them as IO exceptions or using polysemy. If an error is part of `MultiVerb`, there is no need to also report it with `CanThrow`.

### `QualifiedCapture`

This is a capture combinator for a path that looks like `/:domain/:value`, where `value` is of some arbitrary type `a`. The value is returned as a value of type `Qualified a`, which can then be used in federation-aware endpoints.

(named-and-internal-route-ids)=

## `Named`, and internal route IDs in swagger

There is also a combinator `Named` that allows developers to jump back
and forth between the swagger docs (see {ref}`swagger-api-docs`) and
source code: from the swagger docs, copy the *internal route ID* and
full-text-search it in `wire-server/{libs,services}`.  That will give
you both the routing table type and the handler.

Route internal IDs need to instantiate the `Renderable` class in order
to be inserted into the swagger docs.  The instance should satisfy the
property that the ID, as rendered can be copied and fed to grep to
find its occurrances behind `Named`s in the source code.

The initial reason to introduce `Named` was increased type safety: if
two handlers have the same type, they can have be `Named` differently
and thus confusing them will be caught by the type checker.

## Error handling

Several layers of error handling are involved when serving a request. A handler in service code (e.g. Brig or Galley) can:

  1. return a value on the `Right`;
  2. return a value on the `Left`;
  3. throw an `IO` exception.

The `Handler → Servant.Handler` function, together with Servant's internal response creation logic, will, respectively:

  1. produce a normal response;
  2. produce an error response, possibly logging the error;
  3. (ignore any `IO` exceptions, and let them bubble up).

Finally, the error-catching middleware `catchErrors` in `Network.Wai.Utilities.Server` will:

  1. let normal responses through;
  2. depending on the status code:
     - if < 500, let the error through;
     - if >= 500, wrap the error response in a JSON error object (if it is not already
       one), and log it at error level.
  3. catch the exception, turn it into a JSON error object, and log it. The
     log level depends on the status code (error for 5xx, debug otherwise).
