# Introduction

We currently use Servant for the public (i.e. client-facing) API in brig, galley and spar, as well as for their federation (i.e. server-to-server) and internal API.

Client-facing APIs are defined in `Wire.API.Routes.Public.{Brig,Galley}`. Internal APIs are all over the place at the moment. Federation APIs are in `Wire.API.Federation.API.{Brig,Galley}`.

Our APIs are able to generate Swagger documentation semi-automatically using `servant-swagger2`. The `schema-profunctor` library (see README in libs/schema-profunctor) is used to create "schemas" for the input and output types used in the Servant APIs. A schema contains all the information needed to serialise/deserialise JSON values, as well as the documentation and metadata needed to generate Swagger.

# Combinators

We have employed a few custom combinators to try to keep HTTP concerns and vocabulary out of the API handlers that actually implement the functionality of the API.

## `ZAuth`

This is a family of combinators to handle the headers that nginx adds to requests. We currently have:

  - `ZUser`: extracts the `UserId` in the `Z-User` header.
  - `ZLocalUser`: same as `ZUser`, but as a `Local` object (i.e. qualified by the local domain); this is useful when writing federation-aware handlers.
  - `ZConn`: extracts the `ConnId` in the `Z-Connection` header.

## `MultiVerb`

This is an alternative to `UVerb`, designed to prevent any HTTP-specific information from leaking into the type of the handler. Use this for endpoints that can return multiple responses.

## `CanThrow`

This can be used to add an error response to the Swagger documentation. It currently does nothing to the Servant API itself, but this might change in the future, as polysemy is introduced in more services and integrated with Servant. The argument of `CanThrow` can be an `ErrorDescription` type, which is a type-level representation of a possible error.

Note thtat `ErrorDescription` types can also be used directly as `MultiVerb` responses. This is useful for handlers that can return errors as part of their return type, instead of simply throwing them as IO exceptions. If an error is part of `MultiVerb`, there is no need to also report it with `CanThrow`.

## `QualifiedCapture`

This is a capture combinator for a path that looks like `/:domain/:value`, where `value` is of some arbitrary type `a`. The value is returned as a value of type `Qualified a`, which can then be used in federation-aware endpoints.
