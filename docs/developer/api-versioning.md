# Introduction

Since upgrades cannot happen instantaneously, we need to release wire
backends and wire client apps that work together accross releases.

In the past, we have made sure that the api is changed in a way that
the backend can be newer than the client, and then releasing the
change on the backend first.  This worked well on the cloud where we
had control, but fails a lot in the on-prem setting, and will get
worse with federation taking off: if we add a new end-point to the
API, the backend will still be able to handle older clients that
simply don't know about that end-point, but the client won't handle
old backends well, since it will try to call the end-point, and fail.

The new approach outlined here therefore supports API versions.  Every
API version is only compatible with itself, but every node in the
network can support a *set of API versions*.  A HTTP client can query
the set of supported versions from an HTTP server, and then pick one
that works for it.

In the following, we will refer to HTTP clients as "clients", no
matter whether it is a backend talking to another backend
(federation); and to HTTP servers as "server", no matter which API is
it serving (federation or app).  This approach is intended to work for
both.


# Versions and servant routing tables

All routing tables for which a new version is born will be changed
into taking the version number as a parameter, which is added as a
prefix to every route:

```haskell
data Api (version :: Symbol) routes = Api
  { getUnqualifiedConversation ::
      routes
        :- version
        :> Summary "Get a conversation by ID"
        :> ZLocalUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> Get '[Servant.JSON] Conversation,
    getConversation ::
      routes
        :- version
        :> Summary "Get a conversation by ID"
        :> ZLocalUser
        :> "conversations"
        :> QualifiedCapture "cnv" ConvId
        :> Get '[Servant.JSON] Conversation,
    [...]
  }
```

APIs of all the supported versions can be composed like this:

```haskell
type ServantAPI =
  ToServantApi (Api "v1")
    :<|> ToServantApi (Api "v2")
    :<|> ToServantApi (Api "v4") -- v3 is broken
```


## Changes between versions

The point of having versions is of course not that all of them look
exactly the same except for their prefix.  The point is that there are
other things that change between versions.

There are essentially two categories of changes:

1. **data**: request or response bodies, variable path segments,
   possible headers or status codes (as in `UVerb` or `MultiVerb`),
   etc.
2. **structure**: literal path segments, verb, the version itself, ...


## Changes in the data

If a data type in request, response, variable path segments, or anywhere else
changes, introduce a type family parameterized in the version.

```haskell
[...]
    getConversation ::
      routes
        :- version
        :> Summary "Get a conversation by ID"
        :> ZLocalUser
        :> "conversations"
        :> QualifiedCapture "cnv" ConvId
        :> Get '[Servant.JSON] (ConversationV version),
[...]

type family ConversationV (version :: Symbol) :: * where
  ConversationV "v1" = Conversation
  ConversationV "v2" = Conversation
  ConversationV "v4" = ConversationV4
```

Note that before version `"v4"`, nothing changed for this type, so
there was no need to introduce a new concrete data type.

If the last change of a data type is entirely phased out, the type
family turns constant and can be removed again.  If you see this in
your code:

```haskell
type family ConversationV (version :: Symbol) :: * where
  ConversationV "v4" = ConversationV4
  ConversationV "v5" = ConversationV4
  ConversationV "v6" = ConversationV4
  ConversationV "v7" = ConversationV4
```

You can remove 'ConversationV', rename `ConversationV4` to Conversation,
and use it in the routing table instead of `ConversationV` again, as
before `"v4"`.


## Changing structure

Without loss of generality, we only consider additions and deletions
of routes in this section: if you want to change the path or verb of
an end-point, add a new path instead, and phase the old one out (now
or in some future version).

When end-points are present in some supported versions, but not in
others, their record fields in the servant routing type needs to be
present for all versions, but in some versions should behave as if it
weren't.

This is best solved by a new type alias:

```haskell
type NotInThisVersion = Verb 'NOTINTHISVERSION '[] NoContent
```

Then we can write a type family that can crawl a `ServantAPI` (not the
record one, the one with `:<|>`) and drop all the routes marked as not
existing.

This will save us the trouble of writing lots of instances for
`NotInThisVersion` (server, swagger, client, ...), and yield exactly
the desired result:

```haskell
type ServantAPI =
  DropNotInThisVersion
    ( ToServantApi (Api "v1")
        :<|> ToServantApi (Api "v2")
        :<|> ToServantApi (Api "v4") -- v3 is broken
    )
```


## Adoption of versioned APIs

When API versions are introduced to a code base that has a routing
table without versions, the question arises what to do with old
clients or servers talking to new ones.

We define a middleware that

 (1) maps requests without version prefix in the path to ones that
     have version `"v0"`.

 (2) responds with a specific type of error if an unsupported version
     is requested (so the client can re-negotiate a new version to
     speak after an upgrade, see below).


## Version handshake

Client and server need to agree on a version to use.  The server
provides two (kinds of) end-points for that.

```
GET /api-versions
=> { "supported": [1, 2, 45, 119] }

GET /v*/api-docs
=> <swagger for this particular version>
```

The client developer can pull the swagger docs of a new version and
diff it against the one they already support, and work their way
through the changes (see below).

The client will call `GET /api-versions` and pick any number in the
intersection of the versions it supports and the ones the server
supports (usually the largest).

*Corner case:* if we want to distinguish between backend-to-backend
and client-to-backend, we can do that in path suffixes (`GET
/api-versions/{client,federation}` etc.).


### No shared api version

If the intersection set is empty, the client has no way of talking to
the server.  It needs to politely fail with an error message about
upgrading, downgrading, or talking to another server.

This should only happen if the distance between last upgrade on client
and server exceeds the agreed-upon limits (eg., 6 months).


### Update detection and version re-negotation

If the server is upgraded and some old supported versions are phased
out, the client may be caught by surprise.

Servers should respond with status `404`, error label
`unsupported-version`.  The versioning middleware can do that (see
above).

Client should install a catch-all that will handle this specific
error, re-fetch `/api-versions`, and try again.

This will only happen if backend and client grow apart in time beyond
the supported limit, and there is some chance it will result in an
empty set of compatible versions, so it's also ok to just fail here.


## Strongly typed versions

If we make version an ADT `ApiVersion`, we can remove old versions
from it in one place and have the compiler guide us through all the
places where we need to remove it.

There are at least two ways to implement this:

1. Add a few extra servant instances for `(v :: ApiVersion) :> route`.
2. Define a type family `Versions` that maps `V*` to `"v*"`, and write
`Versions version :>` in the routing type instead of `"v1"`.

2 seems a lot less work to write, read, and understand.


## Data migration (aka data marshalling)

If the shape of an end-point changes between versions (if a data type
in the routing table becomes a type family), it is often possible to
write marshalling functions that translate a value from an older
version into one of a newer version or vice versa.

These functions are called marshalling functions and are useful to
define separately to keep the application logic clean.

For certain changes to a data type used in an API, marshalling is
straight-forward in both directions.  The most common example is
adding an optional attribute to a JSON object:

- *backward migration*: remove the new attribute.
- *forward migration*: set the new attribute to `null`.

(This is what wire has traditionally done to accomplish client
backwards compatibility without any API versioning.)

If a mandatory attribute is added in a newer version, there may be a
plausible default value that can be used in the forward migration
(backward migration would still remove the field).

In other cases, whether there is an automatic migration depends on the
use case and the semantics.

It may even be impossible to marshal either in one or in both
directions.  In this case, you have 3 options:

1. abandon compatibility;
2. rethink your new version and craft it in a way that two-way
   marshalling is possible;
3. make the application work around the gap, eg. by gracefully
   refusing to offer video conferencing in a client if it is not
   supported on the server yet.


## Writing client code

If you write all code by hand and don't generate anything from the
swagger docs, just look at the swagger diff for every new version and
take it from there.

If you generate, say, typescript or kotlin or swift from swagger:

0. have a generated source module `Gen.ts`, plus a source module with
   manually written code `Man.ts`.  Re-export everything from `Gen.ts`
   in `Man.ts`, and only import `Man.ts` in any modules that contain
   application logic.

1. look at the diff of the swagger of the last supported and the new
   versions.

2. copy all functions for routes that have changed from `Gen.ts` to
   `Man.ts`.  these are speaking an old api version and won't need to
   be re-generated any more.  work the last version supported by this
   function into the name somehow (eg., suffix `"_v23"`).

3. for every function that moved to `Man.ts` in this way, write a
   function *without* the version suffix.  It somehow knows the api
   version of the server that it talks to (function parameter, app
   config, dosn't matter), and decides based on that whether to call
   the deprecated function with the `"_v23"` suffix or the one from
   `Gen.ts`.  If the old one is called, it may have to do some
   marshalling of request and response (see above).

It will happen that a new client will not be able to accomplish
something with an old API (example: if video calling is introduced in
`"v12"`, you can't emulate `POST /video-call` when talking to a `"v9"`
server.  In these cases, the function in `Man.ts` must raise a "server
too old" exception, and gracefully shut down the new functionality.


## Concerns and design alternatives

### Why not version every end-point separately?

Yes, that would work in principle.  On the backend, it would make the
entire routing table smaller (no need to concatenate the same
end-point for many versions), which may result in shorter compile
times.  On the clients, with a new API version it would be
straight-forward to see which end-points need to be worked on, and
which remain unchanged.

On the other hand, the routing table size may not be an issue, and if
it is there are solutions (introduce a CCP switch to compile only the
most recent API version that you're working on); and the client
process is already quite straight-forward with the approach outlined
above via diffing the swagger docs between most recent version and
predecessor.

Plus, if the entire API has one version, you get a few advantages:

1. The fact that clients are forced to commit to a concrete API
   version for all end-points when talking to the backend reduces
   testing complexity.  If there is a mapping of end-points to
   versions, the behavior of interacting parties is much less
   restricted, and versions that have not been tested against each
   other may be used together.  (This can be avoided, but it's less
   obvious how to get it right, and testing complexity will likely be
   worse.)

2. The "one version" approach makes it obvious which end-points are in
   the most recent API at any given point in time.  The "one version
   per end-point" approach would either yeild a noisy union of all
   supported versions, or there would have to be a mechanism for
   reconstructing something close to what we get for free otherwise.

3. The backend code is a good combinatin of concise and type-safe in
   the "one version" approach.  If every end-point had its own
   version, the routing table entry would either have to accept a
   variable path segment for the version, and fail at run-time if the
   version is not supported, or you would have to add one handler per
   supported version (even if in the case where all versions call the
   same handler function with slightly different parameters).


### Syntactical vs. behavioral changes

It is quite common that behavior of end-points changes together with
the syntax, or even without a change in the syntax.

This is not a fundamental problem: since the handler can be called
with the version as a type parameter, there is no reason why it
shouldn't change behavior with or without changing the syntax.  In
each such case, it needs to be decided whether the difference is
significant enough to justify a new API version.

At the very least though it should result in diverging swagger docs
that explains those differences.


### Client capabilities

Wire supports client capabilities to decide whether a client should be
allowed to use certain parts of the API.

This is another alternative to API versions, and it is in some ways
more straight-forward to decide who to interpret capability sets.  But
this approach has its own problems: Most importantly, the number of
supported capability sets grows quadratically (not in practice,
because historically clients will only ever support a small part of
all possible combinations of capabilities, but that makes thigns
worse: it makes the system more complex, and then doesn't use that
complexity for anything).

Therefore, the capabilities we're using in the wire code base should
be gracefully phased out and replaced by API versions.
