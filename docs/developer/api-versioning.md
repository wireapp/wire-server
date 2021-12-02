# Introduction

Since upgrades cannot happen instantaneously, we need release wire
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


### No shared api version

If the intersection set is empty, the client has no way of talking to
the server.  It needs to politely fail with an error message about
upgrading, downgrading, or talking to another server.

This should only happen if the distance between last upgrade on client
and server exceeds the agreed-upon limits (eg., 6 months).


### Update detection and version re-negotation

If the server is upgraded and some old supported versions are phased
out, the client may be caught by surprise.

The clients should therefore frequently (every 24h?) refetch
/api-versions and possibly raise warnings if the intersection set gets
too small.

The client could also avoid that extra work and instead have a
catch-all that will handle the specific response from the versioning
middleware (see above), re-fetch `/api-versions` and try again.


## Strongly typed versions

If we make version an ADT `ApiVersion`, we can remove old versions
from it in one place and have the compiler guide us through all the
places where we need to remove it.

There are at least two ways to implement this:

1. Add a few extra servant instances for `(v :: ApiVersion) :> route`.
2. Define a type family `Versions` that maps `V*` to `"v*"`, and write
`Versions version :>` in the routing type instead of `"v1"`.

2 seems a lot less work to write, read, and understand.


## Data migration (aka request and response marshalling)

TODO

you want to migrate in two directions (why?)

and you can!!  (can you?)


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


## Design Alternatives and Concerns

TODO:
```
separate version for each end-point?
  - not so important for clients that hand-code everything:
    => if clients write code manually, they only have to adjust the
    end-points that show up in the swagger diff.
  - for generation: interesting problem.  maybe this:
     - generate new client
     - look at the git diff
     - take everything that's different, and handle it outside the
       generated code.  take the code generated for old versions and
       move it somewhere where it can't be re-generated (doesn't have
       to).  the end?
```

TODO: what if the behavior changes?  => then either you can implement
the new behavior in terms of the old one, or you can't offer the
feature (yet!) in this version combination.  if server is old and
client is new, it needs to hide new features.

TODO: client capabilities: we have those, but they are quadratic in
complexity, and it's hard to uncleaer how to phase out old behavior.
