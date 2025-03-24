(federation-api)=

# Federation API

(qualified-identifiers-and-names)=
## Qualified Identifiers and Names

The federated architecture is reflected in the structure of the various
identifiers and names used in the API. Identifiers, such as user ids, are unique
within the context of a backend. They are made unique within the context of all
federating backend by combining them with the {ref}`backend domain
<glossary_backend_domain>`.

For example a user with user id `d389b370-5f7d-4efd-9f9a-8d525540ad93` on
backend `b.example.com` has the *qualified user id*
`d389b370-5f7d-4efd-9f9a-8d525540ad93@b.example.com`. In API request bodies
qualified identities are encoded as objects, e.g.

```
{
  "user": {
      "id": "d389b370-5f7d-4efd-9f9a-8d525540ad93",
      "domain": "b.example.com"
  }
  ...
}

```
In API path segments qualified identities are encoded with the domain first, e.g.
```
POST /connections/b.example.com/d389b370-5f7d-4efd-9f9a-8d525540ad93
```
to send a connection request to a user.

Any identifier on a backend can be qualified:

- conversation ids 
- team ids
- client ids
- user ids
- user handles, e.g. local handle `@alice` is displayed as `@alice@b.example.com` in federating users' devices

User profile names (e.g. "Alice") which are not unique on the user\'s backend,
can be changed by the user at any time and are not qualified.

(api-between-federators)=

## Federated requests

Every federated API request is made by a service component (e.g. brig, galley,
cargohold) in one backend and responded to by a service component in the other
backend. The *Federators* of the backends are relaying the request between the
components across backends . The components talk to each other via the
*Federator* in the originating domain and *Federator Ingress* in the receiving
domain (for details see {ref}`backend-to-backend-communication`).


```{figure} ./img/federation-apis-flow.png
---
width: 100%
---
Federators relaying a request between components. See {ref}`federation-back2back-example` to see the discovery, authentication and authorization steps that are omitted from this figure.
```

### API From Components to Federator


When making the call to the *Federator*, the components use HTTP2. They call the
Federator's `Outward` service, which accepts `POST` requests with path
`/rpc/:domain/:component/:rpc`. Such a request will be forwarded to the remote
Federator with the given {ref}`backend domain<Backend-domains>`, and converted
to the appropriate request of its `Inward` service.

### API between Federators

The layer between *Federator* acts as an envelope for communication
between other components of wire server. The *Inward* service of
*Federator* is an HTTP2 server which is responsible for accepting
external requests coming from other backends, and forwarding them to the
appropriate component (currently Brig or Galley).

*Federator* inspects the header of an incoming requests, performs
discovery and authentication, as described in
{ref}`Backend to backend communication
<backend-to-backend-communication>`, then
forwards the request as-is by repackaging its body into an HTTP request
for the target component.

The *Inward* service accepts only `POST` requests with a path of the
form `/federation/:component/:rpc`, where `:component` is the lowercase
name of the target component (i.e. `brig` or `galley`), and `:rpc` is
the name of the federation RPC to invoke. The arguments of the RPC are
contained the body, which is assumed to be of content type
`application/json`.

See {ref}`api-from-federator-to-components` for more details on RPCs and their paths.

(api-from-components-to-federator)=


(api-from-federator-to-components)=

### API From Federator to Components

The components expose a REST API over HTTP to be consumed by the
*Federator*. All the paths start with `/federation`. When a *Federator*
receives a `POST` request to `/federation/brig/get-user-by-handle`, it
connects to a local Brig and forwards the request to it after changing
its path to `/federation/get-user-by-handle`.

The `/federation` prefix is kept in the path to allow the component to
distinguish federated requests from requests by clients or other local
components.

If this request succeeds, the response is directly used as a response
for the original call to the `Inward` service. Otherwise, a response
with a `5xx` status code is returned, with a body containing a
description of the error that has occurred.

Note that the name of the RPC (`get-user-by-handle` in the above
example) is required to be a single path segment consisting of only
ASCII characters within a restricted set. This prevents path-traversal
attacks such as attempting to access `/federation/../users/by-handle`.

(api-endpoints)=

## List of Federation APIs exposed by Components

Each component of the backend provides an API towards the *Federator*
for access by other backends. 

```{note}
This reflects status of API endpoints as of 2023-01-10. For latest APIs please
refer to the corresponding source code linked in the individual section.
```

(brig)=

### Brig

In its current state, the primary purpose of the Brig API is to allow
users of remote backends to create conversations with the local users of
the backend.

-   `get-user-by-handle`: Given a handle, return the user profile
    corresponding to that handle.
-   `get-users-by-ids`: Given a list of user ids, return the list of
    corresponding user profiles.
-   `claim-prekey`: Given a user id and a client id, return a Proteus
    pre-key belonging to that user.
-   `claim-prekey-bundle`: Given a user id, return a prekey for each of
    the user\'s clients.
-   `claim-multi-prekey-bundle`: Given a list of user ids, return
    prekeys of their respective clients.
-   `search-users`: Given a term, search the user database for matches
    w.r.t. that term.
-   `get-user-clients`: Given a list of user ids, return the lists of
    clients of each of the users.
-   `get-user-clients`: Given a list of user ids, return a list of all their clients with public information
-   `send-connection-action`: Make and also respond to user connection requests
-   `on-user-deleted-connections`: Notify users that are connected to remote user about that user's deletion
-   `get-mls-clients`: Request all {ref}`MLS <mls-message-layer-security>`-capable clients for a given user
-   `claim-key-packages`: Claim a previously-uploaded KeyPackage of a remote user. User for adding users to MLS conversations.

See [the brig source
code](https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/src/Wire/API/Federation/API/Brig.hs)
for the current list of federated endpoints of *Brig*, as well as
their precise inputs and outputs.

(galley)=

### Galley

Each backend keeps a record of the conversations that each of its
members is a part of. The purpose of the Galley API is to allow backends
to synchronize the state of the conversations of their members.

- `get-conversations`: Given a qualified user id and a list of
    conversation ids, return the details of the conversations. This
    allows a remote backend to query conversation metadata of their
    local user from this backend. To avoid metadata leaks, the backend
    will check that the domain of the given user corresponds to the
    domain of the backend sending the request.
- `get-sub-conversation`: Get a MLS subconversation
- `leave-conversation`: Given a remote user and a conversation id,
    remove the the remote user from the (local) conversation.
- `mls-welcome`: Send MLS welcome message to a new user owned by the called backend
- `on-client-removed`: Inform called backend that a client of a user has been deleted
- `on-conversation-created`: Given a name and a list of conversation
    members, create a conversation locally. This is used to inform
    another backend of a new conversation that involves their local
    user(s).
- `on-conversation-updated`: Given a qualified user id and a qualified
    conversation id, update the conversation details locally with the
    other data provided. This is used to alert remote backend of updates
    in the conversation metadata of conversations in which at least one
    of their local users is involved.
- `on-message-sent`: Given a remote message and a conversation id,
    propagate a message to local users. This is used whenever there is a
    remote user in a conversation (see end-to-end flows).
- `on-mls-message-sent`: Receive a MLS message that originates in the calling backend
- `on-new-remote-conversation`: Inform the called backend about a conversation that exists on the calling backend. This request is made before the first time the backend might learn about this conversation, e.g. when its first user is added to the conversation.
- `update-typing-indicator`: Used by the calling backend (that does not own the conversation ) to inform the backend about a change of the typing indicator status of one of its users
- `on-typing-indicator-updated`: Used by the calling backend (that owns a conversation) to inform the called backend about a change of the typing indicator status of remote user
- `on-user-deleted-conversations`: When a user on calling backend this request is made for all conversations on the called backend was part of
- `query-group-info`: Query the MLS public group state
- `send-message`: Given a sender and a raw message request, send a
    message to a conversation owned by another backend. This is used
    when the user sending a message is not on the same backend as the
    conversation the message is sent in.
- `send-mls-commit-bundle`: Send a MLS commit bundle to backend that owns the conversation
- `send-mls-message`: Send MLS message to backend that owns the conversation
- `update-conversation`: Calling backend requests a conversation action on the called backend which owns the conversation

See [the galley source
code](https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/src/Wire/API/Federation/API/Galley.hs)
for the current list of federated endpoints of *Galley*, as well as
their precise inputs and outputs.

(end-to-end-flows)=

### Cargohold
- `get-asset`: Check if asset owned by called backend is available to calling backend
- `stream-asset`: Stream asset owned by the called backend

See [the cargohold source
code](https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/src/Wire/API/Federation/API/Cargohold.hs)
for the current list of federated endpoints of the *Cargohold*, as well as
their precise inputs and outputs.

## Example End-to-End Flows

In the following the interactions between *Federator* and *Federation Ingress*
components of the backends involved are omitted for simplicity. Also the backend
domain and infrastructure domain are assumed the same.

Additionally we assume that the backend domain and the infrastructure domain of
the respective backends involved are the same and each domain identifies
a distinct backend.

(user-discovery)=

### User Discovery

In this flow, the user *Alice* at *a.example.com* tries to search for user
*Bob* at *b.example.com*.

1.  User *Alice* enters the qualified user name of the target
    user *Bob* : `@bob@b.example.com` into the search field of their Wire client.
2.  The client issues a query to `/search/contacts` of the Brig
    searching for *Bob* at *b.example.com*.
3.  The Brig in *Alice*\'s backend asks its local *Federator* to query the
    `search-users` endpoint in *Bob*\'s backend.
4.  *Alice*\'s *Federator* queries *Bob*\'s Brig via *Bob*\'s *Federation
    Ingress* and *Federator* as requested.
5.  *Bob*\'s Brig replies with *Bob*\'s user name and qualified handle, the
    response goes through *Bob*\'s *Federator* and *Federation Ingress*,
    as well as *Alice*\'s *Federator* before it reaches *A*\'s Brig.
6.  *Alice*\'s Brig forwards that information to *A*\'s client.

(conversation-establishment)=

### Conversation Establishment

After having discovered user *Bob* at *b.example.com*, user *Alice* at
*a.example.com* wants to establish a conversation with *Bob*.

1.  From the search results of a
    {ref}`user discovery<user-discovery>`
    process, *Alice* chooses to create a conversation with *Bob*.
2.  *Alice*\'s client issues a `/users/b.example.com/<bobs-user-id>/prekeys` query to
    *Alice*\'s Brig.
3.  *Alice*\'s Brig asks its *Federator* to query the `claim-prekey-bundle`
    endpoint of *Bob*\'s backend using *Bob*\'s user id.
4.  *Bob*\'s *Federation Ingress* forwards the query to the *Federator*,
    who in turn forwards it to the local Brig.
5.  *Bob*\'s Brig replies with a prekey bundle for each of *Bob*\'s clients,
    which is forwarded to *Alice*\'s Brig via *Bob*\'s *Federator* and
    *Federation Ingress*, as well as *Alice*\'s *Federator*.
6.  *Alice*\'s Brig forwards that information to *A*\'s client.
7.  *Alice*\'s client queries the `/conversations` endpoint of its Galley
    using *Bob*\'s user id.
8.  *Alice*\'s Galley creates the conversation locally and queries the
    `on-conversation-created` endpoint of *Bob*\'s Galley (again via its
    local *Federator*, as well as *Bob*\'s *Federation Ingress* and
    *Federator*) to inform it about the new conversation, including the
    conversation metadata in the request.
9.  *Bob*\'s Galley registers the conversation locally and confirms the
    query.
10. *Bob*\'s Galley notifies *Bob*\'s client of the creation of the
    conversation.

(message-sending-a)=

### Message Sending

Having established a conversation with user *Bob* at *b.example.com*, user
*Alice* at *a.example.com* wants to send a message to user *Bob*.

1.  In a conversation *\<conv-id-1\>@a.example.com* on *Alice*\'s backend with
    users *Alice* and *Bob*, *Alice* sends a message
    by using the `/conversations/a.example.com/<conv-id-1>/proteus/messages`
    endpoint on *Alice*\'s Galley.
2.  *Alice*\'s Galley checks if *A* included all necessary user devices in
    their request. For that it makes a `get-user-clients` request to
    *Bob*\'s Galley. *Alice*\'s Galley checks that the returned list of
    clients matches the list of clients the message was encrypted for.
3.  *Alice*\'s Galley sends the message to all clients in the conversation
    that are part of *Alice*\'s backend.
4.  *Alice*\'s Galley queries the `on-message-sent` endpoint on *Bob*\'s
    Galley via its *Federator* and *Bob*\'s *Federation Ingress* and
    *Federator*.
5.  *Bob*\'s Galley will propagate the message to all local clients
    involved in the conversation.

## Ownership

Wire uses the concept of **ownership** as a guiding principle in the design of
Federation. Every resource, e.g. user, conversation, asset, is **owned** by the
backend on which it was *created*.

A backend that owns a resource is the source of truth for it. For example, for
users this means that information about user *Alice* which is owned by backend
*A* is stored only on backend *A*. If any federating backend needs information
about the user *Alice*, e.g. the profile information, it needs to request that
information from *A*.

In some cases backends locally store partial information of resources they don't
own. For example a backend stores a reference to any remotely-owned conversation
any of its users is participating in. However, to get the full list of all
participants of a remote conversation, the owning backend needs to be queried.

Ownership is reflected in the naming convention of federation RPCs. Any rpc
named with prefix `on-` is always invoked by the backend that owns the resource
to inform federating backends. For example, if a user leaves a remote
conversation its backend would call the `leave-conversation` rpc on the remote
conversation. The remote backend would remove the user and inform all other
federating backends that participate in that conversation of this change by
calling their `on-conversation-updated` rpc.
