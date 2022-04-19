.. _federation-api:

API
====

The Federation API consists of two *layers*:
  1. Between two backends (i.e. between a `Federator` and a `Federation
     Ingress`)
  2. Between backend-internal components

.. _qualified-identifiers-and-names:

Qualified Identifiers and Names
-------------------------------

The federated (and consequently distributed) architecture is reflected in the
structure of the various identifiers and names used in the API. Before
federation, identifiers were only unique in the context of a single backend; for
federation, they are made globally unique by combining them with the federation
domain of their backend. We call these combined identifiers *qualified*
identifiers. While other parts of some identifiers or names may change, the
domain name (i.e. the qualifying part) is static.

In particular, we use the following identifiers throughout the API:

* :ref:`Qualified User ID <glossary_qualified-user-id>` (QUID): `user_uuid@backend-domain.com`
* :ref:`Qualified User Name <glossary_qualified-user-name>` (QUN): `user_name@backend-domain.com`
* :ref:`Qualified Client ID <glossary_qualified-client-id>` (QDID) attached to a QUID: `client_uuid.user_uuid@backend-domain.com`
* :ref:`Qualified Conversation <glossary_qualified-conversation-id>`/:ref:`Group ID <glossary_qualified-group-id>` (QCID/QGID): `backend-domain.com/groups/group_uuid`
* :ref:`Qualified Team ID <glossary_qualified-team-id>` (QTID): `backend-domain.com/teams/team_uuid`

While the canonical representation for purposes of visualization is as displayed
above, the API often decomposes the qualified identifiers into an (unqualified)
id and a domain name. In the code and API documentation, we sometimes call a
username a "handle" and a qualified username a "qualified handle".

Besides the above names and identifiers, there are also user :ref:`display names
<glossary_display-name>` (sometimes also referred to as "profile names"), which are not
unique on the user's backend, can be changed by the user at any time and are not
qualified.


API between Federators
-----------------------

The layer between `Federator` acts as an envelope for communication between
other components of wire server. The `Inward` service of `Federator` is an
HTTP2 server which is responsible for accepting external requests coming from
other backends, and forwarding them to the appropriate component (currently
Brig or Galley).


`Federator` inspects the header of an incoming requests, performs discovery and
authentication, as described in :ref:`Backend to backend communication
<backend-to-backend-communication>`, then forwards the request as-is by
repackaging its body into an HTTP request for the target component.

The `Inward` service accepts only ``POST`` requests with a path of the form
``/federation/:component/:rpc``, where `:component` is the lowercase name of
the target component (i.e. ``brig`` or ``galley``), and ``:rpc`` is the name of
the federation RPC to invoke. The arguments of the RPC are contained the body,
which is assumed to be of content type ``application/json``.

See :ref:`below <api-from-federator-to-components>` for more details on RPCs
and their paths.

API From Components to Federator
--------------------------------

Between two federated backends, the components talk to each other via the
`Federator` in the originating domain and `Ingress` in the receiving domain.
When making the call to the `Federator`, the components use HTTP2. They call
the ``Outward`` service, which accepts ``POST`` requests with path
``/rpc/:domain/:component/:rpc``. Such a request will be forwarded to a remote
federator with the given :ref:`Backend domain <Backend-domains>`, and converted
to the appropriate request for its ``Inward`` service.

.. _api-from-federator-to-components:

API From Federator to Components
--------------------------------

The components expose a REST API over HTTP to be consumed by the `Federator`.
All the paths start with ``/federation``. When a `Federator` receives a
``POST`` request to ``/federation/brig/get-user-by-handle``, it connects to a
local Brig and forwards the request to it after changing its path to
``/federation/get-user-by-handle``.

The ``/federation`` prefix is kept in the path to allow the component to
distinguish federated requests from requests by clients or other local
components.

If this request succeeds, the response is directly used as a response for the
original call to the ``Inward`` service. Otherwise, a response with a ``5xx``
status code is returned, with a body containing a description of the error that
has occurred.

Note that the name of the RPC (``get-user-by-handle`` in the above example) is
required to be a single path segment consisting of only ASCII characters within
a restricted set. This prevents path-traversal attacks such as attempting to
access ``/federation/../users/by-handle``.

.. _api-endpoints:

List of Federation APIs exposed by Components
---------------------------------------------

Each component of the backend provides an API towards the `Federator` for access
by other backends. For example on how these APIs are used, see the section on
:ref:`end-to-end flows<end-to-end-flows>`.

.. note:: This reflects status of API endpoints as of 2022-01-28. For latest
          APIs please refer to the corresponding source code linked in the
          individual section.

.. comment: The endpoints and objects are written manually. FUTUREWORK: Automate
   this.

Brig
^^^^

In its current state, the primary purpose of the Brig API is to
allow users of remote backends to create conversations with the local users of
the backend.

* ``get-user-by-handle``: Given a handle, return the user profile
  corresponding to that handle.
* ``get-users-by-ids``: Given a list of user ids, return the list of
  corresponding user profiles.
* ``claim-prekey``: Given a user id and a client id, return a Proteus pre-key
  belonging to that user.
* ``claim-prekey-bundle``: Given a user id, return a prekey for each of the
  user's clients.
* ``claim-multi-prekey-bundle``: Given a list of user ids, return prekeys of
  their respective clients.
* ``search-users``: Given a term, search the user database for matches w.r.t.
  that term.
* ``get-user-clients``: Given a list of user ids, return the lists of clients of
  each of the users.

See `the brig source code
<https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/src/Wire/API/Federation/API/Brig.hs>`_
for the current list of federated endpoints of the `Brig`, as well as their
precise inputs and outputs.

Galley
^^^^^^

Each backend keeps a record of the conversations that each of its members is a
part of. The purpose of the Galley API is to allow backends to synchronize the
state of the conversations of their members.

* ``on-conversation-created``: Given a name and a list of conversation members,
  create a conversation locally. This is used to inform another backend of a new
  conversation that involves their local user(s).  
* ``get-conversations``: Given a qualified user id and a list of conversation
  ids, return the details of the conversations. This allows a remote backend to
  query conversation metadata of their local user from this backend. To avoid
  metadata leaks, the backend will check that the domain of the given user
  corresponds to the domain of the backend sending the request.
* ``on-conversation-updated``: Given a qualified user id and a qualified
  conversation id, update the conversation details locally with the other data
  provided. This is used to alert remote backend of updates in the conversation
  metadata of conversations in which at least one of their local users is involved.
* ``leave-conversation``: Given a remote user and a conversation id, remove the
  the remote user from the (local) conversation.
* ``on-message-sent``: Given a remote message and a conversation id, propagate a message to local users.
  This is used whenever there is a remote user in a conversation (see end-to-end flows).
* ``send-message``: Given a sender and a raw message request, send a message to
  a conversation owned by another backend. This is used when the user sending a
  message is not on the same backend as the conversation the message is sent in.

See `the galley source code
<https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/src/Wire/API/Federation/API/Galley.hs>`_
for the current list of federated endpoints of the `Galley`, as well as their
precise inputs and outputs.

.. _end-to-end-flows:

End-to-End Flows
----------------

In the following end-to-end flows, we focus on the interaction between the Brigs
and Galleys of federated backends. While the interactions are facilitated by the
`Federator` and `Federation Ingress` components of the backends involved, which
handle the necessary discovery, authentication and authorization steps, we won't
mention these steps explicitly each time to keep the flows simple.

Additionally we assume that the backend domain and the infra domain of the
respective backends involved are the same and each domain identifies a distinct
backend.

.. _user-discovery:

User Discovery
^^^^^^^^^^^^^^

In this flow, the user `A` at `backend-a.com` tries to search for user `B` at
`backend-b.com`.

#. User `A@backend-a.com` enters the qualified user name of the target user
   `B@backend-b.com` into the search field of their Wire client.
#. The client issues a query to ``/search/contacts`` of the Brig searching for
   `B` at `backend-b.com`.
#. The Brig in `A`'s backend asks its local `Federator` to query the
   ``search-users`` endpoint of B's backend for `B`.
#. `A`'s `Federator` queries `B`'s Brig via `B`'s `Federation Ingress` and
   `Federator` as requested.
#. `B`'s Brig replies with `B`'s user name and qualified handle, the
   response goes through `B`'s `Federator` and `Federation Ingress`, as well as
   `A`'s `Federator` before it reaches `A`'s Brig.
#. `A`'s Brig forwards that information to `A`'s client.

Conversation Establishment
^^^^^^^^^^^^^^^^^^^^^^^^^^

After having discovered user `B` at `backend-b.com`, user `A` at `backend-a.com`
wants to establish a conversation with `B`.

#. From the search results of a :ref:`user discovery<user-discovery>` process,
   `A` chooses to create a conversation with `B`.
#. `A`'s client issues a ``/users/backend-b.com/B/prekeys`` query to `A`'s
   Brig.
#. `A`'s Brig asks its `Federator` to query the ``claim-prekey-bundle`` endpoint
   of `B`'s backend using `B`'s user id.
#. `B`'s `Federation Ingress` forwards the query to the `Federator`, who in turn forwards it to
   the local Brig.
#. `B`'s Brig replies with a prekey bundle for each of `B`'s clients, which is
   forwarded to `A`'s Brig via `B`'s `Federator` and `Federation Ingress`, as well as `A`'s
   `Federator`.
#. `A`'s Brig forwards that information to `A`'s client.
#. `A`'s client queries the ``/conversations`` endpoint of its Galley
   using `B`'s user id.
#. `A`'s Galley creates the conversation locally and queries the
   ``on-conversation-created`` endpoint of `B`'s Galley (again via its local
   `Federator`, as well as `B`'s `Federation Ingress` and `Federator`) to inform it about the new
   conversation, including the conversation metadata in the request.
#. `B`'s Galley registers the conversation locally and confirms the query.
#. `B`'s Galley notifies `B`'s client of the creation of the conversation.

Message Sending (A)
^^^^^^^^^^^^^^^^^^^

Having established a conversation with user `B` at `backend-b.com`, user `A` at
`backend-a.com` wants to send a message to user `B`.

#. In a conversation `conv-1@backend-a.com` on `A`'s backend with users
   `A@backend-a.com` and `B@backend-b.com`, `A` sends a message by using the
   ``/conversations/backend-a.com/conv-1/proteus/messages`` endpoint
   on `A`'s Galley.
#. `A`'s Galley checks if `A` included all necessary user devices in their
   request. For that it makes a ``get-user-clients`` request to `B`'s Galley.
   `A`'s Galley checks that the returned list of clients matches the list of
   clients the message was encrypted for.
#. `A`'s Galley sends the message to all clients in the conversation that are
   part of `A`'s backend.
#. `A`'s Galley queries the ``on-message-sent`` endpoint on `B`'s Galley via its
   `Federator` and `B`'s `Federation Ingress` and `Federator`.
#. `B`'s Galley will propagate the message to all local clients involved in the
   conversation.

Message Sending (B)
^^^^^^^^^^^^^^^^^^^

Having received a message from user `A` at `backend-a.com`, user `B` at
`backend-b.com` wants send a reply.

#. In a conversation `conv-1@backend-a.com` on `A`'s backend with users
   `A@backend-a.com` and `B@backend-b.com`, `B` sends a message by using the
   ``/conversations/backend-a.com/conv-1/proteus/messages`` endpoint
   on `B`'s backend.
#. `B`'s Galley queries the ``send-message`` endpoint on `A`'s backend.
   *Steps 3-6 below are essentially the same as steps 2-5 in Message Sending (A)*
#. `A`'s Galley checks if `A` included all necessary user devices in their
   request. For that it makes a ``get-user-clients`` request to `B`'s Galley.
   `A`'s Galley checks that the returned list of clients matches the list of
   clients the message was encrypted for.
#. `A`'s Galley sends the message to all clients in the conversation that are
   part of `A`'s backend.
#. `A`'s Galley queries the ``on-message-sent`` endpoint on `B`'s Galley via its
   `Federator` and `B`'s `Federation Ingress` and `Federator`.
#. `B`'s Galley will propagate the message to all local clients involved in the
   conversation.
