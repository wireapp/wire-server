.. _federation-api:

API
====

The Federation API consists of two *layers*:
  1. Between federators
  2. Between other components


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

* :ref:`Qualified User ID <qualified-user-id>` (QUID): `user_uuid@backend-domain.com`
* :ref:`Qualified User Name <qualified-user-name>` (QUN): `user_name@backend-domain.com`
* :ref:`Qualified Client ID <qualified-client-id>` (QDID) attached to a QUID: `client_uuid.user_uuid@backend-domain.com`
* :ref:`Qualified Conversation <qualified-conversation-id>`/:ref:`Group ID <qualified-group-id>` (QCID/QGID): `backend-domain.com/groups/group_uuid`
* :ref:`Qualified Team ID <qualified-team-id>` (QTID): `backend-domain.com/teams/team_uuid`

While the canonical representation for purposes of visualization is as displayed
above, the API often decomposes the qualified identifiers into an (unqualified)
id and a domain name. In the code and API documentation, we sometimes call a
username a "handle" and a qualified username a "qualified handle".

Besides the above names and identifiers, there are also user :ref:`display names
<display-name>` (sometimes also referred to as "profile names"), which are not
unique on the user's backend, can be changed by the user at any time and are not
qualified.


API between Federators
-----------------------

The layer between federators acts as an envelope for communication between other
components of wire server. It uses Protocol Buffers (protobuf from here onwards)
for serialization over gRPC. The latest protobuf schema can be inspected at
`the wire-server repository
<https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/proto/router.proto>`_.

All gRPC calls are made via a :ref:`mutually authenticated TLS connection
<authentication>` and subject to a :ref:`general <authorization>`, as well as a
:ref:`per-request authorization <per-request-authorization>` step.

The ``Inward`` service defined in the schema is used between federators. It
supports one rpc called ``call`` which requires a ``Request`` and returns an
``InwardResponse``. These objects looks like this:


.. code-block:: protobuf

    message Request {
      Component component = 1;
      bytes path = 2;
      bytes body = 3;
      string originDomain = 4
    }

    enum Component {
      Brig = 0;
      Galley = 1;
    }

    message InwardResponse {
      oneof response {
        bytes body = 1;
        string err = 2;
      }
    }

The ``component`` field in ``Request`` tells the federator which components this
request is meant for and the rest of the arguments are details of the HTTP
request which must be made against the component. It intentionally supports a
restricted set of parameters to ensure that the API is simple.

API From Components to Federator
--------------------------------

Between two federated backends, the components talk to each other via their
respective federators. When making the call to the federator, the components use
protobuf over gRPC. They call the ``Outward`` service, which also supports one
rpc called ``call``. This rpc requires a ``FederatedRequest`` object, which
contains a ``Request`` object as defined above, as well as the domain of the
destination federator. The rpc returns an ``OutwardResponse``, which can either
contains a body with the returned information or an ``OutwardError``, these
objects look like this:

.. code-block:: protobuf

    message FederatedRequest {
      string domain = 1;
      Request request = 2;
    }

    message OutwardResponse {
      oneof response {
        OutwardError err = 1;
        bytes body = 2;
      }
    }

    message OutwardError {
      enum ErrorType {
        RemoteNotFound = 0;
        DiscoveryFailed = 1;
        ConnectionRefused = 2;
        TLSFailure = 3;
        InvalidCertificate = 4;
        VersionMismatch = 5;
        FederationDeniedByRemote = 6;
        FederationDeniedLocally = 7;
        RemoteFederatorError = 8;
        InvalidRequest = 9;
      }

      ErrorType type = 1;
      ErrorPayload payload = 2;
    }

    message ErrorPayload {
      string label = 1;
      string msg = 2;
    }


API From Federator to Components
--------------------------------

The components expose a REST API over HTTP to be consumed by the federator. All
the paths start with ``/federation``. When a federator recieves a request like
this (shown as JSON for convenience):

.. code-block:: json

   {
     "component": "Brig",
     "path": "federation/get-user-by-handle",
     "body": "\"janedoe\"",
     "originDomain": "somedomain.example.com"
   }

The federator connects to Brig and makes an HTTP request which looks like this:

.. code-block::

   > POST /federation/get-user-by-handle
   > Wire-Origin-Domain: somedomain.example.com
   > Content-Type: application/json
   >
   > "janedoe"

The ``/federation`` prefix to the path allows the component to distinguish
federated requests from requests by clients or other local components.

If this request succeeds with any status, the body and response are encoded as
the ``HTTPResponse`` object and returned as a response to the ``Inward.call``
gRPC call.

Note, that before the ``path`` field of the ``Request`` is concatenated with
``/federation`` and used as a component of the HTTP request, its segments are
normalized as described in Section 6.2.2.3 of `RFC 3986
<https://datatracker.ietf.org/doc/html/rfc3986/#section-6.2.2.3>`_ to prevent
path-traversal attacks such as ``/federation/../users/by-handle``.

.. _api-endpoints:

List of Federation APIs exposed by Components
---------------------------------------------

Each component of the backend provides an API towards the federator for access
by other backends. For example on how these APIs are used, see the section on
:ref:`end-to-end flows<end-to-end-flows>`.

.. note:: This reflects status of API endpoints as of 2021-06-25. For latest
          APIs please refer to the corresponding source code linked in the
          individual section.

.. comment: The endpoints and objects are written manually. FUTUREWORK: Automate
   this.

Brig
^^^^

See `the brig source code
<https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/src/Wire/API/Federation/API/Brig.hs>`_
for a list of federated endpoints of the `Brig`, as well as their precise inputs
and outputs. In its current state, the primary purpose of the Brig API is to
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
* ``claim-multi-prekey-bundle``: Given a list of user ids, return the lists of
  their respective clients.
* ``search-users``: Given a term, search the user database for matches w.r.t.
  that term.
* ``get-user-clients``: Given a list of user ids, return the lists of clients of
  each of the users.


Galley
^^^^^^

See `the galley source code
<https://github.com/wireapp/wire-server/blob/master/libs/wire-api-federation/src/Wire/API/Federation/API/Galley.hs>`_
for a list of federated endpoints of the `Galley`, as well as their precise
inputs and outputs. Each backend keeps a record of the conversations that each
of its members is a part of. The purpose of the Galley API is to allow backends
to synchronize the state of the conversations of their members.

* ``register-conversation``: Given a name and a list of conversation members,
  create a conversation locally. This is used to inform another backend of a new
  conversation that involves their local user.
* ``get-conversations``: Given a qualified user id and a list of conversation
  ids, return the details of the conversations. This allows a remote backend to
  query conversation metadata of their local user from this backend. To avoid
  metadata leaks, the backend will check that the domain of the given user
  corresponds to the domain of the backend sending the request.
* ``update-conversation-memberships``: Given a qualified user id and a qualified
  conversation id, update the conversation details locally with the other data
  provided. This is used to alert remote backend of updates in the conversation
  metadata of conversations that one of their local users is involved in.



.. _end-to-end-flows:

End-to-End Flows
----------------

.. _user-discovery:

User Discovery
^^^^^^^^^^^^^^

1. User `A@backend-a.com` enters the qualified user name of the target user
   `B@backend-b.com` into the search field of their Wire client.
2. The client issues a query to ``/search/contacts`` searching for `B` at
   `backend-b.com`.
3. `A`'s backend queries the ``search-users`` endpoint of B's backend for `B`.
4. `B`'s backend replies with with `B`'s user name and qualified handle.
5. `A`'s backend forwards that information to `A`'s client.

Conversation Establishment
^^^^^^^^^^^^^^^^^^^^^^^^^^

1. From the search results of a :ref:`user discovery<user-discovery>` process,
   `A` chooses to create a conversation with `B`.
2. `A`'s client issues a ``/users/backend-b.com/B/prekeys`` query to `A`'s
   backend.
3. `A`'s backend queries the ``claim-prekey-bundle`` endpoint of `B`'s backend
   using `B`'s user id.
4. `B`'s backend replies with a prekey bundle for each of `B`'s clients.
5. `A`'s backend forwards that information to `A`'s client.
6. `A`'s client queries the ``/conversations`` endpoint of its backend
   using `B`'s user id.
7. `A`'s backend creates the conversation locally and queries the
   ``register-conversation`` endpoint of `B`'s backend to inform it about the new
   conversation, including the conversation metadata in the request.
8. `B`'s backend registers the conversation locally and confirms the query.
9. `B`'s backend notifies `B`'s client of the creation of the conversation.
