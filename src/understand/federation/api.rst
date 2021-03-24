.. _federation-api:

API
====

The Federation API consists of two *layers*:
  1. Between federators
  2. Between other components


API between Federators
-----------------------

The layer between federators acts as an envelope for communication between other
components of wire server. It uses Protocol Buffers (protobuf from here onwards)
for serialization over gRPC. The latest protobuf schema can be downloaded from
:download:`the wire-server repository
<https://raw.githubusercontent.com/wireapp/wire-server/master/libs/wire-api-federation/proto/router.proto>`.

The ``Inward`` service defined in the schema is used between federators. It
supports one rpc called ``call`` which requires a ``Request`` and returns an
``InwardResponse``. These objects looks like this:


.. code-block:: protobuf

    message Request {
      Component component = 1;
      Method method = 2;
      bytes path = 3;
      repeated QueryParam query = 4;
      bytes body = 5;
    }

    message QueryParam {
      bytes key = 1;
      bytes value = 2;
    }

    enum Method {
      GET = 0;
      POST = 1;
      HEAD = 2;
      PUT = 3;
      DELETE = 4;
      TRACE = 5;
      CONNECT = 6;
      OPTIONS = 7;
      PATCH = 8;
    }

    message HTTPResponse {
        uint32 responseStatus = 1;
        bytes responseBody = 2;
    }

    message InwardResponse {
      oneof response {
        HTTPResponse httpResponse = 1;
        string err = 2;
      }
    }

The ``component`` field in ``Request`` tells the federator which components this
request is meant for and the rest of the arguments are details of the HTTP
request which must be made against the component. It intentionally supports a
restricted set of parameters to ensure that the API is simple.

The ``HTTPResponse`` object also intentionally restricts the response to status
and body to ensure the API is simple and we do not leak headers across backends.
The body must always be considered as json encoded without any compression.

API From Components to Federator
--------------------------------

Between two federated backends, the components talk to each other via their
respective federators. When making the call to the federator, the components use
protobuf over gRPC. They call the ``Outward`` service, which also supports one
rpc called ``call``. This rpc requires the same ``Request`` object defined above
and returns an ``OutwardResponse``. The ``OutwardResponse`` can either contain
an ``HTTPResponse`` or an ``OutwardError``, these objects look like this:

.. code-block:: protobuf

   message OutwardResponse {
     oneof response {
       HTTPResponse httpResponse = 1;
       OutwardError err = 2;
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

   message HTTPResponse {
       uint32 responseStatus = 1;
       bytes responseBody = 2;
   }


API From Federator to Components
--------------------------------

The components expose a REST API over HTTP to be consumed by the federator. All
the paths start with ``/federation``. When a federator recieves a request like
this (shown as JSON for convenience):

.. code-block:: json

   {
     "component": "Brig",
     "method": "GET",
     "path": "/users/by-handle",
     "query": [ { "key": "handle", "value": "janedoe" } ],
     "body": null
   }

The federator connects to brig and makes an HTTP request which looks like this:

.. code-block::

   GET /federation/users/by-handle?handle=janedoe

If this request succeeds with any status, the body and response are encoded as
the ``HTTPResponse`` object and returned as a response to the ``Inward.call``
gRPC call.

List of Federation APIs exposed by Components
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. note:: This reflects status of API endpoints as of 2021-03-24. For latest
          APIs please refer to the :download:`wire-api-federation
          package<https://github.com/wireapp/wire-server/blob/develop/libs/wire-api-federation/src/Wire/API/Federation/API/Brig.hs>`

.. comment: The endpoints and objects are written manually. FUTUREWORK: Automate
   this.

Brig
~~~~

Endpoints
++++++++++

+------------------+---------+------------------+--------------+--------------+---------------+
| Name             | Method  | Path             | Query Params | Request Body | Response Body |
+==================+=========+==================+==============+==============+===============+
| Get user profile |         |                  |              |              |               |
| by handle        | GET     | /users/by-handle | handle       |              |  UserProfile  |
+------------------+---------+------------------+--------------+--------------+---------------+


Objects
+++++++

UserProfile
  +---------------+-------------+----------+-----------------------+
  | Field         | Type        | Required | Remarks               |
  +===============+=============+==========+=======================+
  | qualified_id  | QualifiedId | True     |                       |
  +---------------+-------------+----------+-----------------------+
  | name          | String      | True     |                       |
  +---------------+-------------+----------+-----------------------+
  | picture       | [JSON Value]| False    | Deprecated            |
  +---------------+-------------+----------+-----------------------+
  | assets        | [Asset]     | True     | Could be empty        |
  +---------------+-------------+----------+-----------------------+
  | accent_id     | Integer     | True     |                       |
  +---------------+-------------+----------+-----------------------+
  | deleted       | Boolean     | False    |                       |
  +---------------+-------------+----------+-----------------------+
  | service       |             | False    | Only present for bots |
  +---------------+-------------+----------+-----------------------+
  | handle        | String      | False    |                       |
  +---------------+-------------+----------+-----------------------+
  | locale        | String      | False    |                       |
  +---------------+-------------+----------+-----------------------+
  | expires_at    | UTCTime     | False    | encoded as            |
  |               |             |          | 2016-07-22T00:00:00Z  |
  +---------------+-------------+----------+-----------------------+
  | team          | UUID        | False    |                       |
  +---------------+-------------+----------+-----------------------+
  | email         | String      | False    |                       |
  +---------------+-------------+----------+-----------------------+
  | id            | UUID        | False    | deprecated,           |
  |               |             |          | use qualified_id      |
  +---------------+-------------+----------+-----------------------+

QualifiedId
  +---------------+-------------+----------+-----------------------+
  | Field         | Type        | Required | Remarks               |
  +===============+=============+==========+=======================+
  | id            | UUID        | True     |                       |
  +---------------+-------------+----------+-----------------------+
  | domain        | String      | True     |                       |
  +---------------+-------------+----------+-----------------------+

Asset:
  +---------------+-------------+----------+-----------------------+
  | Field         | Type        | Required | Remarks               |
  +===============+=============+==========+=======================+
  | key           | String      | True     |                       |
  +---------------+-------------+----------+-----------------------+
  | size          | "complete"  | True     |                       |
  |               | or "preview"|          |                       |
  +---------------+-------------+----------+-----------------------+
  | type          | "image"     | True     |                       |
  +---------------+-------------+----------+-----------------------+

Galley
~~~~~~

None yet.

Cargohold
~~~~~~~~~

None yet.
