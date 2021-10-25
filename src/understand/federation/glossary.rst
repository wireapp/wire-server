.. _glossary:

Federation Glossary
=====================


..
   note to documentation authors:
   until https://github.com/rst2pdf/rst2pdf/issues/898 is fixed we should not use the glossary:: directive and not refer to items with the :term:`text to appear <Backend>` syntax. Instead, we can use explicit section labels and refer to them with :ref:`text to appear <backend>`

.. _glossary_backend:

Backend

   A set of servers, databases and DNS configurations together forming one single Wire Server entity as seen from outside. This set of servers can be owned and administrated by different legal entities in different countries.

   Sometimes also called a Wire "instance" or "server" or "Wire installation".
   Every resource (e.g. users, conversations, assets and teams) exists and is owned by one specific backend, which we can refer to as that resource's backend

.. _glossary_backend_domain:

Backend Domain

   The domain of a backend, which is used to qualify the names and identifiers of
   resources (users, clients, groups, etc) that are local to a given backend.
   See also the :ref:`Consequences of choosing a backend domain <consequences-backend-domain>`

.. _glossary_infra_domain:

Infrastructure Domain or Infra Domain

   The domain under which the :ref:`Federator <glossary_federator>` of a given
   backend is reachable (via that backend's :ref:`Ingress <glossary_federation_ingress>`)
   for other, remote backends.

.. _glossary_federation_ingress:

Federation Ingress

   Federation Ingress is the first point of contact of a given :ref:`backend
   <glossary_backend>` for other, remote backends. It also deals with the
   :ref:`authentication` of incoming requests. See :ref:`here <federation_ingress>` for
   more information.

.. _glossary_federator:

Federator

   The `Federator` is the local point of contact for :ref:`other backend
   components <other-wire-server>` that want to make calls to remote backends.
   It is also the component that deals with the :ref:`authorization` of incoming
   requests from other backends after they have passed the :ref:`Federation Ingress
   <glossary_federation_ingress>`. See :ref:`here <federator>` for more information.

.. _glossary_asset:

Asset

   Any file or image sent via Wire (uploaded to and downloaded from a backend).

.. _glossary_qualified-user-id:

Qualified User Identifier (QUID)

  A combination of a UUID (unique on the user's backend) and a domain.

.. _glossary_qualified-user-name:

Qualified User Name (QUN)

  A combination of a name that is unique on the user's backend and a domain. The
  name is a string consisting of 2-256 characters which are either lower case
  alphanumeric, dashes, underscores or dots. See `here
  <https://github.com/wireapp/wire-server/blob/f683299a03207acb505254ff3121213383d0b672/libs/types-common/src/Data/Handle.hs#L76-L93>`_
  for the code defining the rules for user names. Note that in the wire-server
  source code, user names are called 'Handle' and qualified user names
  'Qualified Handle'.

.. _glossary_qualified-client-id:

Qualified Client Identifier (QDID)

  A combination of a client identifier (a hash of the public key generated for a
  user's client) concatenated with a dot and the QUID of the associated user.

.. _glossary_qualified-group-id:

Qualified Group Identifier (QGID)

  The string `backend-domain.com/groups/` concatenated with a UUID that is
  unique on a given backend.

.. _glossary_qualified-conversation-id:

Qualified Conversation Identifier (QCID)

  The same as a :ref:`QGID <glossary_qualified-group-id>`.

.. _glossary_qualified-team-id:

Qualified Team Identifier (QTID)

  The string `backend-domain.com/teams/` concatenated with a UUID that is
  unique on a given backend.

.. _glossary_display-name:

(User) Profile/Display Name

  The profile/display name of a user is a UTF-8 encoded string with 1-128
  characters.
