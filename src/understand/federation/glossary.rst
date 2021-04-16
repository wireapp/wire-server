.. _glossary:

Federation Glossary
=====================


..
   note to documentation authors:
   until https://github.com/rst2pdf/rst2pdf/issues/898 is fixed we should not use the glossary:: directive and not refer to items with the :term:`text to appear <Backend>` syntax. Instead, we can use explicit section labels and refer to them with :ref:`text to appear <backend>`

.. _backend:

Backend

   A set of servers, databases and DNS configurations together forming one single Wire Server entity as seen from outside. This set of servers can be owned and administrated by different legal entities in different countries.

   Sometimes also called a Wire "instance" or "server" or "Wire installation".
   Every resource (e.g. users, conversations, assets and teams) exists and is owned by one specific backend, which we can refer to as that resource's backend


.. _asset:

Asset

   Any file or image sent via Wire (uploaded to and downloaded from a backend).

.. _qualified-user-id:

Qualified User Identifier (QUID)

  A combination of a UUID (unique on the user's backend) and a domain.

.. _qualified-user-name:

Qualified User Name (QUN)

  A combination of a name that is unique on the user's backend and a domain. The
  name is a string consisting of 2-256 characters which are either lower case
  alphanumeric, dashes, underscores or dots. See `here
  <https://github.com/wireapp/wire-server/blob/f683299a03207acb505254ff3121213383d0b672/libs/types-common/src/Data/Handle.hs#L76-L93>`_
  for the code defining the rules for user names. Note that in the wire-server
  source code, user names are called 'Handle' and qualified user names
  'Qualified Handle'.

.. _qualified-device-id:

Qualified Device Identifier (QDID)

  A combination of a device identifier (a hash of the public key generated for a
  user's device) concatenated with a dot and the QUID of the associated user.

.. _qualified-group-id:

Qualified Group Identifier (QGID)

  The string `backend-domain.com/groups/` concatenated with a UUID that is
  unique on a given backend.

.. _qualified-conversation-id:

Qualified Conversation Identifier (QCID)

  The same as a :ref:`QGID <qualified-group-id>`.

.. _qualified-team-id:

Qualified Team Identifier (QTID)

  The string `backend-domain.com/teams/` concatenated with a UUID that is
  unique on a given backend.

.. _display-name:

(User) Profile/Display Name

  The profile/display name of a user is a UTF-8 encoded string with 1-128
  characters.
