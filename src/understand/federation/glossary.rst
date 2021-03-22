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
