Notes for developers
====================

What you need to know as a user of the Wire backend: concepts, features,
and API. We want to keep these up to date. They could benefit from some
re-ordering, and they are far from complete, but we hope they will still
help you.

.. toctree::
   :maxdepth: 1
   :caption: Contents:
   :glob:

   developer/api-versioning.md <./developer/api-versioning.md>
   developer/cassandra-interaction.md <./developer/cassandra-interaction.md>
   developer/changelog.md <./developer/changelog.md>
   developer/dependencies.md <./developer/dependencies.md>
   developer/editor-setup.md <./developer/editor-setup.md>
   developer/features.md <./developer/features.md>
   developer/federation-api-conventions.md <./developer/federation-api-conventions.md>
   developer/how-to.md <./developer/how-to.md>
   developer/linting.md <./developer/linting.md>
   developer/processes.md <./developer/processes.md>
   developer/scim/storage.md <./developer/scim/storage.md>
   developer/servant.md <./developer/servant.md>
   developer/testing.md <./developer/testing.md>
   reference/config-options.md <./reference/config-options.md>
   reference/conversation.md <./reference/conversation.md>
   reference/elastic-search.md <./reference/elastic-search.md>
   reference/elasticsearch-migration-2021-02-16.md <./reference/elasticsearch-migration-2021-02-16.md>
   reference/make-docker-and-qemu.md <./reference/make-docker-and-qemu.md>
   reference/provisioning/scim-token.md <./reference/provisioning/scim-token.md>
   reference/provisioning/scim-via-curl.md <./reference/provisioning/scim-via-curl.md>
   reference/provisioning/wire_scim_token.md <./reference/provisioning/wire_scim_token.md>
   reference/spar-braindump.md <./reference/spar-braindump.md>
   reference/team/legalhold.md <./reference/team/legalhold.md>
   reference/user/activation.md <./reference/user/activation.md>
   reference/user/connection.md <./reference/user/connection.md>
   reference/user/registration.md <./reference/user/registration.md>
   reference/user/rich-info.md <./reference/user/rich-info.md>

Users
-----

User lifecycle:

-  `User registration <reference/user/registration.md>`__
   ``{#RefRegistration}``
-  `User activation <reference/user/activation.md>`__
   ``{#RefActivation}``

User profiles and metadata:

-  `Connections between users <reference/user/connection.md>`__
   ``{#RefConnection}``
-  `Rich info <reference/user/rich-info.md>`__ ``{#RefRichInfo}``

SCIM provisioning
-----------------

We have support for provisioning users via SCIM (`RFC
7664 <https://tools.ietf.org/html/rfc7664>`__, `RFC
7643 <https://tools.ietf.org/html/rfc7643>`__). It’s in the beta stage.

-  `Using the SCIM API with
   curl <reference/provisioning/scim-via-curl.md>`__
   ``{#RefScimViaCurl}``
-  `Authentication via SCIM
   tokens <reference/provisioning/scim-token.md>`__ ``{#RefScimToken}``

Hints
-----

Internal documentation detailing what you need to know as a Wire backend
developer. All of these documents can and should be referenced in the
code.

If you’re not a member of the Wire backend team, you might still find
these documents useful, but keep in mind that they are a work in
progress.

-  `Development setup <developer/dependencies.md>`__ ``{#DevDeps}``
-  `Editor setup <developer/editor-setup.md>`__ ``{#DevEditor}``
-  `Storing SCIM-related data <developer/scim/storage.md>`__
   ``{#DevScimStorage}``

Cassandra
---------

We use `Cassandra <http://cassandra.apache.org/>`__ as the primary data
store. It is scalable, has very fast reads and writes, and is
conceptually simple (or at least simpler than SQL databases).

Some helpful links:

-  `Query
   syntax <https://docs.datastax.com/en/cql/3.3/cql/cql_reference/cqlReferenceTOC.html>`__

-  How deletes work in Cassandra:

   -  `Understanding
      Deletes <https://medium.com/@foundev/domain-modeling-around-deletes-1cc9b6da0d24>`__
   -  `Cassandra Compaction and Tombstone
      Behavior <http://engblog.polyvore.com/2015/03/cassandra-compaction-and-tombstone.html>`__
