# Notes for developers

What you need to know as a user of the Wire backend: concepts,
features, and API. We want to keep these up to date.  They could
benefit from some re-ordering, and they are far from complete, but we
hope they will still help you.

## TOC

### developer

* [api-versioning.md](./developer/api-versioning.md)
* [cassandra-interaction.md](./developer/cassandra-interaction.md)
* [changelog.md](./developer/changelog.md)
* [dependencies.md](./developer/dependencies.md)
* [editor-setup.md](./developer/editor-setup.md)
* [features.md](./developer/features.md)
* [federation-api-conventions.md](./developer/federation-api-conventions.md)
* [how-to.md](./developer/how-to.md)
* [linting.md](./developer/linting.md)
* [processes.md](./developer/processes.md)
* [scim/storage.md](./developer/scim/storage.md)
* [servant.md](./developer/servant.md)
* [testing.md](./developer/testing.md)

### reference

* [config-options.md](./reference/config-options.md)
* [conversation.md](./reference/conversation.md)
* [elastic-search.md](./reference/elastic-search.md)
* [elasticsearch-migration-2021-02-16.md](./reference/elasticsearch-migration-2021-02-16.md)
* [make-docker-and-qemu.md](./reference/make-docker-and-qemu.md)
* [provisioning/scim-token.md](./reference/provisioning/scim-token.md)
* [provisioning/scim-via-curl.md](./reference/provisioning/scim-via-curl.md)
* [provisioning/wire_scim_token.py](./reference/provisioning/wire_scim_token.py)
* [spar-braindump.md](./reference/spar-braindump.md)
* [team/legalhold.md](./reference/team/legalhold.md)
* [user/activation.md](./reference/user/activation.md)
* [user/connection.md](./reference/user/connection.md)
* [user/connections-flow-1-backend.png](./reference/user/connections-flow-1-backend.png)
* [user/connection-transitions.png](./reference/user/connection-transitions.png)
* [user/connection-transitions.xml](./reference/user/connection-transitions.xml)
* [user/registration.md](./reference/user/registration.md)
* [user/rich-info.md](./reference/user/rich-info.md)


## Users

User lifecycle:

* [User registration](reference/user/registration.md) `{#RefRegistration}`
* [User activation](reference/user/activation.md) `{#RefActivation}`

User profiles and metadata:

* [Connections between users](reference/user/connection.md) `{#RefConnection}`
* [Rich info](reference/user/rich-info.md) `{#RefRichInfo}`

## SCIM provisioning

We have support for provisioning users via SCIM ([RFC 7664][], [RFC 7643][]). It's in the beta stage.

[RFC 7664]: https://tools.ietf.org/html/rfc7664
[RFC 7643]: https://tools.ietf.org/html/rfc7643

* [Using the SCIM API with curl](reference/provisioning/scim-via-curl.md) `{#RefScimViaCurl}`
* [Authentication via SCIM tokens](reference/provisioning/scim-token.md) `{#RefScimToken}`

## Hints

Internal documentation detailing what you need to know as a Wire backend developer. All of these documents can and should be referenced in the code.

If you're not a member of the Wire backend team, you might still find these documents useful, but keep in mind that they are a work in progress.

* [Development setup](developer/dependencies.md) `{#DevDeps}`
* [Editor setup](developer/editor-setup.md) `{#DevEditor}`
* [Storing SCIM-related data](developer/scim/storage.md) `{#DevScimStorage}`

## Cassandra

We use [Cassandra](http://cassandra.apache.org/) as the primary data store. It is scalable, has very fast reads and writes, and is conceptually simple (or at least simpler than SQL databases).

Some helpful links:

* [Query syntax](https://docs.datastax.com/en/cql/3.3/cql/cql_reference/cqlReferenceTOC.html)

* How deletes work in Cassandra:

  - [Understanding Deletes](https://medium.com/@foundev/domain-modeling-around-deletes-1cc9b6da0d24)
  - [Cassandra Compaction and Tombstone Behavior](http://engblog.polyvore.com/2015/03/cassandra-compaction-and-tombstone.html)
