## Checklist

 - [ ] If configuration options have been added or removed, check [docs/developer/adding-config-options](https://github.com/wireapp/wire-server/blob/develop/docs/legacy/developer/adding-config-options.md) and follow the steps there.
 - [ ] If a cassandra schema migration has been added, I ran **`make git-add-cassandra-schema`** to update the cassandra schema documentation.

 - [ ] Create a new entry in **changelog.d**.
 - [ ] I updated **changelog.d** subsections with one or more entries with the following bits of information ([details](https://github.com/wireapp/wire-server/blob/develop/docs/developer/changelog.md)):
   - [ ] If a cassandra schema migration is backwards incompatible (see also [these docs](https://github.com/wireapp/wire-server/blob/develop/docs/developer/cassandra-interaction.md#cassandra-schema-migrations)), measures to be taken by on-premise instance operators are explained.
   - [ ] If a data migration (not schema migration) introduced: measures to be taken by on-premise instance operators.
