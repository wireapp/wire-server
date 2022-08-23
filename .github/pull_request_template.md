## Checklist

 - [ ] The **PR Title** explains the impact of the change.
 - [ ] The **PR description** provides context as to why the change should occur and what the code contributes to that effect. This could also be a link to a JIRA ticket or a Github issue, if there is one.
 - [ ] If this PR changes development workflow or dependencies, they have been A) automated and B) documented under docs/developer/. All efforts have been taken to minimize development setup breakage or slowdown for co-workers.
 - [ ] If HTTP endpoint paths have been added or renamed, check [docs/developer/adding-api-endpoints](https://github.com/wireapp/wire-server/blob/develop/docs/legacy/developer/adding-api-endpoints.md) and follow the steps there.
 - [ ] If configuration options have been added or removed, check [docs/developer/adding-config-options](https://github.com/wireapp/wire-server/blob/develop/docs/legacy/developer/adding-config-options.md) and follow the steps there.
 - [ ] If a cassandra schema migration has been added, I ran **`make git-add-cassandra-schema`** to update the cassandra schema documentation.
- [ ] I swear that if I have changed internal end-points, I do not implicitly rely on deployment ordering (brig needing to be deployed before galley), i.e. I have **not** introduced a new internal endpoint in brig and already make use of if in galley, as I'm aware old deployed galleys would throw 500s until all new version have been rolled out and I do not want a few minutes of downtime. Instead, I have thought how to merge brig an galley codebases.
 - [ ] I updated **changelog.d** subsections with one or more entries with the following bits of information ([details](https://github.com/wireapp/wire-server/blob/develop/docs/developer/changelog.md)):
   - [ ] If a cassandra schema migration is backwards incompatible (see also [these docs](https://github.com/wireapp/wire-server/blob/develop/docs/developer/cassandra-interaction.md#cassandra-schema-migrations)), measures to be taken by on-premise instance operators are explained.
   - [ ] If a data migration (not schema migration) introduced: measures to be taken by on-premise instance operators.
