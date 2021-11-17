## Checklist

 - [ ] The **PR Title** explains the impact of the change.
 - [ ] The **PR description** provides context as to why the change should occur and what the code contributes to that effect. This could also be a link to a JIRA ticket or a Github issue, if there is one.
 - [ ] If HTTP endpoint paths have been added or renamed, the **endpoint / config-flag checklist** (see Wire-employee only backend [wiki page](https://github.com/zinfra/backend-wiki/wiki/Checklists)) has been followed.
 - [ ] If a cassandra schema migration has been added, I ran **`make git-add-cassandra-schema`** to update the cassandra schema documentation.
 - [ ] **changelog.d** contains the following bits of information ([details](https://github.com/wireapp/wire-server/blob/develop/docs/developer/changelog.md)):
   - [ ] A file with the changelog entry in one or more suitable sub-sections. The sub-sections are marked by directories inside `changelog.d`.
   - [ ] If new config options introduced: added usage description under docs/reference/config-options.md
   - [ ] If new config options introduced: recommended measures to be taken by on-premise instance operators.
   - [ ] If a cassandra schema migration is backwards incompatible (see also [these docs](https://github.com/wireapp/wire-server/blob/develop/docs/developer/cassandra-interaction.md#cassandra-schema-migrations)), measures to be taken by on-premise instance operators are explained.
   - [ ] If a data migration (not schema migration) introduced: measures to be taken by on-premise instance operators.
   - [ ] If public end-points have been changed or added: does nginz need un upgrade?
   - [ ] If internal end-points have been added or changed: which services have to be deployed in a specific order?
