## Checklist

 - [ ] The **PR Title** explains the impact of the change.
 - [ ] The **PR description** provides context as to why the change should occur and what the code contributes to that effect. This could also be a link to a JIRA ticket or a Github issue, if there is one.
 - [ ] If end-points have been added or changed: the **endpoint / config-flag checklist** (see Wire-employee only backend [wiki page](https://github.com/zinfra/backend-wiki/wiki/Checklists)) has been followed.
 - [ ] If a schema migration has been added, I ran **`make git-add-cassandra-schema`** to update the cassandra schema documentation.
 - [ ] **changelog.d** contains the following bits of information:
   - [ ] A file with the changelog entry in one or more suitable sub-sections. The sub-sections are marked by directories inside `changelog.d`.
   - [ ] If /a: measures to be taken by instance operators.
   - [ ] If /a: list of cassandra migrations.
   - [ ] If public end-points have been changed or added: does nginz need upgrade?
   - [ ] If internal end-points have been added or changed: which services have to be deployed in a specific order?
