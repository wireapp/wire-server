## Checklist

 - [ ] Title of this PR explains the impact of the change.
 - [ ] The description provides context as to why the change should occur and what the code contributes to that effect. This could also be a link to a JIRA ticket or a Github issue, if there is one.
 - [ ] If end-points have been added or changed: the endpoint / config-flag checklist (see Wire-employee only backend [wiki page](https://github.com/zinfra/backend-wiki/wiki/Checklists)) has been followed.
 - [ ] If a schema migration has been added, I ran `make git-add-cassandra-schema` to update the cassandra schema documentation.
 - Section *Unreleased* of CHANGELOG.md contains the following bits of information:
   - [ ] A line with the title and number of the PR in one or more suitable sub-sections.
   - [ ] If /a: measures to be taken by instance operators.
   - [ ] If /a: list of cassandra migrations.
   - [ ] If public end-points have been changed or added: does nginz need upgrade?
   - [ ] If internal end-points have been added or changed: which services have to be deployed in a specific order?
