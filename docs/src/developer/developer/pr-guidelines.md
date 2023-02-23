# PR Guidelines

This document outlines the steps that need to be taken before merging any PR. In most cases, the only required action is creating a changelog entry (see below). However, when a PR affects the database schema, or the API, or service configuration, extra steps are required, and those are detailed below.

The recommended way to use this document is to copy the relevant checklists below into the PR description, when appropriate, and make sure they are all checked before the PR is merged.

## Changelog entries

Every PR should add a new file in the appropriate subdirectory of `changelog.d`, containing just the text of the corresponding changelog entry. There is no need to explicitly write a PR number, because the `mk-changelog.sh` script (used on release) will add it automatically at the end. The name of the file does not matter, but it should be unique to avoid unnecessary conflicts (e.g. use the branch name).

It is still possible to write the PR number manually if so desired, which is useful in case the entry is shared by multiple PRs, or if the PR is merged with a merge commit rather than by squashing. In that case, the script would leave the PR number reference intact, as long as it is at the very end of the entry, with no period afterwards, in brackets, and preceded by a `#` symbol (e.g. #2646).

As long as the PR is merged by squashing, it is also possible to use the pattern `##` to refer to the current PR number. This will be replaced throughout.

Multiline entries are supported, and are handled correctly by the script. Again, the PR reference should either be omitted or put at the very end. If multiple entries for a single PR are desired, there should be a different file for each of them.

See `docs/legacy/developer/changelog.md` for more information.

## Schema migrations

Don't delete columns that are still used by versions that are deployed. If you delete columns then the old version will fail in the deployment process. Rather than deleting keep the unused columns around and comment them as being discontinued in the schema migration code.

If a cassandra schema migration has been added then add this to the checklist:

 - [ ] Run **`make git-add-cassandra-schema`** to update the cassandra schema documentation

### Incompatible schema migrations and data migrations

If the PR contains a cassandra *schema* migration which is backwards incompatible, a changelog entry should be added to the release notes. See [notes on Cassandra](https://github.com/wireapp/wire-server/blob/develop/docs/developer/cassandra-interaction.md#cassandra-schema-migrations) for more details on how to implement such schema changes. A similar entry should be added if the PR contains a *data* migration.

 - [ ] Add a changelog entry in `0-release-notes` detailing measures to be taken by instance operators

## Adding new public endpoints

When adding new endpoints in the Haskell code in wire-server, correct routing needs to be applied at the nginz level.

NB: The nginz paths are interpreted as *prefixes*.  If you add a new end-point that is identical to an existing one except for the path of the latter being a proper prefix of the former, and if the nginz configuration of the two paths should be the same, nothing needs to be done.  Exception: if you see a path like `/self$`, you know it doesn't match `/self/sub/what`.

The following needs to be done, as part of a PR adding endpoints or changing endpoint paths.

 - [ ] Update nginz config in helm: `charts/nginz/values.yaml`
 - [ ] Update nginz config for the local integration tests: `services/nginz/integration-test/conf/nginz/nginx.conf`

### Helm configuration

For internal endpoints for QA access on staging environments, copy a block with `/i/` containing

```
  - path: /some/path
    envs:
    - staging
    disable_zauth: true
    basic_auth: true
```

For customer support access to an internal endpoint, instead update code in [stern](https://github.com/wireapp/wire-server/tree/develop/tools/stern) as part of your PR. There is no need to add that endpoint to nginz.

### Demo nginz configuration

New entris should include `common_response_no_zauth.conf;` for public endpoints without authentication and `common_response_with_zauth.conf;` for regular (authenticated) endpoints. Browse the file to see examples.

### Example

If the following endpoints are added to galley:

```
GET /new/endpoint
POST /turtles
PUT /turtles/<turtleId>/name
```

Add to `charts/nginz/values.yaml`, under the `galley` section:

```
- path: /new/endpoint
- path: ^/turtles(.*)
```

## Adding new configuration flags in wire-server

If a PR adds new configuration options for say brig, the following files need to be edited:

* [ ] The parser under `services/brig/src/Brig/Options.hs`
* [ ] The integration test config: `services/brig/brig.integration.yaml`
* [ ] The charts: `charts/brig/templates/configmap.yaml`
* [ ] The default values: `charts/brig/values.yaml`
* [ ] The values files for CI: `hack/helm_vars/wire-server/values.yaml.gotmpl`
* [ ] The configuration docs: `docs/src/developer/reference/config-options.md`

If any new configuration value is required and has no default, then:

* [ ] Write a changelog entry in `0-release-notes` advertising the new configuration value
* [ ] Update all the relevant environments

For wire Cloud, look into all the relevant environments (look for `helm_vars/wire-server/values.yaml.gotmpl` files in cailleach). Ideally, these configuration updates should be merged **before** merging the corresponding wire-server PR.

### Removing configuration flags

Remove them with the PR from wire-server `./charts` folder, as charts are linked to code and go hand-in hand. Possibly notify all operators (through `./changelog.d/0-release-notes/`) that some overrides may not have any effect anymore.

### Renaming configuration flags

Avoid doing this, it's usually viable to introduce an at-least-equally-good name and remove the old one, that admins can first add the new options, then uprade the software, then remove the old ones.

If you must, see Removing/adding sections above. But please note that all people who have an installation of wire also may have overridden any of the configuration option you may wish to change the name of. As this is not type checked, it's very error prone and people may find themselves with default configuration values being used instead of their intended configuration settings. Guideline: only rename for good reasons, not for aesthetics; or be prepared to spend a significant amount on documenting and communication about this change.

## Changes to developer workflow

If a PR changes development workflow or dependencies, they should be automated and documented under `docs/developer/`. All efforts should be taken to minimize development setup breakage or slowdown for co-workers.
