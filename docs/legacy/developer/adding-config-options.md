<!-- vim-markdown-toc GFM -->

* [Adding new configuration flags in wire-server](#adding-new-configuration-flags-in-wire-server)
    * [Removing configuration flags](#removing-configuration-flags)
    * [Renaming configuration flags](#renaming-configuration-flags)

<!-- vim-markdown-toc -->

## Adding new configuration flags in wire-server

While developing code for e.g. `brig` in `wire-server`, edit:

* [ ] the parser under `services/brig/src/Brig/Options.hs`
* [ ] the integration test config: `services/brig/brig.integration.yaml`
* [ ] the demo config: under `deploy/services-demo/conf/` files `brig.demo.yaml` and `brig.demo-docker.yaml`
* [ ] the charts: `charts/brig/templates/configmap.yaml` (and maybe `charts/brig/values.yaml` for defaults, and `hack/helm_vars/wire-server/values.yaml` for different values during CI integration tests)
* [ ] Add usage description under `docs/legacy/reference/config-options.md`

If your configuration value is new and required, and has no default, then you need to

* [ ] advertise this in the `./changelog.d/0-release-notes/` for other wire-server operators to know about.
* [ ] update all the environments into which code will get deployed to. For wire Cloud, look into all the relevant environments (search for `helm_vars/wire-server/values.yaml.gotmpl` files.) (these configuration updates should ideally be merged **before** merging your PR to wire-server/develop.

### Removing configuration flags

Remove them with the PR from wire-server `./charts` folder, as charts are linked to code and go hand-in hand. Possibly notify all operators (through `./changelog.d/0-release-notes/`) that some overrides may not have any effect anymore.

### Renaming configuration flags

Avoid doing this. If you must, see Removing/adding sections above. But please note that all people who have an installation of wire also may have overridden any of the configuration option you may wish to change the name of. As this is not type checked, it's very error prone and people may find themselves with default configuration values being used instead of their intended configuration settings. Guideline: only rename for good reasons, not for aesthetics; or be prepared to spend a significant
amount on documenting and communication about this change.

