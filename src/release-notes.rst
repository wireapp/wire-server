****************************
Release notes of helm charts
****************************

We have recently migrated all the helm charts from
https://github.com/wireapp/wire-server-deploy to the
https://github.com/wireapp/wire-server repository as we want to decouple
the release schedule for deploying our ansible playbooks for kubernetes
and databases from the wire application charts. Having the wire-server
charts live next to the wire-server code simplifies the release process
for us and release consumption for you. Especially if you’re deploying
wire on your own Kubernetes cluster.

We know it is confusing that the helm chart version was not the same as
the release tag. We are aiming to address this with the migration of the
charts to https://github.com/wireapp/wire-server. This should make
consuming the ‘Releases’ tab on https://github.com/wireapp/wire-server
more self-explanatory. Though this has not been addressed at this point.

I have made a summary of the release notes spread around
wire-server-deploy and wire-server for the latest few helm chart
releases. Please read them carefully as some require you to perform
specific operations.

The following helm chart versions have been published since then:

Chart version 2.104.0
=====================

Release Notes
-------------

Features
--------

-  [federation] Handle errors which could happen while talking to remote
   federator (#1408)
-  [federation] Forward grpc traffic to federator via ingress (or nginz
   for local integration tests) (#1386)
-  [federation] Return UserProfile when getting user by qualified handle
   (#1397)

Bug fixes and other updates
---------------------------

-  [SCIM] Fix: Invalid requests raise 5xxs (#1392)
-  [SAML] Fix: permissions for IdP CRUD operations. (#1405)

Documentation
-------------

-  Tweak docs about team search visibility configuration. (#1407)
-  Move docs around. (#1399)
-  Describe how to look at swagger locally (#1388)

Internal changes
----------------

-  Optimize /users/list-clients to only fetch required things from DB
   (#1398)
-  [SCIM] Remove usage of spar.scim_external_ids table (#1418)
-  Add-license. (#1394)
-  Bump nixpkgs for hls-1.0 (#1412)
-  stack-deps.nix: Use nixpkgs from niv (#1406)

Chart version 2.103.0
=====================

Release Notes
-------------

If you are using Wire's SCIM functionality you shouldn't skip this release.
If you skip it then there's a chance of requests from SCIM clients being missed
during the time window of Wire being upgraded. This might cause sync issues between your SCIM peer
and Wire's user DB.
This is due to an internal data migration job (``spar-migrate-data``) that needs to run once.
If it hasn't run yet then any upgrade to this and any later release will automatically run it.
After it has completed once it is safe again to upgrade Wire while receiving requests from SCIM clients.

Internal changes
----------------

-  Migrate spar external id table (#1400, #1413, #1415, #1417)

Chart version 2.102.0
=====================

Release notes
-------------

This release contains bugfixes and internal changes

Bug fixes and other updates
---------------------------

-  Return PubClient instead of Client from /users/list-clients (#1391)

Internal changes
----------------

-  Federation: Add qualified endpoints for prekey management (#1372)

Chart version 2.101.0
=====================

Release notes
-------------

This release contains bugfixes and internal changes

Bug fixes and other updates
---------------------------

-  Pin kubectl image in sftd chart (#1383)
-  Remove imagePullPolicy: Always for reaper chart (#1387)

Internal changes
----------------

-  Use mu-haskell to implement one initial federation request across
   backends (#1319)
-  Add migrate-external-ids tool (#1384)

Chart version 2.100.0
=====================

Release Notes
-------------

This release might require manual migration steps, see `ElasticSearch
migration instructions for release
2021-02-16 <https://github.com/wireapp/wire-server/blob/c81a189d0dc8916b72ef20d9607888618cb22598/docs/reference/elasticsearch-migration-2021-02-16.md>`__.
The instructions are also shown here below:

Release ``2.100.0`` of ``wire-server`` requires an update of the
ElasticSearch index of ``brig``. During the update the team member
search in TeamSettings will be defunct.

The update is triggered automatically on upgrade by the
``elasticsearch-index-create`` and ``brig-index-migrate-data`` jobs. If
these jobs finish sucessfully the update is complete.

Troubleshooting
---------------

In case the ``elasticsearch-index-create`` job fails this document
describes how to create a new index.

The index that brig is using is defined at
``brig.config.elasticsearch.index`` of the ``wire-server`` chart. We
will refer to its current setting as ``<OLD_INDEX>``.

1. Choose a new index name that is different from ``<OLD_INDEX>``. We
   will refer to this name as ``<NEW_INDEX>``.
2. Upgrade the release with these config changes:

   -  Set ``brig.config.elasticsearch.additionalWriteIndex`` to
      ``<NEW_INDEX>``
   -  Set ``elasticsearch-index.elasticsearch.additionalWriteIndex`` to
      ``<NEW_INDEX>`` and wait for completion.

3. Upgrade the release again with these config changes:

   -  Unset ``brig.config.elasticsearch.additionalWriteIndex``
   -  Unset ``elasticsearch-index.elasticsearch.additionalWriteIndex``
   -  Set ``brig.config.elasticsearch.index`` to ``<NEW_INDEX>``
   -  Set ``elasticsearch-index.elasticsearch.index`` to ``<NEW_INDEX>``

Features
--------

-  Team search: Add search by email (#1344) (#1286)
-  Add endpoint to get client metadata for many users (#1345)
-  Public end-point for getting the team size. (#1295)
-  sftd: add support for multiple SFT servers (#1325) (#1377)
-  SAML allow enveloped signatures (#1375)

Bug fixes and other updates
---------------------------

-  Wire.API.UserMap & Brig.API.Public: Fix Swagger docs (#1350)
-  Fix nix build on OSX (#1340)

Internal changes
----------------

-  [federation] Federation end2end test scripts and Makefile targets
   (#1341)
-  [federation] Brig integration tests (#1342)
-  Add stack 2.3.1 to shell.nix (#1347)
-  buildah: Use correct dist directory while building docker-images
   (#1352)
-  Add spar.scim_external table and follow changes (#1359)
-  buildah: Allow building only a given exec and fix brig templates
   (#1353)
-  Galley: Add /teams/:tid/members csv download (#1351) (#1351)
-  Faster local docker image building using buildah (#1349)
-  Replace federation guard with env var (#1346)
-  Update cassandra schema after latest changes (#1337)
-  Add fast-intermediate Dockerfile for faster PR CI (#1328)
-  dns-util: Allow running lookup with a given resolver (#1338)
-  Add missing internal qa routes (#1336)
-  Extract and rename PolyLog to a library for reusability (#1329)
-  Fix: Spar integration tests misconfigured on CI (#1343)
-  Bump ormolu version (#1366, #1368)
-  Update ES upgrade path (#1339) (#1376)
-  Bump saml2-web-sso version to latest upstream (#1369)
-  Add docs for deriving-swagger2 (#1373) # Chart version 2.99.0

This version was skipped. As we adjusted release procedures to allow for
elasticsearch data migration without downtime in 2.100.0

Chart version 2.98.0
====================

Release Notes
-------------

This release contains bugfixes and internal changes.

Features
--------

-  [federation] Add helm chart for the federator (#1317)

Bug fixes and other updates
---------------------------

-  [SCIM] Accept any query string for externalId (#1330)
-  [SCIM] Allow at most one identity provider (#1332)

Internal changes
----------------

-  [SCIM] Change log level to Warning & format filter logs (#1331)
-  Improve flaky integration tests (#1333)
-  Upgrade nixpkgs and niv (#1326)

Chart version 2.97.0
====================

Release Notes
-------------

This release contains bugfixes and internal changes.

Bug fixes and other updates
---------------------------

-  [SCIM] Fix bug: Deleting a user retains their externalId (#1323)
-  [SCIM] Fix bug: Provisioned users can update update to email, handle,
   name (#1320)

Internal changes
----------------

-  [SCIM] Add logging to SCIM ops, invitation ops, createUser (#1322)
   (#1318)
-  Upgrade nixpkgs and add HLS to shell.nix (#1314)
-  create_test_team_scim.sh script: fix arg parsing and invite (#1321)

Chart version 2.96.0
====================

Release Notes
-------------

This release contains bugfixes and internal changes.

Bug fixes and other updates
---------------------------

-  [SCIM] Bug fix: handle is lost after registration (#1303)
-  [SCIM] Better error message (#1306)

Documentation
-------------

-  [SCIM] Document ``validateSAMLemails`` feature in
   docs/reference/spar-braindump.md (#1299)

Internal changes
----------------

-  [federation] Servantify get users by unqualified ids or handles
   (#1291)
-  [federation] Add endpoint to get users by qualified ids or handles
   (#1291)
-  Allow overriding NAMESPACE for kube-integration target (#1305)
-  Add script create_test_team_scim.sh for development (#1302)
-  Update brig helm chart: Add ``setExpiredUserCleanupTimeout`` (#1304)
-  Nit-picks (#1300)
-  nginz_disco: docker building consistency (#1311)
-  Add tools/db/repair-handles (#1310)
-  small speedup for ‘make upload-charts’ by inlining loop (#1308)
-  Cleanup stack.yaml. (#1312) (#1316)

Wire version 2.95.0
===================

This was the release that the helm charts and wire-server repo were
merged. However no helm chart version was published for it. All the
required changes are bundled in 2.96.0

Chart version 0.130.0, Wire version 2.94.0
==========================================

No notable changes

Chart version 0.129.0, Wire version 2.94.0
==========================================

Release Notes
-------------

As a preparation for federation, this release introduces a mandatory
‘federationDomain’ configuration setting for brig and galley (#1261)

Please update your values/wire-server/values.yaml to set
brig.optSettings.setFederationDomain and
galley.settings.federationDomain (Note the slightly different option
name).

Because federation is not enabled yet the value of this option does not
really matter at this point, but we advise you to set it to the base
domain of your wire instalation.

Features
--------

-  brig: Allow setting a static SFT Server (#1277)

Bug fixes and other updates
---------------------------

Documentation
-------------

Internal changes
----------------

-  Add federation aware endpoint for getting user (#1254)
-  refactor brig Servant API for consistency (#1276)
-  Feature flags cleanup (#1256)

Chart version 0.128.0, Wire version 2.93.0
==========================================

Release Notes
-------------

-  Allow an empty SAML contact list, which is configured at
   ``saml.contacts`` in spar’s config. The contact list is exposed at
   the ``/sso/metadata`` endpoint.

.. _features-4:

Features
--------

-  Make Content-MD5 header optional for asset upload (#1252)
-  Add applock team feature (#1242, #1253)
-  /teams/[tid]/features endpoint

Bug fixes
---------

-  Fix content-type headers in saml responses (#1241)

Internal changes
----------------

-  parse exposed ‘tracestate’ header in nginz logs if present (#1244)
-  Store SCIM tokens in hashed form (#1240)
-  better error handling (#1251)
