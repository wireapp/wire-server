<!-- if you're not the release manager, do your edits to changelog under CHANGELOG.d/ -->

# [2021-09-14]

## API changes

* Remove the long-deprecated `message` field in `POST /connections` (#1726)
* Add `PUT /conversations/:domain/:cnv/name` (#1737)
* Deprecate `PUT /conversations/:cnv/name` (#1737)
* Add `GET & PUT /conversations/:domain/:cnv/self` (#1740)
* Deprecate `GET & PUT /conversations/:cnv/self` (#1740)
* Remove endpoint `GET /conversations/:domain/:cnv/self` (#1752)
* The `otr_muted` field in `Member` and `MemberUpdate` has been removed. (#1751)
* Removed the ability to update one's own role (#1752)

## Features

* Disallow changing phone number to a black listed phone number (#1758)
* Support using a single IDP with a single EntityID (aka issuer ID) to set up two teams. Required to support multiple teams in environments where the IDP software cannot present anything but one EntityID (E.G.: DualShield). (#1755)

## Documentation

* Added documentation of federation errors (#1674)
* Better swagger schema for the Range type (#1748)
* Add better example for Domain in swagger (#1748)

## Internal changes

* Introduce new process for writing changelogs (#1749)
* Clean up JSON golden tests (Part 4, Part 5) (#1756, #1762)
* Increased timeout on certificate update tests to 10s (#1750)
* Fix for flaky test in spar (#1760)
* Rewrite the `POST /connections` endpoint to Servant (#1726)
* Various improvements and fixes around SAML/SCIM (#1735)

## Federation changes

* Avoid remote calls to get conversation when it is not found locally (#1749)
* Federator CA store and client credentials are now automatically reloaded (#1730)
* Ensure clients only receive messages meant for them in remote convs (#1739)


# [2021-09-08]

## Release Notes

## API Changes

* Add `POST /conversations/list/v2` (#1703)
* Deprecate `POST /list-conversations` (#1703)

## Features

* Bump SFTD to 2.0.127 (#1745)

## Bug fixes and other updates

* Remove support for managed conversations in member removal (#1718)
* Update the webapp to correct labeling on CBR calling (#1743)

## Documentation

* Document backend internals for user connections (#1717)
* Open Update spar braindump and explain idp deletion (#1728)

## Internal changes

* Integration test script now displays output interactively (#1700)
* Fixed a few issues with error response documentation in Swagger (#1707)
* Make mapping between (team) permissions and roles more lenient (#1711)
* The `DELETE /conversations/:cnv/members/:usr` endpoint rewritten to Servant (#1697)
* Remove leftover auto-connect internal endpoint and code (#1716)
* Clean up JSON golden tests (#1729, #1732, #1733)
* Make regenerated golden tests' JSON output deterministic (#1734)
* Import fix for snappy linker issue (#1736)

## Federation changes

* Added client certificate support for server to server authentication (#1682)
* Implemented full server-to-server authentication (#1687)
* Add an endpoint for removing a qualified user from a local conversation (#1697)
* Refactored remote error handling in federator (#1681)
* The update conversation membership federation endpoint takes OriginDomainHeader (#1719)
* Added new endpoint to allow fetching conversation metadata by qualified ids (#1703)

# [2021-08-27]

## Release Notes

## API Changes

* Deprecate `DELETE /conversations/:cnv/members/:usr` (#1697)
* Add `DELETE /conversations/:cnv/members/:domain/:usr` (#1697)

## Features

## Bug fixes and other updates

* Fix case sensitivity in schema parser in hscim library (#1714)
* [helm charts] resolve a rate-limiting issue when using certificate-manager alongside wire-server and nginx-ingress-services helm charts (#1715)

## Documentation

* Improve Swagger for `DELETE /conversations/:cnv/members/:usr` (#1697)

## Internal changes

* Integration test script now displays output interactively (#1700)
* Fixed a few issues with error response documentation in Swagger (#1707)
* Make mapping between (team) permissions and roles more lenient (#1711)
* The `DELETE /conversations/:cnv/members/:usr` endpoint rewritten to Servant (#1697)
* Remove leftover auto-connect internal endpoint and code (#1716)
* Bump wire-webapp (#1720)
* Bump team-settings (#1721)
* Bump account-pages (#1666)

## Federation changes

* Added client certificate support for server to server authentication (#1682)
* Implemented full server-to-server authentication (#1687)
* Add an endpoint for removing a qualified user from a local conversation (#1697)


# [2021-08-16]

## Release Notes

This is a routine release requiring only the routine upgrade steps.

## API Changes

* Add `POST /conversations/list-ids` (#1686)
* Deprecate `GET /converstations/ids` (#1686)

## Features

* Client functions for the hscim library (#1694, #1699, #1702, https://hackage.haskell.org/package/hscim)

## Bug fixes and other updates

* Change http response code for `missing-legalhold-consent`. (#1688)
* Remove old end-point for changing email

## Federation changes (alpha feature, do not use yet)

* Add new API to list paginated qualified conversation ids (#1686)

## Documentation

* Fix swagger: mark name in UserUpdate as optional (#1691, #1692)

## Internal changes

* Replaced uses of `UVerb` and `EmptyResult` with `MultiVerb` (#1693)
* Added a mechanism to derive `AsUnion` instances automatically (#1693)
* Integration test coverage (#1696, #1704)

# [2021-08-02]

## Release Notes

If you want to set the default for file sharing in all teams to `disabled`, search for "File Sharing" in https://github.com/wireapp/wire-server/tree/develop/docs/reference/config-options.md.

## Release Notes for Wire.com Cloud operators

Upgrade nginz (#1658)

## API Changes

## Features

* A new team feature for classified domains is available (#1626):
  - a public endpoint is at `GET /teams/:tid/features/classifiedDomains`
  - an internal endpoint is at `GET /i/teams/:tid/features/classifiedDomains`
* Extend feature config API (#1658)
* `fileSharing` feature config (#1652, #1654, #1655)
* `conferenceCalling` feature flag (#1683)
* Add user_id to csv export (#1663)

## Bug fixes and other updates

* New, hardened end-point for changing email (68b4db08)
* Fix: CSV export is missing SCIM external id when SAML is also used (#1608)
* Fix: sso_id field in user record (brig) was not always filled correctly in cassandra (#1334)
* Change http response code for `missing-legalhold-consent` from 412 to 403 (#1688)

## Documentation

* Improved Swagger documentation for endpoints with multiple responses (#1649, #1645)

## Internal changes

* Improvements to local integration test setup when using buildah and kind (#1667)
* The servant-swagger dependency now points to the current upstream master (#1656)
* Improved error handling middleware (#1671)
* Refactor function createUser for readability (#1670)
* Removed explicit implementation for user HEAD endpoints (#1679)
* Improved test coverage for error responses (#1680)
* Introduced `MultiVerb` endpoints in Servant API (#1649).

## Federation changes (alpha feature, do not use yet)

* Validate server TLS certificate between federators (#1662)
* A clarification is added about listing your own domain as a classified domain (#1678)
* Added a `QualifiedCapture` type to Servant for qualified paths (#1669)
* Renamed `DomainHeader` type to `OriginDomainHeader` (#1689)
* Added golden tests for protobuf serialisation / deserialisation (#1644).

# [2021-07-09]

## Release Notes

This release requires a manual change in your galley configuration: `settings.conversationCodeURI` in `galley.yaml` was had to be set to `${WEBAPP}/join` before this release, and must be set to `${ACCOUNTS}/conversation-join` from now on, where `${WEBAPP}` is the url to the webapp and `${ACCOUNTS}` is the url to the account pages.

## API Changes

* Several public team feature endpoints are removed (their internal and
  Stern-based counterparts remain available):
  - `PUT /teams/:tid/features/sso`
  - `PUT /teams/:tid/features/validateSAMLemails`
  - `PUT /teams/:tid/features/digitalSignatures`
* All endpoints that fetch conversation details now also include a new key
  `qualified_id` for a qualified conversation ID (#1640)
* New endpoint `POST /list-conversations` similar to `GET /conversations`, but which will also return your own remote conversations (if federation is enabled). (#1591)

## Features

* Change `settings.conversationCodeURI` in galley.yaml (#1643).
* [Federation] RPC to propagate messages to other backends (#1596).
* [Federation] Fetch remote user's clients when sending messages (#1635).
* [Federation] Actually propagate messages to other backends (#1638).
* [Federation] Support sending messages to remote conversations (#1609).
* [Federation] Guard against path traversal attacks (#1646).

## Internal changes

* Feature endpoints are rewritten in Servant (#1642).
* Internal federation endpoints using the publicly-facing conversation data type
  now also include a qualified conversation ID under the `qualified_id` key
  (#1640)
* schema-profunctor: add `optField` combinator and corresponding documentation (#1621, #1624).
* [Federation] Let a receiving backend decide conversation attribute specifics of its users
  added to a new conversation via `POST /federation/register-conversation` (#1622).
* [Federation] Adjust scripts under ./hack/federation to work with recent changes to the federation API (#1632).
* Refactored Proteus endpoint to work with qualified users (#1634).
* Refactored Federator InternalServer (#1637)

### Internal Federation API changes

* Breaking change on InwardResponse and OutwardResponse in router.proto for improved error handling (#1637)
  * Note: federation should not be in use anywhere yet, so this should not have any impact

## Documentation

* Fix validation errors in Swagger documentation (#1625).

## Bug fixes and other updates

* Restore old behaviour for parse errors in request bodies (#1628, #1629).
* Allow to change IdP Issuer name to previous name (#1615).


# [2021-06-23]

## API Changes

* [Federation] Add qualified endpoint for sending messages at `POST /conversations/:domain/:cnv/proteus/messages` (#1593, #1614, #1616, #1620).
* Replace 'otr' with 'proteus' in new message sending API (#1616)

## Features

## Bug fixes and other updates

* [helm] Allow sending messages upto 40 MB by default (#1614)
* Fix for https://github.com/wireapp/wire-webapp/security/advisories/GHSA-382j-mmc8-m5rw  (#1613)
* Update wire-webapp version (#1613)
* Update team-settings version (#1598)
* Allow optional password field in RmClient (#1604, #1607)
* Add endpoint: Get name, id with for CodeAccess conversations (#1592)
* demote logging failed invitations to a warning, rather than an error. Server operators can't act on these errors in any way (#1586)

## Documentation

* Add descriptive comments to `ConversationMemberUpdate` (#1578)
* initial few anti-patterns and links about cassandra (#1599)

## Internal changes

* Rename a local members field in the Conversation data type (#1580)
* Servantify Protobuf endpoint to send messages (#1583)
* Servantify own client API (#1584, #1603)
* Remove resource requests (#1581)
* Import http2 fix (#1582)
* Remove stale FUTUREWORK comment (#1587)
* Reorganise helper functions for conversation notifications (#1588)
* Extract origin domain header name for use in API (#1597)
* Merge Empty200, Empty404 and EmptyResult (#1589)
* Set content-type header for JSON errors in Servant (#1600)
* Add golden tests for ClientCapability(List) (#1590)
* Add checklist for PRs (#1601, #1610)
* Remove outdated TODO (#1606)
* submodules (#1612)

## More federation changes (inactive code)

* Add getUserClients RPC (and thereby allow remote clients lookup) (#1500)
* minor refactor: runFederated (#1575)
* Notify remote backends when users join (#1556)
* end2end test getting remote conversation and complete its implementation (#1585)
* Federation: Notify Remote Users of Being Added to a New Conversation (#1594)
* Add qualified endpoint for sending messages (#1593, #1614)
* Galley/int: Expect remote call when creating conv with remotes (#1611)


# [2021-06-08]

## Release Notes

This release doesn't require any extra considerations to deploy.

## Release Notes for Wire.com Cloud operators

Deploy brig before galley (#1526, #1549)

## Features
* Update versions of webapp, team-settings, account-pages (#1559)
* Add missing /list-users route (#1572)
* [Legalhold] Block device handshake in case of LH policy conflict (#1526)
* [Legalhold] Fix: Connection type when unblocking after LH (#1549)
* [Legalhold] Allow Legalhold for large teams (>2000) if enabled via whitelist (#1546)
* [Legalhold] Add ClientCapabilities to NewClient. (#1552)
* [Legalhold] Dynamic whitelisted teams & whitelist-teams-and-implicit-consent feature in tests (#1557, #1574)
* [Federation] Add remote members to conversations (#1529)
* [Federation] Federation: new endpoint: GET /conversations/{domain}/{cnv} (#1566)
* [Federation] Parametric mock federator (#1558)
* [Federation] Add more information to federation errors (#1560)
* [Federation] Add remote users when creating a conversation (#1569)
* [Federation] Update conversation membership in a remote backend (#1540)
* [Federation] expose /conversations/{cnv}/members/v2 for federation backends (#1543)

## Bug fixes and other updates
* Fix MIME-type of asset artifacts
* Add some missing charts (#1533)

# Internal changes
* Qualify users and conversations in Event (#1547)
* Make botsAndUsers pure (#1562)
* Set swagger type of text schema (#1561)
* More examples in schema-profunctor documentation (#1539)
* Refactoring-friendly FutureWork data type (#1550)
* nginz/Dockerfile: Run 'apk add' verbosely for debugging (#1565)
* Introduce a generalized version of wai-extra Session type constructor (#1563)
* Avoid wrapping error in rethrow middleware (#1567)
* wire-api: Introduce ErrorDescription (#1573)
* [Federation] Use Servant.respond instead of explicit SOP (#1535)
* [Federation] Add end2end test for adding remote users to a conversation (#1538)
* [Federation] Add required fields to Swagger for SchemaP (#1536)
* [Federation] Add Galley component to federator API (#1555)
* [Federation] Generalises the mock federator to work with any MonadIO m monad (#1564)
* [Federation] Introduces the HasGalley class (#1568)
* [Federation] Servantify JSON endpoint to send messages (#1532)
* [Federation] federator: rename Brig -> Service and add galley (#1570)

## Documentation
* Update Rich Info docs (#1544)


# [2021-05-26]

## Release Notes

**Legalhold:** This release introduces a notion of "consent" to
legalhold (LH).  If you are using LH on your site, follow the
instructions in
https://github.com/wireapp/wire-server/blob/814f3ebc251965ab4492f5df4d9195f3b2e0256f/docs/reference/team/legalhold.md#whitelisting-and-implicit-consent
after the upgrade.  **Legalhold will not work as expected until you
change `galley.conf` as described!**

**SAML/SCIM:** This release introduces changes to the way `NameID` is
processed: all identifiers are stored in lower-case and qualifiers are
ignored.  No manual upgrade steps are necessary, but consult
https://docs.wire.com/how-to/single-sign-on/trouble-shooting.html#theoretical-name-clashes-in-saml-nameids
on whether you need to re-calibrate your SAML IdP / SCIM setup.
(Reason / technical details: this change is motivated by two facts:
(1) email casing is complicated, and industry best practice appears to
be to ignore case information even though that is in conflict with the
official standard documents; and (2) SCIM user provisioning does not
allow to provide SAML NameID qualifiers, and guessing them has proven
to be infeasible.  See
https://github.com/wireapp/wire-server/pull/1495 for the code
changes.)

## Features
 - [SAML/SCIM] More lenient matching of user ids (#1495)
 - [Legalhold] Block and kick users in case of LH no_consent conflict (1:1 convs). (#1507, #1530)
 - [Legalhold] Add legalhold status to user profile (#1522)
 - [Legalhold] Client-supported capabilities end-point (#1503)
 - [Legalhold] Whitelisting Teams for LH with implicit consent (#1502)
 - [Federation] Remove OptionallyQualified data type from types-common (#1517)
 - [Federation] Add RPC getConversations (#1493)
 - [Federation] Prepare remote conversations: Remove Opaque/Mapped Ids, delete remote identifiers from member/user tables. (#1478)
 - [Federation] Add schema migration for new tables (#1485)
 - [SAML/SCIM] Normalize SAML identifiers and fix issues with duplicate account creation (#1495)
 - Internal end-point for ejpd request processing. (#1484)

## Bug fixes and other updates
 - Fix: NewTeamMember vs. UserLegalHoldStatus (increase robustness against rogue clients) (#1496)

## Documentation
 - Fixes a typo in the wire-api documentation (#1513)
 - Unify Swagger 2.0 docs for brig, galley and spar (#1508)

## Internal changes
 - Cleanup (no change in behavior) (#1494, #1501)
 - wire-api: Add golden test for FromJSON instance of NewOtrMessage (#1531)
 - Swagger/JSON cleanup (#1521, #1525)
 - Work around a locale issue in Ormolu (#1520)
 - Expose mock federator in wire-api-federation (#1524)
 - Prettier looking golden tests (#1527)
 - Refactorings, bug fixes (in tests only) (#1523)
 - Use sed instead of yq to read yaml files (#1518)
 - Remove zauth dependency from wire-api (#1516)
 - Improve naming conventions federation RPC calls (#1511)
 - Event refactoring and schema instances (#1506)
 - Fix: regenerate cabal files. (#1510)
 - Make DerivingVia a package default. (#1505)
 - Port instances to schemas library (#1482)
 - wire-api-federator: Make client tests more reliable (#1491)
 - Remove duplicated roundtrip test (#1498)
 - schema-profunctor: Add combinator for nonEmptyArray (#1497)
 - Golden tests for JSON instances (#1486)
 - galley: Convert conversation endpoints to servant (#1444, #1499)
 - Fix Arbitrary instances and enable corresponding roundtrip tests (#1492)
 - wire-api-fed: Mark flaky tests as pending
 - RFC: Schemas for documented bidirectional JSON encoding (#1474)


# [2021-05-04]

## Features
 - [brig] New option to use a random prekey selection strategy to remove DynamoDB dependency (#1416, #1476)
 - [brig] Ensure servant APIs are recorded by the metrics middleware (#1441)
 - [brig] Add exact handle matches from all teams in /search/contacts (#1431, #1455)
 - [brig] CSV endpoint: Add columns to output (#1452)
 - [galley] Make pagination more idiomatic (#1460)
 - [federation] Testing improvements (#1411, #1429)
 - [federation] error reporting, DNS error logging (#1433, #1463)
 - [federation] endpoint refactoring, new brig endpoints, servant client for federated calls, originDomain metadata (#1389, #1446, #1445, #1468, #1447)
 - [federation] Add federator to galley (#1465)
 - [move-team] Update move-team with upstream schema changes #1423

## Bug fixes and other updates
 - [security] Update webapp container image tag to address CVE-2021-21400 (#1473)
 - [brig] Return correct status phrase and body on error (#1414) …
 - [brig] Fix FromJSON instance of ListUsersQuery (#1456)
 - [galley] Lower the limit for URL lengths for galley -> brig RPC calls (#1469)
 - [chores] Remove unused dependencies (#1424) …
 - [compilation] Stop re-compiling nginz when running integration test for unrelated changes
 - [tooling] Use jq magic instead of bash (#1432), Add wget (#1443)
 - [chores] Refactor Dockerfile apk installation tasks (#1448)
 - [tooling] Script to generate token for SCIM endpoints (#1457)
 - [tooling] Ormolu script improvements (#1458)
 - [tooling] Add script to colourise test failure output (#1459)
 - [tooling] Setup for running tests in kind (#1451, #1462)
 - [tooling] HLS workaround for optimisation flags (#1449)

## Documentation
 - [docs] Document how to run multi-backend tests for federation (#1436)
 - [docs] Fix CHANGELOG: incorrect release dates (#1435)
 - [docs] Update release notes with data migration for SCIM (#1442)
 - [docs] Fixes a k8s typo in the README (#1475)
 - [docs] Document testing strategy and patterns (#1472)


# [2021-03-23]

## Features

* [federation] Handle errors which could happen while talking to remote federator (#1408)
* [federation] Forward grpc traffic to federator via ingress (or nginz for local integration tests) (#1386)
* [federation] Return UserProfile when getting user by qualified handle (#1397)

## Bug fixes and other updates

* [SCIM] Fix: Invalid requests raise 5xxs (#1392)
* [SAML] Fix: permissions for IdP CRUD operations. (#1405)

## Documentation

*  Tweak docs about team search visibility configuration. (#1407)
*  Move docs around. (#1399)
*  Describe how to look at swagger locally (#1388)

## Internal changes

* Optimize /users/list-clients to only fetch required things from DB (#1398)
* [SCIM] Remove usage of spar.scim_external_ids table (#1418)
* Add-license. (#1394)
* Bump nixpkgs for hls-1.0 (#1412)
* stack-deps.nix: Use nixpkgs from niv (#1406)


# [2021-03-21]

## Release Notes

If you are using Wire's SCIM functionality you shouldn't skip this release. If you skip it then there's a chance of requests from SCIM clients being missed during the time window of Wire being upgraded.
This might cause sync issues between your SCIM peer and Wire's user DB.
This is due to an internal data migration job (`spar-migrate-data`) that needs to run once. If it hasn't run yet then any upgrade to this and any later release will automatically run it. After it has completed once it is safe again to upgrade Wire while receiving requests from SCIM clients.

## Internal changes

* Migrate spar external id table (#1400, #1413, #1415, #1417)

# [2021-03-02]

## Bug fixes and other updates

* Return PubClient instead of Client from /users/list-clients (#1391)

## Internal changes

* Federation: Add qualified endpoints for prekey management (#1372)


# [2021-02-25]

## Bug fixes and other updates

* Pin kubectl image in sftd chart (#1383)
* Remove imagePullPolicy: Always for reaper chart (#1387)


## Internal changes

* Use mu-haskell to implement one initial federation request across backends (#1319)
* Add migrate-external-ids tool (#1384)


# [2021-02-16]

## Release Notes

This release might require manual migration steps, see [ElasticSearch migration instructions for release 2021-02-16 ](https://github.com/wireapp/wire-server/blob/c81a189d0dc8916b72ef20d9607888618cb22598/docs/reference/elasticsearch-migration-2021-02-16.md).

## Features

* Team search: Add search by email (#1344) (#1286)
* Add endpoint to get client metadata for many users (#1345)
* Public end-point for getting the team size. (#1295)
* sftd: add support for multiple SFT servers (#1325) (#1377)
* SAML allow enveloped signatures (#1375)

## Bug fixes and other updates

* Wire.API.UserMap & Brig.API.Public: Fix Swagger docs (#1350)
* Fix nix build on OSX (#1340)

## Internal changes

* [federation] Federation end2end test scripts and Makefile targets (#1341)
* [federation] Brig integration tests (#1342)
* Add stack 2.3.1 to shell.nix (#1347)
* buildah: Use correct dist directory while building docker-images (#1352)
* Add spar.scim_external table and follow changes (#1359)
* buildah: Allow building only a given exec and fix brig templates (#1353)
* Galley: Add /teams/:tid/members csv download (#1351) (#1351)
* Faster local docker image building using buildah (#1349)
* Replace federation guard with env var (#1346)
* Update cassandra schema after latest changes (#1337)
* Add fast-intermediate Dockerfile for faster PR CI (#1328)
* dns-util: Allow running lookup with a given resolver (#1338)
* Add missing internal qa routes (#1336)
* Extract and rename PolyLog to a library for reusability (#1329)
* Fix: Spar integration tests misconfigured on CI (#1343)
* Bump ormolu version (#1366, #1368)
* Update ES upgrade path (#1339) (#1376)
* Bump saml2-web-sso version to latest upstream (#1369)
* Add docs for deriving-swagger2 (#1373)


# [2021-01-15]

## Release Notes

This release contains bugfixes and internal changes.

## Features

* [federation] Add helm chart for the federator (#1317)

## Bug fixes and other updates

* [SCIM] Accept any query string for externalId (#1330)
* [SCIM] Allow at most one identity provider (#1332)

## Internal changes

* [SCIM] Change log level to Warning & format filter logs (#1331)
* Improve flaky integration tests (#1333)
* Upgrade nixpkgs and niv (#1326)


# [2021-01-12]

## Release Notes

This release contains bugfixes and internal changes.

## Bug fixes and other updates

* [SCIM] Fix bug: Deleting a user retains their externalId (#1323)
* [SCIM] Fix bug: Provisioned users can update update to email, handle, name (#1320)

## Internal changes

* [SCIM] Add logging to SCIM ops, invitation ops, createUser (#1322) (#1318)
* Upgrade nixpkgs and add HLS to shell.nix (#1314)
* create_test_team_scim.sh script: fix arg parsing and invite (#1321)


# [2021-01-06]

## Release Notes

This release contains bugfixes and internal changes.

## Bug fixes and other updates

* [SCIM] Bug fix: handle is lost after registration (#1303)
* [SCIM] Better error message (#1306)

## Documentation

* [SCIM] Document `validateSAMLemails` feature in docs/reference/spar-braindump.md (#1299)

## Internal changes

* [federation] Servantify get users by unqualified ids or handles (#1291)
* [federation] Add endpoint to get users by qualified ids or handles (#1291)
* Allow overriding NAMESPACE for kube-integration target (#1305)
* Add script create_test_team_scim.sh for development (#1302)
* Update brig helm chart: Add `setExpiredUserCleanupTimeout` (#1304)
* Nit-picks (#1300)
* nginz_disco: docker building consistency (#1311)
* Add tools/db/repair-handles (#1310)
* small speedup for 'make upload-charts' by inlining loop (#1308)
* Cleanup stack.yaml. (#1312) (#1316)


# [2020-12-21]

## Release Notes

* upgrade spar before brig
* upgrade nginz

## Features

* Increase the max allowed search results from 100 to 500. (#1282)

## Bug fixes and other updates

* SCIM: Allow strings for boolean values (#1296)
* Extend SAML IdP/SCIM permissions to admins (not just owners) (#1274, #1280)
* Clean up SCIM-invited users with expired invitation (#1264)
* move-team: CLI to export/import team data (proof of concept, needs testing) (#1288)
* Change some error labels for status 403 responses under `/identity-providers` (used by team-settings only) (#1274)
* [federation] Data.Qualified: Better field names (#1290)
* [federation] Add endpoint to get User Id by qualified handle (#1281, #1297)
* [federation] Remove DB tables for ID mapping (#1287)
* [federation] servantify /self endpoint, add `qualified_id` field (#1283)

## Documentation

* Integrate servant-swagger-ui to brig (#1270)

## Internal changes

* import all charts from wire-server-deploy/develop as of 2012-12-17 (#1293)
* Migrate code for easier CI (#1294)
* unit test and fix for null values in rendered JSON in UserProfile (#1292)
* hscim: Bump upper bound for servant packages (#1285)
* drive-by fix: allow federator to locally start up by specifying config (#1283)


# 2020-12-15

## Release Notes

As a preparation for federation, this release introduces a mandatory 'federationDomain' configuration setting for brig and galley (#1261)

## Features

* brig: Allow setting a static SFT Server (#1277)

## Bug fixes and other updates

## Documentation

## Internal changes

* Add federation aware endpoint for getting user (#1254)
* refactor brig Servant API for consistency (#1276)
* Feature flags cleanup (#1256)


# 2020-11-24

## Release Notes

* Allow an empty SAML contact list, which is configured at `saml.contacts` in spar's config.
  The contact list is exposed at the `/sso/metadata` endpoint.

## Features

* Make Content-MD5 header optional for asset upload (#1252)
* Add applock team feature (#1242, #1253)
* /teams/[tid]/features endpoint

## Bug fixes

* Fix content-type headers in saml responses (#1241)

## Internal changes

* parse exposed 'tracestate' header in nginz logs if present (#1244)
* Store SCIM tokens in hashed form (#1240)
* better error handling (#1251)


# 2020-10-28

## Features

* Onboard password-auth'ed users via SCIM, via existing invitation flow (#1213)

## Bug fixes and other updates

* cargohold: add compatibility mode for Scality RING S3 implementation (#1217, reverted in 4ce798e8d9db, then #1234)
* update email translations to latest (#1231)

## Documentation

* [brig:docs] Add a note on feature flag: setEmailVisibility (#1235)

## Internal changes

* Upgrade bonanza to geoip2 (#1236)
* Migrate rex to this repository (#1218)
* Fix stack warning about bloodhound. (#1237)
* Distinguish different places that throw the same error. (#1229)
* make fetch.py compatible with python 3 (#1230)
* add missing license headers (#1221)
* More debug logging for native push notifications. (#1220, #1226)
* add libtinfo/ncurses to docs and nix deps (#1215)
* Double memory available to cassandra in demo mode (#1216)


# 2020-10-05

## Release Notes

With this release, the `setCookieDomain` configuration (under `brig`/`config`.`optSettings`) no longer has any effect, and can be removed.

## Security improvements

* Authentication cookies are set to the specific DNS name of the backend server (like nginz-https.example.com), instead of a wildcard domain (like *.example.com). This is achieved by leaving the domain empty in the Set-Cookie header, but changing the code to allow clients with old cookies to continue using them until they get renewed. (#1102)

## Bug Fixes

* Match users on email in SCIM search: Manage invited user by SCIM when SSO is enabled (#1207)

## New Features

* Amount of SFT servers returned on /calls/config/v2 can be limited (default 5, configurable) (#1206)
* Allow SCIM without SAML (#1200)

## Internal changes

* Cargohold: Log more about AWS errors, ease compatibility testing (#1205, #1210)
* GHC upgrade to 8.8.4 (#1204)
* Preparation for APNS notification on iOS 13 devices: Use mutable content for non-voip notifications and update limits (#1212)
* Cleanup: remove unused scim_user table (#1211)


# 2020-09-04

## Release Notes

## Bug Fixes

* Fixed logic related to ephemeral users (#1197)

## New Features

* SFT servers now exposed over /calls/config/v2 (#1177)
* First federation endpoint (#1188)

## Internal changes

* ormolu upgrade to 0.1.2.0 and formatting (#1145, #1185, #1186)
* handy cqlsh make target to manually poke at the database (#1170)
* spar cleanup
* brig user name during scim user parsing (#1195)
* invitation refactor (#1196)
* SCIM users are never ephemeral (#1198)


# 2020-07-29

## Release Notes

* This release makes a couple of changes to the elasticsearch mapping and requires a data migration. The correct order of upgrade is:
  1. [Update mapping](./docs/reference/elastic-search.md#update-mapping)
  1. Upgrade brig as usual
  1. [Run data migration](./docs/reference/elastic-search.md#migrate-data)
  Search should continue to work normally during this upgrade.
* Now with cargohold using V4 signatures, the region is part of the Authorization header, so please make sure it is configured correctly. This can be provided the same way as the AWS credentials, e.g. using the AWS_REGION environment variable.

## Bug Fixes

* Fix member count of suspended teams in journal events (#1171)
* Disallow team creation when setRestrictUserCreation is true (#1174)

## New Features

* Pending invitations by email lookup (#1168)
* Support s3 v4 signatures (and use package amazonka instead of aws in cargohold) (#1157)
* Federation: Implement ID mapping (brig) (#1162)

## Internal changes

* SCIM cleanup; drop table `spar.scim_user` (#1169, #1172)
* ormolu script: use ++FAILURES as it will not evaluate to 0 (#1178)
* Refactor: Simplify SRV lookup logic in federation-util (#1175)
* handy cqlsh make target to manually poke at the database (#1170)
* hscim: add license headers (#1165)
* Upgrade stack to 2.3.1 (#1166)
* gundeck: drop deprecated tables (#1163)


# 2020-07-13

## Release Notes

* If you are self-hosting wire on the public internet, consider [changing your brig server config](https://github.com/wireapp/wire-server/blob/49f414add470f4c5e969814a37bc851e26f6d9a7/docs/reference/user/registration.md#blocking-creation-of-personal-users-new-teams-refrestrictregistration).
* Deploy all services except nginz.
* No migrations, no restrictions on deployment order.

## New Features

* Restrict user creation in on-prem installations (#1161)
* Implement active flag in SCIM for user suspension (#1158)

## Bug Fixes

* Fix setting team feature status in Stern/backoffice (#1146)
* Add missing Swagger models (#1153)
* docs/reference/elastic-search.md: fix typos (#1154)

## Internal changes

* Federation: Implement ID mapping (galley) (#1134)
* Tweak cassandra container settings to get it to work on nixos. (#1155)
* Merge wireapp/subtree-hscim repository under `/libs`, preserving history (#1152)
* Add link to twilio message ID format (#1150)
* Run backoffice locally (#1148)
* Fix services-demo (#1149, #1156)
* Add missing license headers (#1143)
* Test sign up with invalid email (#1141)
* Fix ormolu script (source code pretty-printing) (#1142)


# 2020-06-19

## Release Notes

- run galley schema migrations
- no need to upgrade nginz

## New Features

* Add team level flag for digital signtaures (#1132)

## Bug fixes

* Bump http-client (#1138)

## Internal changes

* Script for finding undead users in elasticsearch (#1137)
* DB changes for federation (#1070)
* Refactor team feature tests (#1136)


# 2020-06-10

## Release Notes

- schema migration for cassandra_galley
- promote stern *after* galley
- promote spar *after* brig
- no need to upgrade nginz

## New Features

* Validate saml emails (#1113, #1122, #1129)

## Documentation

* Add a note about unused registration flow in docs (#1119)
* Update cassandra-schema.cql (#1127)

## Internal changes

* Fix incomplete pattern in code checking email domain (custom extensions) (#1130)
* Enable additional GHC warnings (#1131)
* Cleanup export list; swagger names. (#1126)


# 2020-06-03

## Release Notes

* This release fixes a bug with searching. To get this fix, a new elasticsearch index must be used.
  The steps for doing this migration can be found in [./docs/reference/elastic-search.md](./docs/reference/elastic-search.md#migrate-to-a-new-index)
  Alternatively the same index can be recreated instead, this will cause downtime.
  The steps for the recreation can be found in [./docs/reference/elastic-search.md](./docs/reference/elastic-search.md#recreate-an-index-requires-downtime)

## New Features

* Customer Extensions (not documented, disabled by default, use at your own risk, [details](https://github.com/wireapp/wire-server/blob/3a21a82a1781f0d128f503df6a705b0b5f733d7b/services/brig/src/Brig/Options.hs#L465-L504)) (#1108)
* Upgrade emails to the latest version: small change in the footer (#1106)
* Add new "team event queue" and send MemberJoin events on it (#1097, #1115)
* Change maxTeamSize to Word32 to allow for larger teams (#1105)

## Bug fixes

* Implement better prefix search for name/handle (#1052, #1124)
* Base64 encode error details in HTML presented by Spar. (#1120)
* Bump schemaVersion for Brig and Galley (#1118)

## Internal Changes

* Copy swagger-ui bundle to nginz conf for integration tests (#1121)
* Use wire-api types in public endpoints (galley, brig, gundeck, cargohold) (#1114, #1116, #1117)
* wire-api: extend generic Arbitrary instances with implementation for 'shrink' (#1111)
* api-client: depend on wire-api only (#1110)
* Move and add wire-api JSON roundtrip tests (#1098)
* Spar tests cleanup (#1100)


# 2020-05-15

## New Features

* Add tool to migrate data for galley (#1096)
  This can be used in a more automated way than the backfill-billing-team-member.
  It should be done as a step after deployment.

## Internal Changes

* More tests for OTR messages using protobuf (#1095)
* Set brig's logLevel to Warn while running integration-tests (#1099)
* Refactor: Create wire-api package for types used in the public API (#1090)


# 2020-05-07

## Upgrade steps (IMPORTANT)

* Deploy new version of all services as usual, make sure `enableIndexedBillingTeamMember` setting in galley is `false`.
* Run backfill using
  ```bash
  CASSANDRA_HOST_GALLEY=<IP Address of one of the galley cassandra instaces>
  CASSANDRA_PORT_GALLEY=<port>
  CASSANDRA_KEYSPACE_GALLEY=<GALLEY_KEYSPACE>
  docker run quay.io/wire/backfill-billing-team-members:2.81.18 \
    --cassandra-host-galley="$CASSANDRA_HOST_GALLEY" \
    --cassandra-port-galley="$CASSANDRA_PORT_GALLEY" \
    --cassandra-keyspace-galley="$CASSANDRA_KEYSPACE_GALLEY"
  ```
  You can also run the above using [`kubectl run`](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands#run).
* Set `enableIndexedBillingTeamMember` setting in galley to `true` and re-deploy the same version.

## New Features

* Custom search visibility - limit name search (#1086)
* Add tool to backfill billing_team_member (#1089)
* Index billing team members (#1081, #1091)
* Allow team deletion on stern (#1080)
* Do not fanout very large teams (#1060, #1075)

## Bug fixes

* Fix licenses of db tools (#1088)

## Internal Changes
* Add docs for updating ID Provider (#1074)
* Add comments/docs about hie.yaml (#1037)
* Don't poll from SQS as often (#1082)
* Refactor: Split API modules into public/internal (#1083)
* Manage license headers with headroom instead of licensure (#1084)
* Monitor access to DynamoDB (#1077)
* Make make docker-intermediate command work again (#1079)
* Upgrade Ormolu to 0.0.5.0 (#1078)
* Add (very few) unit tests to galley (#1071)
* Pull brig-index before running the docker ephemeral setup (#1066)


# 2020-04-21

## New Features

* Allow for `report_missing` in `NewOtrMessage`. (#1056, #1062)
* List team members by UserId (#1048)
* Support idp update.  (#1065 for issuer, #1026 for everything else)
* Support synchronous purge-deletion of idps (via query param).  (#1068)

## Bug fixes

* Test that custom backend domains are case-insensitive (#1051)
* Swagger improvements. (#1059, #1054)

## Internal Changes

* Count team members using es (#1046)
* Make delete or downgrade team owners scale (#1029)
* services-demo/demo.sh: mkdir zauth (if not exists) (#1055)
* Use fork of bloodhound to support ES 5.2 (#1050)


# 2020-04-15

## Upgrade steps (IMPORTANT)

1. Update mapping in ElasticSearch (see [./docs/reference/elastic-search.md](./docs/reference/elastic-search.md))
2. Upgrade brig and the other services as usual
3. Migrate data in ElasticSearch (see [./docs/reference/elastic-search.md](./docs/reference/elastic-search.md))

## New features

* Allow `brig-index create` to set ES index settings (#1023)
* Extended team invitations to have name and phone number (#1032)
* Allow team members to be searched by teammates. (#964)
* Better defaults for maxKeyLen and maxValueLen (#1034)

## Bug Fixes

* Fix swagger (#1012, #1031)
* Custom backend lookup by domain is now case-insensitive (#1013)

## Internal Changes

* Federation: resolve opaque IDs at the edges of galley (#1008)
* Qualify all API imports in Galley (#1006)
* types-common: write unit tests for Data.Qualified (#1011)
* Remove subv4 (#1003)
* Add federation feature flag to brig and galley (#1014)
* Add hie.yaml (#1024)
* Improve reproducibility of builds (#1027)
* Update types of some brig endpoints to be federation-aware (#1013)
* Bump to lts-14.27 (#1030)
* Add comments about which endpoints send which events to clients (#1025)
* Minimize dependencies of all brig binaries (#1035)
* Federation: Use status 403 for 'not implemented' (#1036)
* Add endpoint to count team members using ES (#1022)
* Rename brig's userName to userDisplayName to avoid confusion (#1039)
* Upgrade to restund 0.4.14 (#1043)
* Add license headers to all files (#980, #1045)
* Federation: Link related issue IDs (#1041)


# 2020-03-10

## New features

- Remove autoconnect functionality; deprecate end-point. (#1005)
- Email visible to all users in same team (#999)

## Bug fixes

- fix nginx permissions in docker image (#985)

## Significant internal changes

- Update nginx to latest stable (#725)

## Internal Changes

- ormolu.sh: make queries for options more robust (#1009)
- Run hscim azure tests (#941)
- move FUTUREWORK(federation) comment to right place
- stack snapshot 3.0. (#1004, works around 8697b57609b523905641f943d68bbbe18de110e8)
- Fix .gitignore shenanigans in Nix (#1002)
- Update types of some galley endpoints to be federation-aware (#1001)
- Cleanup (#1000)
- Compile nginx with libzauth using nix (#988)
- Move and create federation-related types (#997)
- Tweak ormolu script. (#998)
- Give handlers in gundeck, cannon stronger types (#990)
- Rename cassandra-schema.txt to cassandra-schema.cql (#992)
- Ignore dist-newstyle (#991)
- Refactor: separate HTTP handlers from app logic (galley) (#989)
- Mock federator (#986)
- Eliminate more CPP (#987)
- Cleanup compiler warnings (#984)
- Make ormolu available in builder (#983)


# 2020-02-27

## Hotfix

- Fix encoding bug in SAML SSO (#995)


# 2020-02-06

## New features

* Configure max nr of devices (#969)
* libs/federation-util: SRV resolution (#962)

## Significant internal changes

* Better docs on brig integration yaml (#973)

## Internal changes

- Remove unnecessary LANGUAGE CPP pragmas (#978)
- Introduce code formatting with ormolu (#974, #979)
- Soften a rarely occurring timing issue by slowing things down. (#975)
- debug spar prod (#977)
- Upgrade amazonka (abandon fork) (#976)
- remove unused imports
- Symlink local dist folders in tools to the global one (#971, similar to #904)
- Upgrade to GHC 8.6.5 (LTS 14.12) (#958)
- Refactor: separate http parsing / generation from app logic. (#967)
- spar/integration: no auth required for /sso/settings (#963)


# 2020-02-06

## New features

- SCIM top level extra attrs / rich info (#931)
  - Added to all endpoints under "/scim/v2"
- Create endpoint for default SSO code (#954)
  - New public endpoint:
    - GET "/sso/settings"
  - New private endpoint:
    - PUT "/i/sso/settings"

## Relevant for client developers

- add docs for default sso code (#960)
- Add missing options to services-demo config files (#961)

## Security fixes

- Remove verifcation code from email subject line. (#950)

## Internal changes

- Whitespace (#957)


# 2020-01-30

## API changes (relevant client developers)

- Allow up to 256 characters as handle, dots and dashes too (#953)
  - All handles related endpoints, namely:
    - POST "/users/handles"
    - HEAD "/users/handles/:handle"
    - GET "/users/handles/:handle"
  - now accept this new format of handles
- Refuse to delete non-empty IdPs (412 precondition failed) (#875)
  - DELETE "identity-providers/:idp" will now return 412 if there are users provisioned with that IDP
- Linear onboarding feature: Provide information about custom backends (#946)
  - New public endpoint:
    - GET "/custom-backend/by-domain/:domain"
  - New interal endpoints:
    - PUT "/i/custom-backend/by-domain/:domain"
    - DELETE "/i/custom-backend/by-domain/:domain"

## Bug fixes

- Make sure that someone is SSO user before setting ManagedBy (#947)
- Misc SCIM bugfixes (#948)

## Internal changes

- Fix complexity issue in cassandra query. (#942)
- Remove collectd metrics (finally!) (#940)
- Update `cargoSha256` for cryptobox-c in stack-deps.nix (#949)


# 2020-01-08

## Relevant for self-hosters

- Handle search within team (#921)
- Fixed logic with connection checks (#930)

## Relevant for client developers

- SCIM Fixes Phase 1 + 2 (#926)

## Bug fixes

- Stack nix fixes (#937)


# 2019-12-20

## Relevant for self-hosters

- Access tokens are now sanitized on nginz logs (#920)

## Relevant for client developers

- Conversation roles (#911)
  - Users joining by link are always members (#924) and (#927)

## Bug fixes

- Limit batch size when adding users to conversations (#923)
- Fixed user property integration test (#922)



# 2019-11-28

## Relevant for client developers

- Remove unnecessary fanout team events (#915)


## Bug fixes

- SCIM fixes Phase 0: User creation in correct order (#905)

## Internal changes

- Gundeck: Use polledMapConcurrently (#914)


# 2019-11-06 #901

## Relevant for self-hosters

- New configuration options available (none mandatory). See #895 #900 #869

## Relevant for client developers

- Support HEAD requests for `/sso/initiate-bind` (#878)

## Bug fixes

- Do not send conversation delete events to team members upon team deletion (#897)
- Support SNI for bot registrations (by bumping http-client version) (#899)

## Internal changes

- Make gundeck handle AWS outages better. (#869, #890, #892)
- Improve performance by avoiding unbounded intra-service traffic spikes on team deletions (#900)
- Add optional native push connection throttling (#895)
- New backoffice/stern endpoint (#896)
- SAML: Store raw idp metadata with typed details in c* (#872)
- documentation/script updates


# 2019-09-30 #868

## Relevant for self-hosters
- More information is logged about user actions (#856)

## Relevant for client developers
- Make team member property size configurable (#867)

## Bug fixes
- Fix bugs related to metrics (#853, #866)
- Sneak up on flaky test. (#863)

## Internal Changes
- Derive Generic everywhere (#864)
- Add issue templates (#862)
- Cleanup stern (#845)
- Log warnings only when users are suspended (#854)
- Documentation update for restund and smoketester (#855)


# 2019-09-16 #858

## Relevant for self-hosters

- Documentation changes for Twilio configurations and TURN setup. (#775)

## Relevant for client developers

- Better events for deletion of team conversations (also send `conversation.delete` to team members) (#849)
- Add a new type of authorization tokens for legalhold (for details on legalhold, see https://github.com/wireapp/wire-server/blob/develop/docs/reference/team/legalhold.md) (#761)

## Bug fixes

- Fix swagger docs. (#852)
- Fix intra call in stern (aka customer support, aka backoffice) (#844)

## Internal Changes

- Change feature flags from boolean to custom enum types. (#850)
- Fix flaky integration test. (#848)
- Cleanup: incoherent functions for response body parsing. (#847)
- add route for consistency (#851)


# 2019-09-03 #843

## Relevant for self-hosters

- Option for limiting login retries (#830)
- Option for suspending inactive users (#831)
- Add json logging (#828) (#836)
- Feature Flags in galley options. (#825)

## Relevant for client developers

- Specialize the error cases on conversation lookup. (#841)

## Bug fixes

- Fix is-team-owner logic (don't require email in all cases) (#833)
- Typos in swagger (#826)

## Internal changes

- Fix flaky integration test. (#834)
- Remove `exposed-modules` sections from all package.yaml files. (#832)
- Remove Debug.Trace from Imports. (#838)
- Cleanup integration tests (#839)


# 2019-08-08 #822

## Features

- legalhold (#802), but block feature activation (#823)
- a few shell scripts for self-hosters (#805, #801)
- Release nginz_disco (#759)

## Public API changes

- SSO is disabled by default now; but enabled for all teams that already have an IdP.
- feature flags (starting with legalhold, sso) (#813, #818)
  - new public end-points (#813, #818):
    - get "/teams/:tid/features/legalhold"
    - get "/teams/:tid/features/sso"
  - new internal end-points:
    - get "/i/teams/:tid/features/legalhold"
    - get "/i/teams/:tid/features/sso"
    - put "/i/teams/:tid/features/legalhold"
    - put "/i/teams/:tid/features/sso"
  - new backoffice end-points:
    - get "/teams/:tid/features/legalhold"
    - get "/teams/:tid/features/sso"
    - put "/teams/:tid/features/legalhold"
    - put "/teams/:tid/features/sso"
- Always throw json errors, never plaintext (#722, #814)
- Register IdP: allow json bodies with xml strings (#722)

## Backend-internal changes

- [stern aka backoffice] allow galeb returning a 404 (#820)
- Cleanup logging (#816, #819)
- Canonicalize http request path capture names (#808, #809)
- Galley depends on libsodium too now (#807)
- Add generics instances to common, brig, galley types. (#804)
- Upgrade CQL protocol version to V4 (#763)
- Log last prekey used only at debug level (#785)
- Cleanup (#799)


# 2019-07-08 #798

## Internal Changes

* restund: add EXTRA_CFLAGS  to work on ubuntu 16 (#788)
* Fix flaky unit test. (#770)
* Add upstream references in stack.yaml deps (wai-middleware-prometheus). (#760)
* Cannon analytics (2) (#750)
* fix this file.


# 2019-05-13 #756

## Documentation changes

* Group provisioning (#748)
* Instructions for running load tests (#738)
* Twilio configuration (#733)

## Bug fixes

Cannon no longer reports 500s in the prometheus metrics when establishing websocket connections. (#751, #754)

## Features

Per-installation flag: Allow displaying emails of users in a team (code from #724, see description in #719)

## Internal Changes

Docker image building improvements (#755)

## Changes (potentially) requiring action for self-hosters

Config value `setEmailVisibility` must be set in brig's config file (if you're not sure, `visible_to_self` is the preferred default)


# 2019-05-02 #746

## Documentation changes

* Improved Cassandra documentation in `docs/README.md`
* Improved documentation on SCIM storage in `docs/README.md`
* Improved documentation on SCIM Tokens in `docs/reference/provisioning/scim-token.md`

## Bug fixes

* Sanitize metric names to be valid prometheus names in metrics-core
* Add missing a `.git` suffix on gitlab dependencies in stack.yaml
* Time bounds checks now allow 60s of tolerance; this is helpful in cases of drifting clocks (#730)

## Features

* Services now provide Prometheus metrics on `/i/metrics`
* Garbage Collection and memory statistics are available alongside other prometheus metrics

## Internal Changes

* Alpine Builder is no longer built with `--profile`
* SCIM users now have an additional wire-specific schema attached.

## Changes (potentially) requiring action
* `/i/monitoring` is *DEPRECATED*. Please use prometheus metrics provided by `/i/metrics` instead.
* On password reset the new password must be different than the old one
* Stern is now available as a new tool for performing adminstrative tasks via API (#720)
* SCIM handler errors are now reported according to SCIM error schema (#575)


# 2019-04-09 #710

## API changes

- Do not allow provisioning saml users if SCIM is configured (#706)

## Documentation changes

- Docs for user deletion via SCIM. (#691)
- Docs for jump-to-definition with Emacs (#693)
- Add missing config options in demo (#694)
- Move the connections doc, add haddocks (#695)

## Bug fixes

- Fix templating in outgoing SMSs. (#696)
- Saml implicit user creation no longer chokes on odd but legal names. (#702)
- Fix: user deletion via scim (#698)

## Internal changes

- Remove redundant cassandra write in renewCookie (#676)
- Add Prometheus middleware for wire-services (#672)
- Improve logging of spar errors (#654)
- Upgrade cql-io-1.1.0 (#697)
- Switch metrics-core to be backed by Prometheus (#704)
- Refactorings:
    - #665, #687, #685, #686

## Changes (potentially) requiring action for self-hosters

- Switch proxy to use YAML-only config (#684)


# 2019-03-25 #674

## API changes

  * SCIM delete user endpoint (#660)
  * Require reauthentication when creating a SCIM token (#639)
  * Disallow duplicate external ids via SCIM update user (#657)

## Documentation changes

  * Make an index for the docs/ (#662)
  * Docs: using scim with curl. (#659)
  * Add spar to the arch diagram. (#650)

## Bug fixes

  * ADFS-workaround for SAML2 authn response signature validation. (#670)
  * Fix: empty objects `{}` are valid TeamMemberDeleteData. (#652)
  * Better logo rendering in emails (#649)

## Internal changes

  * Remove some unused instances (#671)
  * Reusable wai middleware for prometheus (for Galley only for now) (#669)
  * Bump cql-io dep from merge request to latest release. (#661)
  * docker image building for all of the docker images our integration tests require. (#622, #668)
  * Checking for 404 is flaky; depends on deletion succeeding (#667)
  * Refactor Galley Tests to use Reader Pattern (#666)
  * Switch Cargohold to YAML-only config (#653)
  * Filter newlines in log output.  (#642)


# 2019-02-28 #648

## API changes

  * Support for SCIM based rich profiles (#645)
    * `PUT /scim/v2/Users/:id` supports rich profile
    * `GET /users/:id/rich-info` to get the rich profile id

## Internal changes

  * Gundeck now uses YAML based config
  * Brig templates can now be easily customized and have been updated too
  * Misc improvements to our docs and build processes


# 2019-02-18 #646

## API changes

  * n/a

## Bug fixes

  * SAML input sanitization (#636)

## Internal changes

  * helper script for starting services only without integration tests (#641)
  * Scim error handling (#640)
  * Gundeck: cleanup, improve logging (#628)


# 2019-02-18 #634

## API changes

  * Support for SCIM (#559, #608, #602, #613, #617, #614, #620, #621, #627)
    - several new end-points under `/scim` (see hscim package or the standards for the details; no swagger docs).
    - new end-point `put "/i/users/:uid/managed-by"` for marking scim-managed users (no swagger docs)
  * Add support for excluding certain phone number prefixes (#593)
    - several new end-points under `/i/users/phone-prefixes/` (no swagger docs)
  * Fix SAML2.0 compatibility issues in Spar (#607, #623)

## Bug fixes

  * Update swagger docs (#598)

## Internal changes

  * Architecture independence, better use of make features, more docs. (#594)
  * Fix nginz docker image building (#605)
  * Enable journaling locally and fix integration tests (#606)
  * Use network-2.7 for more informative "connection failed" errors (#586)
  * Use custom snapshots (#597)
  * Add module documentation for all Spar modules (#611)
  * Change the bot port in integration tests to something less common (#618)
  * Spar metrics (#604, #633)
  * Extend the list of default language extensions (#619)
  * Fix: do not have newlines in log messages. (#625)


# 2019-01-27 #596

## API changes

  * Track inviters of team members (#566)
  * New partner role. (#569, #572, #573, #576, #579, #584, #577, #592)
  * App-level websocket pongs. (#561)

## Bug fixes

  * Spar re-login deleted sso users; fix handling of brig errors. (#588)
  * Gundeck: lost push notifications with push-all enabled.  (#554)
  * Gundeck: do not push natively to devices if they are not on the whitelist.  (#554)
  * Gundeck: link gundeck unit tests with -threaded.  (#554)

## Internal changes

  * Get rid of async-pool (unliftio now provides the same functionality) (#568)
  * Fix: log multi-line error messages on one line. (#595)
  * Whitelist all wire.com email addresses (#578)
  * SCIM -> Scim (#581)
  * Changes to make the demo runnable from Docker (#571)
  * Feature/docker image consistency (#570)
  * add a readme, for how to build libzauth. (#591)
  * better support debian style machines of different architecturs (#582, #587, #583, #585, #590, #580)


# 2019-01-10 #567

## API changes

  * `sigkeys` attribute on POST|PUT to `/clients` is now deprecated and ignored (clients can stop sending it)
  * `cancel_callback` parameter on GET `/notifications` is now deprecated and ignored (clients can stop sending it)
  * The deprecated `POST /push/fallback/<notif>/cancel` is now removed.
  * The deprecated `tokenFallback` field returned on `GET /push/tokens` is now removed.

## Bug fixes

  * Size-restrict SSO subject identities (#557)
  * Propagate team deletions to spar (#519)
  * Allow using `$arg_name` in nginz (#538)

## Internal changes

  * Version upgrades to GHC 8.4 (LTS-12), nginx 14.2, alpine 3.8 (#527, #540)
  * Code refactoring, consitency with Imports.hs (#543, #553, #552)
  * Improved test coverage on spar (#539)
  * Use yaml configuration in cannon (#555)

## Others

  * Docs and local dev/demo improvements


# 2018-12-07 #542

## API changes

  * New API endpoint (`/properties-values`) to get all properties keys and values

## Bug fixes

  * Proper JSON object encapsulation for `conversation.receipt-mode-update` events (#535)
  * Misc Makefile related changes to improve dev workflow

## Internal changes

  * Gundeck now pushes events asynchronously after writing to Cassandra (#530)

# Others

  * Improved docs (yes!) with (#528)


# 2018-11-28 #527

## Bug fixes

  * Spar now handles base64 input more leniently (#526)

  * More lenient IdP metadata parsing (#522)

## Internal changes

  * Refactor Haskell module imports (#524, #521, #520)

  * Switch Galley, Brig to YAML-only config (#517, #510)

  * Better SAML error types (#522)

  * Fix: gundeck bulkpush option. (#511)


# 2018-11-16 #515

## Bug Fixes

  * Fix: spar session cookie (#512)

  * SSO: fix cookie handling around binding users (#505)

## Internal Changes

  * partial implementation of SCIM (without exposure to the spar routing table)

  * Always build benchmarks (#486)

  * Fix: gundeck compilation (#506)

  * Fix: use available env var for docker tag in dev make rule.  (#509)

  * Use Imports.hs in Brig, Spar, Galley (#507)

  * update dependencies docs (#514)


# 2018-10-25 #500

## New Features

  * SSO: team member deletion, team deletion do not require
    the user to have chosen a password.  (Needed for
    SAML-authenticated team co-admins.)  #497

  * SSO: `sso-initiate-bind` end-point for inviting ("binding")
    existing users to SAML auth.  #496

  * SSO: shell script for registering IdPs in wire-teams.
    (`/deploy/services-demo/register_idp.sh`)  #489

  * Allow setting a different endpoint for generating download links.
    #480

  * Allow setting specific ports for SMTP and use different image for
    SMTP.  #481

  * Route calls/config in the demo to brig.  #487

## Internal Changes

  * Metrics for spar (service for SSO).  #498

  * Upgrade to stackage lts-11.  #478

  * Upgrade cql-io library.  #495

  * Allow easily running tests against AWS.  #482


# 2018-10-04 #477

## Highlights

  * We now store the `otr_muted_status` field per conversation,
    suitable for supporting more notifications options than just "muted/not
    muted". The exact meaning of this field is client-dependent.  #469

  * Our schema migration tools (which you are probably using if
    you're doing self-hosting) are more resilient now. They have longer
    timeouts and they wait for schema consistency across peers before
    reporting success.  #467

## Other changes

  * Building from scratch on macOS is now a tiny bit easier.  #474

  * Various Spar fixes, breaking changes, refactorings, and what-not. Please
    refer to the commit log, in particular commits c173f42b and
    80d06c9a.

  * Spar now only accepts a [subset][TLS ciphersuite] of available TLS
    ciphers. See af8299d4.

[TLS ciphersuite]: https://hackage.haskell.org/package/tls-1.4.1/docs/src/Network-TLS-Extra-Cipher.html#ciphersuite_default
