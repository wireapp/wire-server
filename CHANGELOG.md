<!--

# [2020-next]

## Release Notes

## Features

## Bug fixes and other updates

## Documentation

## Internal changes

-->
# [2020-03-23]

## Release Notes

Note that you should never skip a release when upgrading Wire. If you are upgrading to this release, make sure you have deployed the previous releases in order beforehand.

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


# [2020-03-21]

## Release Notes

This release will automtically migrate a table after deployment.
Note that you should never skip a release when upgrading Wire.

## Internal changes

* Migrate spar external id table (#1400, #1413, #1415, #1417)

# [2020-03-02]

## Bug fixes and other updates

* Return PubClient instead of Client from /users/list-clients (#1391)

## Internal changes

* Federation: Add qualified endpoints for prekey management (#1372)


# [2020-02-25]

## Bug fixes and other updates

* Pin kubectl image in sftd chart (#1383)
* Remove imagePullPolicy: Always for reaper chart (#1387)


## Internal changes

* Use mu-haskell to implement one initial federation request across backends (#1319)
* Add migrate-external-ids tool (#1384)

# [2020-02-16]

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

# [2020-01-15]

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
