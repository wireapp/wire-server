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
