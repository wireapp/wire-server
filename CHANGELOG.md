# 2019-02-14 #629

## API changes

  * Support for SCIM (#559, #608, #602, #613, #617, #614, #620, #621, #627)
    - several new end-points under `/scim` (see hscim package or the standards for the details; no swagger docs).
    - new end-point `put "/i/users/:uid/managed-by"` for marking scim-managed users (no swagger docs)
  * Add support for excluding certain phone number prefixes (#593)
    - several new end-points under `/i/users/phone-prefixes/` (no swagger docs)
  * Fix SAML2.0 compatibility issues in Spar (#607, #623)

## Bug fixes

  * update swagger docs (#598)

## Internal changes

  * architecture independance, better use of make features, more docs. (#594)
  * Fix nginz docker image building (#605)
  * Enable journaling locally and fix integration tests (#606)
  * Use network-2.7 for more informative "connection failed" errors (#586)
  * Use custom snapshots (#597)
  * Add module documentation for all Spar modules (#611)
  * Change the bot port in integration tests to something less common (#618)
  * Spar metrics (#604)
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
  * deprecated `POST /push/fallback/<notif>/cancel` is removed
  * deprecated `tokenFallback` field returned on `GET /push/tokens` is removed

## Bug fixes

  * Size-restrict SSO subject identities (#557)
  * Propagate team deletions to spar (#519)
  * allow using $arg_name in nginz (#538)

## Internal changes

  * Version upgrades to GHC 8.4 (LTS-12), nginx 14.2, alpine 3.8 (#527, #540)
  * Code refactoring, consitency with Imports.hs (#543, #553, #552)
  * improved test coverage on spar (#539)
  * Use yaml configuration in cannon (#555)

## Others

  * Docs and local dev/demo improvements


# 2018-12-07 #542

## API changes

  * New API endpoint (`/properties-values`) to get all properties keys and values

## Bug Fixes

  * Proper JSON object encapsulation for `conversation.receipt-mode-update` events (#535)
  * Misc Makefile related changes to improve dev workflow

## Internal Changes

  * Gundeck now pushes events asynchronously after writing to Cassandra (#530)

# Others

  * Improved docs (yes!) with (#528)

# 2018-11-28 #527

## Bug Fixes

  * spar now handles base64 input more leniently (#526)

  * More lenient IdP metadata parsing (#522)

## Internal Changes

  * Refactor Haskell module imports (#524, #521, #520)

  * Switch Galley, Brig to YAML-only config (#517, #510)

  * Better SAML error types (#522)

  * Fix: gundeck bulkpush option. (#511)


# 2018-11-16  #515

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


# 2018-10-25  #500

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


# 2018-10-04  #477

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
