.. _release-notes:

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

Chart Release 2.118.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-11-15

Release Notes
-------------

Release notes
~~~~~~~~~~~~~

-  In case you use a multi-datacentre cassandra setup (most likely you
   do not), be aware that now
   `LOCAL_QUORUM <https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html>`__
   is in use as a default. (#1884)
-  Deploy galley before brig. (#1857)
-  Upgrade webapp version to 2021-11-01-production.0-v0.28.29-0-d919633
   (#1856)

API changes
~~~~~~~~~~~

-  Remove locale from publicly facing user profiles (but not from the
   self profile) (#1888)

Features
~~~~~~~~

-  End-points for configuring self-deleting messages. (#1857)

Bug fixes and other updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Ensure that all endpoints have a correct handler in prometheus
   metrics (#1919)
-  Push events when AppLock or SelfDeletingMessages config change.
   (#1901)

Documentation
~~~~~~~~~~~~~

-  Federation: Document how to deploy local builds (#1880)

Internal changes
~~~~~~~~~~~~~~~~

-  Add a 'filterNodesByDatacentre' config option useful during cassandra
   DC migration (#1886)
-  Add ormolu to the direnv, add a GH Action to ensure formatting
   (#1908)
-  Turn placeholder access effects into actual Polysemy effects. (#1904)
-  Fix a bug in the IdP.Mem interpreter, and added law tests for IdP
   (#1863)
-  Introduce fine-grained error types and polysemy error effects in
   Galley. (#1907)
-  Add polysemy store effects and split off Cassandra specific
   functionality from the Galley.Data module hierarchy (#1890, #1906).
   (#1890)
-  Make golden-tests in wire-api package a separate test suite (for
   faster feedback loop during development). (#1926)
-  Separate IdPRawMetadataStore effect from IdP effect (#1924)
-  Test sending message to multiple remote domains (#1899)
-  Use cabal to build wire-server (opt-in) (#1853)

Federation changes
~~~~~~~~~~~~~~~~~~

-  Close GRPC client after making a request to a federator. (#1865)
-  Do not fail user deletion when a remote notification fails (#1912)
-  Add a one-to-one conversation test in getting conversations in the
   federation API (#1899)
-  Notify remote participants when a user leaves a conversation because
   they were deleted (#1891)

Chart Release 2.117.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-10-29

Release Notes
-------------

Release notes
~~~~~~~~~~~~~

-  Upgrade SFT to 2.1.15 (#1849)
-  Upgrade team settings to Release:
   `v4.2.0 <https://github.com/wireapp/wire-team-settings/releases/tag/v4.2.0>`__
   and image tag: 4.2.0-v0.28.28-1e2ef7 (#1856)
-  Upgrade Webapp to image tag: 20021-10-28-federation-m1 (#1856)

API changes
~~~~~~~~~~~

-  Remove ``POST /list-conversations`` endpoint. (#1840)
-  The member.self ID in conversation endpoints is qualified and
   available as "qualified_id". The old unqualified "id" is still
   available. (#1866)

Features
~~~~~~~~

-  Allow configuring nginz so it serve the deeplink for apps to discover
   the backend (#1889)
-  SFT: allow using TURN discovery using 'turnDiscoveryEnabled' (#1519)

Bug fixes and other updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Fix an issue related to installing the SFT helm chart as a sub chart
   to the wire-server chart. (#1677)
-  SAML columns (Issuer, NameID) in CSV files with team members. (#1828)

Internal changes
~~~~~~~~~~~~~~~~

-  Add a 'make flake-PATTERN' target to run a subset of tests multiple
   times to trigger a failure case in flaky tests (#1875)
-  Avoid a flaky test to fail related to phone updates and improve
   failure output. (#1874)
-  Brig: Delete deprecated ``GET /i/users/connections-status`` endpoint.
   (#1842)
-  Replace shell.nix with direnv + nixpkgs.buildEnv based setup (#1876)
-  Make connection DB functions work with Qualified IDs (#1819)
-  Fix more Swagger validation errors. (#1841)
-  Turn ``Galley`` into a polysemy monad stack. (#1881)
-  Internal CI tooling improvement: decrease integration setup time by
   using helmfile. (#1805)
-  Depend on hs-certificate master instead of our fork (#1822)
-  Add internal endpoint to insert or update a 1-1 conversation. This is
   to be used by brig when updating the status of a connection. (#1825)
-  Update helm to 3.6.3 in developer tooling (nix-shell) (#1862)
-  Improve the ``Qualified`` abstraction and make local/remote tagging
   safer (#1839)
-  Add some new Spar effects, completely isolating us from saml2-web-sso
   interface (#1827)
-  Convert legacy POST conversations/:cnv/members endpoint to Servant
   (#1838)
-  Simplify mock federator interface by removing unnecessary arguments.
   (#1870)
-  Replace the ``Spar`` newtype, instead using ``Sem`` directly. (#1833)

Federation changes
~~~~~~~~~~~~~~~~~~

-  Remove remote guests as well as local ones when "Guests and services"
   is disabled in a group conversation, and propagate removal to remote
   members. (#1854)
-  Check connections when adding remote users to a local conversation
   and local users to remote conversations. (#1842)
-  Check connections when creating group and team conversations with
   remote members. (#1870)
-  Server certificates without the "serverAuth" extended usage flag are
   now rejected when connecting to a remote federator. (#1855)
-  Close GRPC client after making a request to a remote federator.
   (#1865)
-  Support deleting conversations with federated users (#1861)
-  Ensure that the conversation creator is included only once in
   notifications sent to remote users (#1879)
-  Allow connecting to remote users. One to one conversations are not
   created yet. (#1824)
-  Make federator's default log level Info (#1882)
-  The creator of a conversation now appears as a member when the
   conversation is fetched from a remote backend (#1842)
-  Include remote connections in the response to
   ``POST /list-connections`` (#1826)
-  When a user gets deleted, notify remotes about conversations and
   connections in chunks of 1000 (#1872, #1883)
-  Make federated requests to multiple backends in parallel. (#1860)
-  Make conversation ID of ``RemoteConversation`` unqualified and move
   it out of the metadata record. (#1839)
-  Make the conversation creator field in the
   ``on-conversation-created`` RPC unqualified. (#1858)
-  Update One2One conversation when connection status changes (#1850)

Chart Release 2.116.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-10-01


Release Notes
-------------

Release notes
~~~~~~~~~~~~~

-  Deploy brig before galley (#1811, #1818)
-  You can now configure if personal accounts are allowed to initiate conference calls
   in ``brig.yaml``. ``enabled`` is both the default and
   the previous behavior, so if you are not sure if you need this, it's safe to do nothing. If you want to change the default, read
   `/docs/reference/config-options.md#conference-calling-1 <https://github.com/wireapp/wire-server/blob/develop/docs/reference/config-options.md#conference-calling-1>`__
   (#1811, #1818)
-  Only if you are an early adopter of multi-team IdP issuers on release
   `2021-09-14 <https://github.com/wireapp/wire-server/releases/tag/v2021-09-14>`__:
   note that the `query parameter for IdP creation has
   changed <https://github.com/wireapp/wire-server/pull/1763/files#diff-bd66bf2f3a2445e08650535a431fc33cc1f6a9e0763c7afd9c9d3f2d67fac196>`__.
   This only affects future calls to this one end-point. (#1763)
-  For wire.com cloud operators: reminder to also deploy nginz. (No
   special action needed for on-premise operators) (#1773)

API changes
~~~~~~~~~~~

-  Add endpoint ``POST /connections/:domain/:userId`` to create a
   connection (#1773)
-  Deprecate ``PUT /conversations/:cnv/access`` endpoint (#1807)
-  Deprecate ``PUT /conversations/:cnv/message-timer`` endpoint (#1780)
-  Deprecate ``PUT /conversations/:cnv/members/:usr`` endpoint (#1784)
-  Deprecate ``PUT /conversations/:cnv/receipt-mode`` endpoint (#1797)
-  Add endpoint ``GET /connections/:domain/:userId`` to get a single
   connection (#1773)
-  Add ``POST /list-connections`` endpoint to get connections (#1773)
-  Add qualified endpoint for updating conversation access (#1807)
-  Add qualified endpoint for updating message timer (#1780)
-  Add qualified endpoint for updating conversation members (#1784)
-  Add qualified endpoint for updating receipt mode (#1797)
-  Add endpoint ``PUT /connections/:domain/:userId`` to update a
   connection (#1773)

Features
~~~~~~~~

-  Helm charts to deploy
   `ldap-scim-bridge <https://github.com/wireapp/ldap-scim-bridge>`__
   (#1709)
-  Per-account configuration of conference call initiation (details:
   /docs/reference/config-options.md#conference-calling-1) (#1811,
   #1818)

Bug fixes and other updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  An attempt to create a 3rd IdP with the same issuer was triggering an
   exception. (#1763)
-  When a user was auto-provisioned into two teams under the same pair
   of ``Issuer`` and ``NameID``, they where directed into the wrong
   team, and not rejected. (#1763)

Documentation
~~~~~~~~~~~~~

-  Expand documentation of ``conversations/list-ids`` endpoint (#1779)
-  Add documentation of the multi-table paging abstraction (#1803)
-  Document how to use IdP issuers for multiple teams (#1763)
-  All named Swagger schemas are now displayed in the Swagger UI (#1802)

Internal changes
~~~~~~~~~~~~~~~~

-  Abstract out multi-table-pagination used in list conversation-ids
   endpoint (#1788)
-  Testing: rewrite monadic to applicative style generators (#1782)
-  Add a test checking that creating conversations of exactly the size
   limit is allowed (#1820)
-  Rewrite the DELETE /self endpoint to Servant (#1771)
-  Fix conversation generator in mapping test (#1778)
-  Polysemize spar (#1806, #1787, #1793, #1814, #1792, #1781, #1786,
   #1810, #1816, #1815)
-  Refactored a few functions dealing with conversation updates, in an
   attempt to make the conversation update code paths more uniform, and
   also reduce special cases for local and remote objects. (#1801)
-  Merged http2-client fixes as mentioned in the comments of #1703
   (#1809)
-  Some executables now have a runtime dependency on ncurses (#1791)
-  Minor changes around SAML and multi-team Issuers.

   -  Change query param to not contain ``-``, but ``_``. (This is
      considered an internal change because the feature has been release
      in the last release, but only been documented in this one.)
   -  Haddocks.
   -  Simplify code.
   -  Remove unnecessary calls to cassandra. (#1763)

-  Clean up JSON Golden Tests (Part 6) (#1769)
-  Remove explicit instantiations of ErrorDescription (#1794)
-  Remove one flaky integration test about ordering of search results
   (#1798)
-  Report all failures in JSON golden tests in a group at once (#1746)
-  Convert the ``PUT /conversations/:cnv/access`` endpoint to Servant
   (#1807)
-  Move /connections/\* endpoints to Servant (#1770)
-  Servantify Galley’s DELETE /i/user endpoint (#1772)
-  Convert the ``PUT /conversations/:cnv/message-timer`` endpoint to
   Servant (#1780)
-  Convert the ``PUT /conversations/:cnv/members/:usr`` endpoint to
   Servant (#1796)
-  Convert the ``PUT /conversations/:cnv/receipt-mode`` endpoint to
   Servant (#1797)
-  Expose wire.com internal EJDP process to backoffice/stern. (#1831)
-  Update configurable boolean team feature list in backoffice/stern.
   (#1829)
-  Handle upper/lower case more consistently in scim and rich-info data.
   (#1754)

Federation changes
~~~~~~~~~~~~~~~~~~

-  Add value for verification depth of client certificates in federator
   ingress (#1812)
-  Document federation API conventions and align already existing APIs
   (#1765)
-  Notify remote users when a conversation access settings are updated
   (#1808)
-  Notify remote users when a conversation member role is updated
   (#1785)
-  Notify remote users when a conversation message timer is updated
   (#1783)
-  Notify remote users when a conversation is renamed (#1767)
-  Make sure that only users that are actually part of a conversation
   get notified about updates in the conversation metadata (#1767)
-  Notify remote users when a conversation receipt mode is updated
   (#1801)
-  Implement updates to remote members (#1785)
-  Make conversation ID of the on-conversation-created RPC unqualified
   (#1766)
-  4 endpoints for create/update/get/list connections designed for
   remote users in mind. So far, the implementation only works for local
   users (actual implementation will come as a follow-up) (#1773)
-  The returned ``connection`` object now has a ``qualified_to`` field
   with the domain of the (potentially remote) user. (#1773)
-  Add migration for remote connection table (#1789)
-  Remove a user from remote conversations upon deleting their account
   (#1790)
-  Remove elasticsearch specific details from the search endpoint
   (#1768)
-  Added support for updating self member status of remote conversations
   (#1753)



Chart Release 2.115.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-09-14


Release Notes
-------------

API changes
~~~~~~~~~~~

-  Remove the long-deprecated ``message`` field in ``POST /connections``
   (#1726)
-  Add ``PUT /conversations/:domain/:cnv/name`` (#1737)
-  Deprecate ``PUT /conversations/:cnv/name`` (#1737)
-  Add ``GET & PUT /conversations/:domain/:cnv/self`` (#1740)
-  Deprecate ``GET & PUT /conversations/:cnv/self`` (#1740)
-  Remove endpoint ``GET /conversations/:domain/:cnv/self`` (#1752)
-  The ``otr_muted`` field in ``Member`` and ``MemberUpdate`` has been
   removed. (#1751)
-  Removed the ability to update one’s own role (#1752)

Features
~~~~~~~~

-  Disallow changing phone number to a black listed phone number (#1758)
-  Support using a single IDP with a single EntityID (aka issuer ID) to
   set up two teams. Sets up a migration, and makes teamID + EntityID
   unique, rather than relying on EntityID to be unique. Required to
   support multiple teams in environments where the IDP software cannot
   present anything but one EntityID (E.G.: DualShield). (#1755)

Documentation
~~~~~~~~~~~~~

-  Added documentation of federation errors (#1674)
-  Better swagger schema for the Range type (#1748)
-  Add better example for Domain in swagger (#1748)

Internal changes
~~~~~~~~~~~~~~~~

-  Introduce new process for writing changelogs (#1749)
-  Clean up JSON golden tests (Part 4, Part 5) (#1756, #1762)
-  Increased timeout on certificate update tests to 10s (#1750)
-  Fix for flaky test in spar (#1760)
-  Rewrite the ``POST /connections`` endpoint to Servant (#1726)
-  Various improvements and fixes around SAML/SCIM (#1735)

Federation changes
~~~~~~~~~~~~~~~~~~

-  Avoid remote calls to get conversation when it is not found locally
   (#1749)
-  Federator CA store and client credentials are now automatically
   reloaded (#1730)
-  Ensure clients only receive messages meant for them in remote convs
   (#1739)



Chart Release 2.114.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-09-08


Release Notes
-------------

API Changes
~~~~~~~~~~~

-  Add ``POST /conversations/list/v2`` (#1703)
-  Deprecate ``POST /list-conversations`` (#1703)

Features
~~~~~~~~

-  Bump SFTD to 2.0.127 (#1745)

Bug fixes and other updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Remove support for managed conversations in member removal (#1718)
-  Update the webapp to correct labeling on CBR calling (#1743)

Documentation
~~~~~~~~~~~~~

-  Document backend internals for user connections (#1717)
-  Open Update spar braindump and explain idp deletion (#1728)

Internal changes
~~~~~~~~~~~~~~~~

-  Integration test script now displays output interactively (#1700)
-  Fixed a few issues with error response documentation in Swagger
   (#1707)
-  Make mapping between (team) permissions and roles more lenient
   (#1711)
-  The ``DELETE /conversations/:cnv/members/:usr`` endpoint rewritten to
   Servant (#1697)
-  Remove leftover auto-connect internal endpoint and code (#1716)
-  Clean up JSON golden tests (#1729, #1732, #1733)
-  Make regenerated golden tests’ JSON output deterministic (#1734)
-  Import fix for snappy linker issue (#1736)

Federation changes
~~~~~~~~~~~~~~~~~~

-  Added client certificate support for server to server authentication
   (#1682)
-  Implemented full server-to-server authentication (#1687)
-  Add an endpoint for removing a qualified user from a local
   conversation (#1697)
-  Refactored remote error handling in federator (#1681)
-  The update conversation membership federation endpoint takes
   OriginDomainHeader (#1719)
-  Added new endpoint to allow fetching conversation metadata by
   qualified ids (#1703)



Chart Release 2.113.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-08-27

Upstream release notes for wire-server-deploy playbooks: https://github.com/wireapp/wire-server-deploy/blob/master/CHANGELOG.md#2021-08-27


Release Notes
-------------

API Changes
-----------

* Deprecate `DELETE /conversations/:cnv/members/:usr` (#1697)
* Add `DELETE /conversations/:cnv/members/:domain/:usr` (#1697)

Features
--------

Bug fixes and other updates
---------------------------

* Fix case sensitivity in schema parser in hscim library (#1714)
* [helm charts] resolve a rate-limiting issue when using certificate-manager alongside wire-server and nginx-ingress-services helm charts (#1715)

Documentation
-------------

* Improve Swagger for `DELETE /conversations/:cnv/members/:usr` (#1697)

Internal changes
----------------

* Integration test script now displays output interactively (#1700)
* Fixed a few issues with error response documentation in Swagger (#1707)
* Make mapping between (team) permissions and roles more lenient (#1711)
* The `DELETE /conversations/:cnv/members/:usr` endpoint rewritten to Servant (#1697)
* Remove leftover auto-connect internal endpoint and code (#1716)
* Bump wire-webapp (#1720)
* Bump team-settings (#1721)
* Bump account-pages (#1666)

Federation changes
------------------

* Added client certificate support for server to server authentication (#1682)
* Implemented full server-to-server authentication (#1687)
* Add an endpoint for removing a qualified user from a local conversation (#1697)


Chart Release 2.112.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-08-16

Release Notes
-------------

This is a routine release requiring only the routine upgrade steps.

API Changes
-----------

* Add `POST /conversations/list-ids` (#1686)
* Deprecate `GET /converstations/ids` (#1686)

Features
--------

* Client functions for the hscim library (#1694, #1699, #1702, https://hackage.haskell.org/package/hscim)

Bug fixes and other updates
---------------------------

* Change http response code for `missing-legalhold-consent`. (#1688)
* Remove old end-point for changing email

Federation changes (alpha feature, do not use yet)
--------------------------------------------------

* Add new API to list paginated qualified conversation ids (#1686)

Documentation
-------------

* Fix swagger: mark name in UserUpdate as optional (#1691, #1692)

Internal changes
----------------

* Replaced uses of `UVerb` and `EmptyResult` with `MultiVerb` (#1693)
* Added a mechanism to derive `AsUnion` instances automatically (#1693)
* Integration test coverage (#1696, #1704)

Chart Release 2.111.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-08-02

Release Notes
-------------

If you want to set the default for file sharing in all teams to `disabled`, search for "File Sharing" in https://github.com/wireapp/wire-server/tree/develop/docs/reference/config-options.md.

Release Notes for Wire.com Cloud operators
------------------------------------------

Upgrade nginz (#1658)

API Changes
-----------

Features
--------

* A new team feature for classified domains is available (#1626):
  - a public endpoint is at `GET /teams/:tid/features/classifiedDomains`
  - an internal endpoint is at `GET /i/teams/:tid/features/classifiedDomains`
* Extend feature config API (#1658)
* `fileSharing` feature config (#1652, #1654, #1655)
* `conferenceCalling` feature flag (#1683)
* Add user_id to csv export (#1663)

Bug fixes and other updates
---------------------------

* New, hardened end-point for changing email (68b4db08)
* Fix: CSV export is missing SCIM external id when SAML is also used (#1608)
* Fix: sso_id field in user record (brig) was not always filled correctly in cassandra (#1334)
* Change http response code for `missing-legalhold-consent` from 412 to 403 (#1688)

Documentation
-------------

* Improved Swagger documentation for endpoints with multiple responses (#1649, #1645)

Internal changes
----------------

* Improvements to local integration test setup when using buildah and kind (#1667)
* The servant-swagger dependency now points to the current upstream master (#1656)
* Improved error handling middleware (#1671)
* Refactor function createUser for readability (#1670)
* Removed explicit implementation for user HEAD endpoints (#1679)
* Improved test coverage for error responses (#1680)
* Introduced `MultiVerb` endpoints in Servant API (#1649).

Federation changes (alpha feature, do not use yet)

* Validate server TLS certificate between federators (#1662)
* A clarification is added about listing your own domain as a classified domain (#1678)
* Added a `QualifiedCapture` type to Servant for qualified paths (#1669)
* Renamed `DomainHeader` type to `OriginDomainHeader` (#1689)
* Added golden tests for protobuf serialisation / deserialisation (#1644).



Chart version 2.110.0
=====================

Upstream release notes: https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-07-09

.. warning::

   This release requires a manual change in your galley configuration: `galley.settings.conversationCodeURI` in `values/wire-server/values.yaml` was had to be set to `${WEBAPP}/join` before this release, and must be set to `${ACCOUNTS}/conversation-join` from now on, where `${WEBAPP}` is the url to the webapp and `${ACCOUNTS}` is the url to the account pages.

API Changes
-----------

* Several public team feature endpoints are removed (their internal and
  Stern-based counterparts remain available):
  - `PUT /teams/:tid/features/sso`
  - `PUT /teams/:tid/features/validateSAMLemails`
  - `PUT /teams/:tid/features/digitalSignatures`
* All endpoints that fetch conversation details now also include a new key
  `qualified_id` for a qualified conversation ID (#1640)
* New endpoint `POST /list-conversations` similar to `GET /conversations`, but which will also return your own remote conversations (if federation is enabled). (#1591)

Features
--------

* Change `settings.conversationCodeURI` in galley.yaml (#1643).
* [Federation] RPC to propagate messages to other backends (#1596).
* [Federation] Fetch remote user's clients when sending messages (#1635).
* [Federation] Actually propagate messages to other backends (#1638).
* [Federation] Support sending messages to remote conversations (#1609).
* [Federation] Guard against path traversal attacks (#1646).

Internal changes
----------------

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

Internal Federation API changes
-------------------------------

* Breaking change on InwardResponse and OutwardResponse in router.proto for improved error handling (#1637)
  * Note: federation should not be in use anywhere yet, so this should not have any impact
* Added golden tests for protobuf serialisation / deserialisation (#1644).

Documentation
-------------

* Fix validation errors in Swagger documentation (#1625).

Bug fixes and other updates
---------------------------

* Restore old behaviour for parse errors in request bodies (#1628, #1629).
* Allow to change IdP Issuer name to previous name (#1615).


Chart version 2.109.0
=====================

See https://github.com/wireapp/wire-server/blob/develop/CHANGELOG.md#2021-06-23

Release notes
-------------

.. warning::

   This release went out with a bug that makes breaks certain error messages in the log in process.
   This has been rectified in 2.110.0

API Changes
------------

* [Federation] Add qualified endpoint for sending messages at `POST /conversations/:domain/:cnv/proteus/messages` (#1593, #1614, #1616).

Security fixes
--------------
* Fix for https://github.com/wireapp/wire-webapp/security/advisories/GHSA-382j-mmc8-m5rw  (#1613)

Bug fixes
----------
* [helm] Allow sending messages upto 40 MB by default (#1614)
* Fix for https://github.com/wireapp/wire-webapp/security/advisories/GHSA-382j-mmc8-m5rw  (#1613)
* Update wire-webapp version (#1613)
* Update team-settings version (#1598)
* Allow optional password field in RmClient (#1604, #1607)
* Add endpoint: Get name, id with for CodeAccess conversations (#1592)
* demote logging failed invitations to a warning, rather than an error. Server operators can't act on these errors in any way (#1586)


Documentation
-------------

* Add descriptive comments to `ConversationMemberUpdate` (#1578)
* initial few anti-patterns and links about cassandra (#1599)

Internal changes
----------------

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

More federation changes (inactive code)
---------------------------------------

* Add getUserClients RPC (and thereby allow remote clients lookup) (#1500)
* minor refactor: runFederated (#1575)
* Notify remote backends when users join (#1556)
* end2end test getting remote conversation and complete its implementation (#1585)
* Federation: Notify Remote Users of Being Added to a New Conversation (#1594)
* Add qualified endpoint for sending messages (#1593, #1614)
* Galley/int: Expect remote call when creating conv with remotes (#1611)



Chart version 2.108.0
=====================

Release notes
-------------

This release doesn't require any extra considerations to deploy.

Features
--------
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

Bug fixes and other updates
---------------------------
* Fix MIME-type of asset artifacts
* Add some missing charts (#1533)

Internal changes
----------------
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



Chart version 2.107.0
=====================

Release notes
-------------


.. warning::

   This release introduces a notion of "consent" to
   legalhold (LH).  If you are using LH on your site, follow the
   instructions in
   https://github.com/wireapp/wire-server/blob/814f3ebc251965ab4492f5df4d9195f3b2e0256f/docs/reference/team/legalhold.md#whitelisting-and-implicit-consent
   after the upgrade.  **Legalhold will not work as expected until you
   change `galley.conf` as described!**

.. warning::

   This release introduces changes to the way `NameID` is
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


Features
--------
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

Bug fixes and other updates
---------------------------
 - Fix: NewTeamMember vs. UserLegalHoldStatus (increase robustness against rogue clients) (#1496)

Documentation
-------------
 - Fixes a typo in the wire-api documentation (#1513)


Chart version 2.106.0
=======================

Release notes
-------------


.. warning::

   From this version on; we do not ship DynamoDB-compatible service anymore. Instead, we ship with a built-in prekey distribution strategy
   that no longer depends on an external locking service. (#1416, #1476).

   If you want to keep using DynamoDB, you must set ``brig.randomPrekeys`` to ``false`` in your ``values.yaml`` explicitly.




Features
-------------
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

Bug fixes and other updates
----------------------------
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

Documentation
-------------
 - [docs] Document how to run multi-backend tests for federation (#1436)
 - [docs] Fix CHANGELOG: incorrect release dates (#1435)
 - [docs] Update release notes with data migration for SCIM (#1442)
 - [docs] Fixes a k8s typo in the README (#1475)
 - [docs] Document testing strategy and patterns (#1472)



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
