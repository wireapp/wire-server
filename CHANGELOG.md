# [2024-02-13] (Chart Release 4.41.0)

# [2024-02-12] (Chart Release 4.40.0)

## Release notes


* The settings `setDisabledAPIVersions` (brig) and `disabledAPIVersions` (in cannon, cargohold, galley, gundeck, proxy, and spar) are now required.
  The default defined in `charts/<service>/values.yaml` is set to `[ development ]` and disables all development API versions.
  For more information see <https://docs.wire.com/developer/reference/config-options.html#disabling-api-versions> (#3772)

* The mls team feature now has a lock status. If the current installation has default settings defined in wire-server's `values.yaml`, the `lockStatus` has to be added, e.g.:

  ```yaml
  mls:
    defaults:
      status: enabled
      config:
        protocolToggleUsers: []
        defaultProtocol: mls
        allowedCipherSuites: [1]
        defaultCipherSuite: 1
        supportedProtocols: [proteus, mls] # must contain defaultProtocol
      lockStatus: locked
  ``` (#3681)

* * Replace fake-sqs with ElasticMQ

  ElasticMQ is an actively maintained project, fake-sqs hasn't seen a commit since
  2018. This is not expected to have any noticeable effect on deployments that
  don't have any extra configurations for the SQS queues. If the fake-aws-sqs
  chart had configured custom queue names, they have couple of extra limitations:
  - The queue names must only contain alphanumeric characters and hyphens.
  - The FIFO queue names must end in `.fifo`. (#3750)

* Upgrade team-settings version to 4.15.1-v0.31.19-0-ee1dbce (#2180)

* Upgrade webapp to 2023-12-11-production.0-v0.31.17-0-1e91445

  Beside using up-to-date versions in Helm charts is generally beneficial,
  this version also provides multi-ingress support. (#3803)

* Upgrade webapp version to 2024-01-22-production.1-v0.31.17-0-7f83dbe (#2302)


## API changes


* Un-verified users can no longer upload assets (#3604)

* * Remove IP request header from add-client endpoint.
  * Remove longitude and latitude response fields from add-client, list-clients, and get-client endpoints.

  These are not considered breaking changes, since clients are not using this information. (#3792)

* Create new API version V6 and finalize V5 (#3815)

* Block changes of userDisplayName, userHandle in mlsE2EI-enabled teams on the backend without SCIM; report `"managed_by" == "scim"` in `GET /self`, but only there (#3827)

* The federation API can now be versioned. Multiple versions of an RPC can be defined on the same path. After version negotiation, the federation client now sets the `X-Wire-API-Version` header, and federator propagates it to the destination service. (#3762)

* Improved formatting of federation errors. No extra copy of the response body, and nested errors are now part of the JSON structure, not quoted inside the message. (#3742)

* New endpoint for replacing MLS key packages in bulk: `PUT /mls/key-packages/self/:client`. It replaces all existing key packages that match the given ciphersuites with the new key packages provided in the body. (#3654)


## Features


* The lifetime of conversation guest links is now configurable (#3796)

* Events for a member update, join and leave are not sent to everyone in the team any longer. Only team admins get them. (#3703)

* Allowlist for who on cloud can connect to on-prem:
  - Internal API to configure allowlist
  - Restrict federated user search according to team federation policy
  (#3697, #3732, #3758)

* The mls team feature now has a lock status (#3681)

* add a uniform timeout to the integration test-suite set by the environment variable TEST_TIMEOUT_SECONDS with a default of 10 seconds if the variable isn't set (#3692)

* Apply team-level federation policies when establishing and updating user connections (#3774)

* Introduce a feature flag that controls whether the limited event fanout should be used when a team member is deleted (#3797)

* Send a `conversation.member-leave` event to team admins for each conversation the deleted team member used to be part of (#3790)

* Allow the configuration of TLS-secured connections to Cassandra. TLS is used
  when a certificate is provided. This is either done with
  `--tls-ca-certificate-file` for cli commands or the configuration attribute
  `cassandra.tlsCa` for services. In Helm charts, the certificate is provided as
  literal PEM string; either as attribute `cassandra.tlsCa` (analog to service
  configuration) or by a reference to a secret (`cassandra.tlsCaSecretRef`.) (#3587)

* [SFT chart] Add option to enable serviceMonitor to scrape prometheus metrics (#3770)


## Bug fixes and other updates


* galley's DB migrations fixed (#3680)

* The X509 client identity parser supports a new format: `wireapp://{userid}!{deviceid}@{host}` (#3808)

* Updated `demo-smtp` Helm chart from deprecated docker image namshi/smtp to ixdotai/smtp (#3791)

* External partners search restriction enforced by backend (#3708)

* File upload size is now limited to 100 MiB (#3752)

* Fix a bug where non-team conversation members that are remote would not get a `conversation.member-leave` event (#3745, #3764)

* Enforce external partner permissions on the backend (#3788)

* Various improvements around LH policy conflict detection:
  * Fix LH policy conflict detection logic when posting messages
  * Better policy conflict error messages (distinguish between old clients and missing consent)
  * Add first LH scaffolding and tests to `/integration`
  * Annotate some API functions in `/integration` with links to openapi3 docs (#3773)

* Do not match on the `Accept` header for service provider endpoints with no response body (#3766)

* Guests should not be added to conversations that are under legalhold (#3853)

* Intra-service calls from brig to galley's public API are now aware of disabled API versions (#3863)

* fix Helm pretty-printer for disabledAPIVersions (#3877)

* Adjust the requested memory and upper bound limit of `nginz` pods in the related Helm chart. (We experienced OOM errors with the old settings.) (#3821)

* don't use shell when communicating with mls-test-cli, move flaking brig tests over to new integration testsuite (#3701)

*  set notificationTimeOut to 28 days, make it legible (#3714)

* Update coturn image with bugfix to its prestop-hook from https://github.com/wireapp/coturn/pull/10 to allow coturn pods to terminate once their traffic has drained. (#3872)

* Extra remove proposals were being sent when a user was removed from a conversation (#3672)

* Remove client check for subconversations (#3677)

* Ensure that SCIM can find users even after the team admin has changed the SAML issuer for the user. (#3747)

* addClient used the internal brig API in the integration testsuite when it should use the public one (#3869)

* Ensure that HTTP 1.1 connections are grafully closed

  To fix this warp had to be patched to fix the bug upstream: https://github.com/yesodweb/wai/pull/958 (#3775)


## Documentation


* Fix missing code sections on docs.wire.com, notably on "configuring TLS" page. (#3839)

* Swagger generation no longer adds tags containing information about federation calls.

  Added the federation calling graph to the Federation API Conventions page. (#3674)

* Backend-to-backend OpenApi Docs added (#3666)

* Documentation for creating a new API version updated (#3817)

* Update documentation of MLS group ID (#3705)

* Turn long summaries in openapi documentation into descriptions (#3706)

* update the build instructions for wire-server (#3854)


## Internal changes


* stern/backoffice `PUT /teams/{teamId}/features/conferenceCalling` fixed (#3723)

* Removed client ID conversion round trip (#3727)

* Migrate to Servant the Galley conversation internal endpoints (#3718)

* The development API version is now disabled by default (#3772)

* Attempt to fix flaky integration test `provider.service.delete` (#3689)

* The fedcalls tool no longer walks the Swagger/OpenAPI structure when generating call graphs. These graphs are now generated directly from the Servant API types. (#3674, #3691)

* Increased ingress payload size from 256k to 512k (#3756)

* Request tracing across federated requests (#3765)

* upgrade nixpkgs to upgrade haskell-language-server (#3650)

* upgrade the GHC version to GHC 9.4 (#3679)

* Removed APNS_VOIP code. (APNS_VOIP is a native push notification channel which we aren't using anymore.) (#3695)

* Improve error logs (#3782)

* Migrating tests for Cargohold to the new `integration` test suite. (#3741)

* Fix calendar integration setting in backoffice / stern (#3761)

* `Reply-Nonce` is added to `Access-Control-Expose-Headers` (#3729)

* Add custom feature flag; only supported for some on-prem installations; locked & disabled by default (#3779)

* Improved how tests are automatically extracted from the `integration` test suite.

  The test extractor parser has been improved to handle block comments, and to more strictly check for Haddock documentation for each test. (#3749)

* Additional logging on user/team suspension (#3795)

* cleanup the haskell-pins
  - remove many pins
  - remove many overrides
  - restructure the files such that it's easier to see what is going on (#3814)

* Version of rusty-jwt-tools bumped to v0.8.0 (#3805)

* Feature enforceFileDownloadLocation lockstatus can be set with basic auth on staging (#3802)

* Version of rusty-jwt-tools bumped to v0.8.5 (#3820)

* Translate integration tests: manually add / delete LH device (#3830)

* adds a new executable, hs-run, to quickly run haskell scripts (#3716)

* Represent client IDs as Word64 internally (#3713)

* Allow to install the coturn chart multiple times in multiple namespaces on the same cluster. (#3698)

* For some rust packages (cryptobox and libzauth-c), we now use crate2nix as a build tool, rather than the more coarse and FOD-based nixpkgs `rustPlatform.buildRustPackage` approach. (#3686)

* Delete `shell.nix`. It has been broken for quite some time. The supported way to get a development nix environment is to use direnv. (#3726)

* Deploy a backend with federation API V0 while setting up services for local testing (#3719)

* Improve integration test coverage (#3757)

* Increase timeout for waiting for SQS notifications in galley's integration tests (#3699)

* Simplify process spawning of dynamic backends in integration tests (#3759)

* More robust consuming of MLS messages: the behaviour of `sendAndConsumeMessage` and `sendAndConsumeCommitBundle` is changed to actually wait for those messages on the client's websocket (#3671)

* Update group state after application message (#3678)

* bump the nixpkgs version to allow updating curl (#3781)

* Simplify the definition of the servant notification API (#3685)

* Start refactoring code into subsystems, first subsystem being the NotificationSubsystem. (#3786)

* Remove apply-refact from CI image

  This gets rid of GHC in the image, making the image smaller. (#3712)

* Refactor getOptions (#3707)

* Restored Brig memory quota to 512mb down from 1gb. (prev bump #3751) (#3806)

* Add tool to analyse test results in junit/ant xml format (#3652)

* updated annotation for enabling Topology Aware Routing to service.kubernetes.io/topology-mode for k8s 1.27+ (#3878)

* replace runAsNonRoot with runAsUser and runAsGroup 1000 (#3826)

* Update SFTD default to 4.0.10 and its nginx to 1.25.3. (#3768)

* add a Makefile target to make it possible to upload a bom of all services to s3 on every CI run (#3744)

* Upload bill-of-material (BOM) files directly to the Dependency Tracker via REST.
  This eases the life of the security team and prevents cluttering our release
  artifact page. (#3810)

* Passively migrate user passwords from scrypt to argon2id.

  By passively we mean that whenever a user re-enters their passwords, if it was hashed using scrypt, it is then rehashed using argon2id and stored as such.
  If that user has a legacy short password (under 8 characters in length), it does not migrate to argon2id. (#3720)


## Federation changes


* Define a few tests for adding members to an MLS conversation when unreachable backends are involved (#3673)

* Make sure that remote users can be added to both a Proteus and an MLS conversation when other users are unreachable (#3688)


# [2023-10-23] (Chart Release 4.39.0)

## Release notes


* New field for Supported protocols in Galley's MLS feature config

  Galley will refuse to start if the list `supportedProtocols` does not contain
  the value of the field `defaultProtocol`. Galley will also refuse to start if
  MLS migration is enabled and MLS is not part of `supportedProtocols`.

  The default value for `supportedProtocols` is:
  ```
  [proteus, mls]
  ``` (#3374)


## API changes


* The JSON schema of `NonConnectedBackends` has changed to have its single field now called `non_connected_backends`. (#3518)

* Remove de-federation (to avoid a scalability issue). (#3582)

* Replace the placeholder self conversation id with the qualified conversation id for welcome events. (#3335)

* Add new endpoint `DELETE /mls/key-packages/self/:client` (#3295)

* Introduce an endpoint for deleting a subconversation (#2956, #3119, #3123)

* Remove MLS endpoints from API v4 and finalise it (#3545)

* Add new endpoint `GET /conversations/one2one/:domain/:uid` to fetch the MLS 1-1 conversation with another user (#3345)

* Introduce a subconversation GET endpoint (#2869, #2995)

* Add `GET /conversations/:domain/:cid/subconversations/:id/groupinfo` endpoint to fetch the group info object for a subconversation (#2932)

* Introduce v5 development version (#3527)

* It is now possible to use `PUT /conversation/:domain/:id/protocol` to transition from Mixed to MLS (#3334)

* Report a failure to add remote users to an MLS conversation (#3304)

* The key package API has gained a `ciphersuite` query parameter, which should be the hexadecimal value of an MLS ciphersuite, defaulting to `0x0001`. The `ciphersuite` parameter is used by the claim and count endpoints. For uploads, the API is unchanged, and the ciphersuite is taken directly from the uploaded key package. (#3454)

* Add MLS migration feature config (#3299)

* Switch to MLS draft 20. The following endpoints are affected by the change:

   - All endpoints with `message/mls` content type now expect and return draft-20 MLS structures.
   - `POST /conversations` does not require `creator_client` anymore.
   - `POST /mls/commit-bundles` now expects a "stream" of MLS messages, i.e. a sequence of TLS-serialised messages, one after the other, in any order. Its protobuf interface has been removed.
   - `POST /mls/welcome` has been removed. Welcome messages can now only be sent as part of a commit bundle.
   - `POST /mls/message` does not accept commit messages anymore. All commit messages must be sent as part of a commit bundle. (#3172)

* Key packages and leaf nodes with x509 credentials are now supported (#3532)


## Features


* Add reason field to conversation.member-leave (#3640)

* Support deleting a remote subconversation (#2964)

* Introduce support for resetting a subconversation (#2956)

* Introduce a "mixed" conversation protocol type. A conversation of "mixed" protocol functions as a Proteus converation as well as a MLS conversations. It's intended to be used for migrating conversations from Proteus to MLS. (#3258)

* Added support for post-quantum ciphersuite 0xf031. Correspondingly, MLS groups with a non-default ciphersuite are now supported. The first commit in a group determines the group ciphersuite. (#3454)

* Remove conversation size limit for MLS conversations (#3468)

* Added support for MSL 1-1 conversations (#3360)

* MLS application messages for older epochs are now rejected (#3438)

* The public key in an x509 credential is now checked against that of the client (#3542)

* Add federated endpoints to get subconversations (#2952)

* Add Helm chart (`rabbitmq-external`) to interface RabbitMQ instances outside of the Kubernetes cluster. (#3626)

* Removing or kicking a user from a conversation also removes the user's clients from any subconversation. (#2942)

* Add support for subconversations in `POST /mls/commit-bundles` (#2932)

* Implement endpoint for leaving a subconversation (#2969, #3080, #3085, #3107)


## Bug fixes and other updates


* Fix nix derivations for rust packages (#3628)

* Ensure benchmarking dependencies are provided by nix development environment (#3628)

* Disable a guest user from creating a group conversation (#3622)

* Adding users to a conversation now enforces that all federation domains that will be in the conversation are federated with each other. (#3514)

* Fix ES migration script. (#3558)

* Fixed add user to conversation when one of the other participating backends is offline (#3585)

* Create a new http2 connection in every federator client request instead of using a shared connection. (#3602)

* list-clients returns with partial success even if one of the remote backends is unreachable (#3611)

* Defederation notifications, federation.delete and federation.connectionRemoved, now deduplicate the user list so that we don't send them more notifications than required. (#3515)

* Fix memory and TCP connection leak in brig, galley, caroghold and background-worker. (#3663)

* Fix bug where notifications for MLS messages were not showing up in all notification streams of clients (#3610)

* Map the MLS self-conversation creator's key package reference in Brig (#3055)

* This fixes a bug where a remote member is removed from a conversation while their backend is unreachable, and the backend does not receive the removal notification once it is reachable again. (#3537)

* Welcome messages are not sent anymore to the creator of an MLS group on the first commit (#3392)


## Documentation


* Fix: support api versions other than v0 in swagger docs. (#3619)

* Updating the route documentation from Swagger 2 to OpenAPI 3. (#3570)

* Elaborate on internal user creation in prod (#3596)

* Adding a testing config entry to the PR guidelines. (#3624)


## Internal changes


* remove leaving clients immediately from subconversations (#3096)

* Servantify internal end-points: brig/teams (#3634)

* add conversation type to group ID serialisation (#3344)

* Do not cache federation remote configs on non-brig services (#3612)

* JSON derived schemas have been changed to no longer pre-process record fields to drop prefixes that were required to disambiguate fields.
  Prefix processing still exists to drop leading underscores from field names, as we are using prefixed field names with `makeLenses`.
  Code has been updated to use `OverloadedRecordDot` with the changed field names. (#3518)

* Updating the route documentation library from swagger2 to openapi3.

  This also introduced a breaking change in how we track what federation calls each route makes.
  The openapi3 library doesn't support extension fields, and as such tags are being used instead in a similar way. (#3570)

* - Extending the information returned in errors for Federator. Paths and response bodies, if available, are included in error logs.
  - Prometheus metrics for outgoing and incoming federation requests added. They can be enabled by setting `metrics.serviceMonitor.enabled`, like in other charts. (#3556)

* CLI tool to consume messages from a RabbitMQ queue (#3589, #3655)

* Removed user and client threshold fields from mls migration feature. (#3364)

* Include timestamp in s3 upload path for test logs (#3621)

* Migrating the following routes to the Servant API form.

  POST /provider/services
  GET /provider/services
  GET /provider/services/:sid
  PUT /provider/services/:sid
  PUT /provider/services/:sid/connection
  DELETE /provider/services/:sid
  GET /providers/:pid/services
  GET /providers/:pid/services/:sid
  GET /services
  GET /services/tags
  GET /teams/:tid/services/whitelisted
  POST /teams/:tid/services/whitelist (#3554)

* Provider API has been migrated to servant (#3547)

* background-worker: Get list of domains from RabbitMQ instead of brig for pushing backend notifications (#3588)

* Avoid including MLS application messages in the sender client's event stream. (#3379)

* Avoid empty pushes when chunking pushes in galley (#PR_NOT_FOUND)

* Introduce a Galley DB table for subconversations (#2869)

* Support mapping MLS group IDs to subconversations (#2869)

* change version and conversation type to 16 bit in group ID serialisation (#3353)

* Brig does not perform key package ref mapping anymore. Claimed key packages are simply removed from the `mls_key_packages` table. The `mls_key_package_refs` table is now unused, and will be removed in the future. (#3172)

* Add intermediate "mixed" protocol for migrating from Proteus to MLS (#3292)

* - Do not perform client checks for add and remove proposals in mixed conversations
  - Restrict protocol updates to team conversations
  - Disallow MLS application messages in mixed conversations
  - Send remove proposals when users leave mixed conversations (#3303)

* New cron job to save data usable to watch the progress of the Proteus to MLS migration in S3 bucket.

  **IMPORTANT:** This cron job is _not_ meant for general use! It can leak data about one team to other teams. (#3579)

* Subconversations are now created on their first commit (#3355)

* Propagate messages in MLS subconversations (#2937)

* Move some MLS tests to new integration suite (#3286)

* Check validity of notification IDs in the notification API (#3550)

* stern: Optimize RAM usage of /i/users/meta-info (#3522)

* Additional integration test for federated connections (#3538)

* The bot API is now migrated to servant (#3540)

* `rusty-jwt-tools` is upgraded to version 0.5.0 (#3572)

* Refactored schema version tracking from manually managed to automatic. (#3643)

* Avoid unnecessary error logs on service shutdown (#3592)

* Introduce an effect for subconversations (#2869)

* Via the update path update the key package of the committer in epoch 0 of a subconversation (#2975)

* Add more tests for joining a subconversation (#2974)

* Added `/tools/db/repair-brig-clients-table` to clean up after the fix in #3504 (#3507)

* Distinguish between update and upsert cassandra commands (follow-up to #3504) (#3513)

* Truncate `galley.mls_group_member_client` table and drop `galley.member_client` table.

  The data in `mls_group_member_client` could contain nulls from client testing in prod. So, its OK to truncate it.
  The `member_client` table is unused. (#3648)

* All integration tests can generate XML reports.

  To generate the report in brig-integration, galley-integration,
  cargohold-integration, gundeck-integration, stern-integration and the new
  integration suite pass `--xml=<outfile>` to generate the XML file.

  For spar-integration and federator-integration pass `-f junit` and set
  `JUNIT_OUTPUT_DIRECTORY` and `JUNIT_SUITE_NAME` environment variables. The XML
  report will be generated at `$JUNIT_OUTPUT_DIRECTORY/junit.xml`.

  (#3568, #3633)


## Federation changes


* Add subconversation ID to onMLSMessageSent request payload. (#3270)

* Derive group ID from qualified conversation ID and, if applicable,
  subconversation ID.

  Retire mapping from group IDs to conversation IDs. (group_id_conv_id)

  Remove federation endpoints
  - on-new-remote-conversation,
  - on-new-remote-subconversation, and
  - on-delete-mls-conversation
  which were used to synchronise the group to conversation mapping. (#3309)

* Reorganise the federation API such that queueing notification endpoints are separate from synchronous endpoints. Also simplify queueing federation notification endpoints. (#3647)

* Introduce an endpoint for resetting a remote subconversation (#2964)

* Split federation endpoint into on-new-remote-conversation and on-new-remote-subconversation
  Call on-new-remote-subconversation when a new subconversation is created
  Call on-new-remote-subconversation for all existing subconversations when a new backend gets involved
  Call on-new-remote-subconversation when a subconversation is reset (#2997)

* federator: Allow setting TCP connection timeout for HTTP2 requests

  The helm chart defaults it to 5s which should be best for most installations. (#3595)

* Constrain which federation endpoints can be used via the queueing federation client (#3629)

* There is a breaking change in the "on-mls-message-sent" federation endpoint due to queueing. Now that there is retrying because of queueing, the endpoint can no longer respond with a list of unreachable users. (#3629)

* Remote MLS messages get queued via RabbitMQ (#PR_NOT_FOUND)


# [2023-08-16] (Chart Release 4.38.0)

## Bug fixes and other updates

* Fix syntax error in cassandra update to `brig.client`. (#3508)


# [2023-08-16] (Chart Release 4.37.0)

## API changes


* Conversation creation endpoints can now return `unreachable_backends` error responses with status code 533 if any of the involved backends are unreachable. The conversation is not created in that case. (#3486)


## Bug fixes and other updates


* Make sure cassandra updates do not re-introduce removed content. (#3504)


## Federation changes


* Return `unreachable_backends` error when some backends of newly added users to a conversation are not reachable (#3496)


# [2023-08-11] (Chart Release 4.36.0)

## Release notes


* **federation only** Introduce background-worker

  This release introduces a new component: background-worker. This is currently
  only used to federation-related tasks. Enabling federation in
  the wire-server helm chart automatically installs this component.

  When federation is enabled, wire-server will require running RabbitMQ. The helm
  chart in `rabbitmq` can be used to install RabbitMQ. Please refer to the
  documentation at https://docs.wire.com to install RabbitMQ in Kubernetes. These
  new configurations are required:

  ```yaml
  brig:
    config:
      rabbitmq:
        host: rabbitmq
        port: 5672
        vHost: /
    secrets:
      rabbitmq:
        username: <YOUR_USERNAME>
        password: <YOUR_PASSWORD>
  galley:
    config:
      rabbitmq:
        host: rabbitmq
        port: 5672
        vHost: /
    secrets:
      rabbitmq:
        username: <YOUR_USERNAME>
        password: <YOUR_PASSWORD>
  background-worker:
    config:
      rabbitmq:
        host: rabbitmq
        port: 5672
        vHost: /
        adminPort: 15672
    secrets:
      rabbitmq:
        username: <YOUR_USERNAME>
        password: <YOUR_PASSWORD>
  ```

  The above are the default values (except for secrets, which do not have
  defaults), if they work they are not required to be configured.
  (#3276, #3314, #3333, #3366, #3383, #3391)

* **Federation only** A few helm values related to federation have been renamed, no action is required if federation was disabled.
  If federation was enabled these values must be renamed in the wire-server chart:
  - tags.federator -> tags.federation
  - brig.enableFederator -> brig.enableFederation
  - galley.enableFederator -> galley.enableFederation
  - cargohold.enableFederator -> galley.enableFederation

  So, an old config which looked like this:

  ```yaml
  tags:
    federator: true
  brig:
    enableFederator: true
  galley:
    enableFederator: true
  cargohold:
    enableFederator: true
  ```

  would now look like this:

  ```yaml
  tags:
    federation: true
  brig:
    enableFederation: true
  galley:
    enableFederation: true
  cargohold:
    enableFederation: true
  ```
   (#3236)

* **Federation only** From this release on, remote connections can be configured via an
  internal REST API; the remote connections configured in the
  values.yaml file(s) will be honored for a transition period, but will
  be ignored starting in some future release.

  YOU NEED TO UPDATE YOUR BRIG HELM VALUES BEFORE DEPLOYING THIS RELEASE.

  Add the following to brig:

  ```
  brig:
    config:
      optSettings:
        setFederationStrategy: allowNone # [allowAll | allowDynamic | allowNone]
        setFederationDomainConfigsUpdateFreq: 10 # seconds
  ```

  `allowNone` is equivalent to `allowList` with empty list; `allowAll`
  remains the same as before; `allowDynamic` is `allowList`, but the
  list is now stored in cassandra, not the config file.

  If your federator config values contain something like this:

  ```
      federationStrategy:
        allowedDomains:
        - red.example.com
        - blue.example.com
  ```

  you need to make sure that the following lines are part of your brig
  config (after the upgrade and until you have loaded the data into
  casssandra, federation with those domains won't possible if you forget
  this):

  ```
  brig:
    config:
      optSettings:
        setFederationDomainConfigs:
        - domain: red.example.com
          search_policy: full_search
        - domain: blue.example.com
          search_policy: no_search
  ```

  The search policy for a remote backend can be:
  - `no_search`: No users are returned by federated searches. default.
  - `exact_handle_search`: Only users where the handle exactly matches are returned.
  - `full_search`: Additionally to exact_handle_search, users are found by a freetext search on handle and display name.

  Once the new release is deployed, you need to copy all the data from
  the config files into `brig.federation_remotes` in cassandra [internal
  CRUD
  API](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/brig/);
  look for `/i/federation/remotes`).

  Once the upgrade has been deployed *and* cassandra has been filled
  with the temporary contents of
  `brig.config.optSettings.setFederationDomainConfigs`, it is safe to
  remove the latter and the above lines from the federator config.

  [See also.](http://docs.wire.com/understand/configure-federation.html#if-your-instance-has-been-federating-before) (#3260, #3384, #3389)

* Upgrade team-settings version to 4.15.0-v0.31.16-0-8138d2e (#2180)

* Upgrade webapp version to 2023-07-13-production.0-v0.31.16-0-a9b67c6 (#2302)

* Update email templates from https://github.com/wireapp/wire-emails (#3386)

* Removed brig configuration value from gundeck. (#3404)


## API changes


* Updating conversation meta-data APIs to be fault tolerant of unavailable federation servers. (#3229)

* Adding users in Proteus will only succeed if all federated backends hosting the
  users are available. Otherwise, the endpoint will fail with a Federation error,
  enumerating all unavailable domains. (#3449)

* Added a new notification event type, "federation.delete". (#3397)
  This event contains a single domain for a remote server that the local server is de-federating from.
  This notification is sent twice during de-federation. Once before and once after cleaning up and removing references to the remote server from the local database.

* list unavailable backends as JSON on federation-unreachable-domains-error
  - extend `federation-unreachable-domains-error` by `FederationErrorData`
  - add `domains` field in `FederationErrorData`, containing the list of failing
    domains
  - deprecate `domain` field in `FederationErrorData` which now contains the first
    element of `domains` (#3407)

* Throw when remote users to be added to an MLS conversation are unreachable (#3322)

* The `connection-update` internal Brig endpoint now has a different JSON format for its request body. See the swagger documentation for details. (#3458)

* Client objects have gained an optional `last_active` field. Whenever a client fetches notifications via `GET /notifications`, as long as it provides a client parameter, the `last_active` field of that client is updated, and set to the current timestamp, rounded to the next multiple of a week. (#3409)

* The `POST /conversations` endpoint now in case of the Proteus protocol gives a 503 error response listing unreachable backends in case there were any, instead of a 2xx response by adding only members from reachable backends. (#3479)

* User objects have gained a `supported_protocols` field. Users can set it to any subset of `["proteus", "mls"]` using `PUT /self/supported-protocols`. There is also a new endpoint `GET /users/:domain/:id/supported-protocols`. The backend does not assign any semantics to this field, but it is intended to be used to coordinate migration to MLS across the clients of a user, as well as between two users participating in a 1-1 conversation. (#3326)

* Several federation Galley endpoints have a breaking change in their response types: "leave-conversation", "update-conversation" and "send-mls-message". They have been extended with information related to unreachable users. (#3248)


## Features


* Add federation options to the `coturn` Helm chart including DTLS support. The options themselves are strongly inspired by the `restund` Helm chart. (#3283)

* Let cargohold redirect to different s3 download endpoints according to a `multiIngress` configuration. This is part of a larger multi-ingress story where one backend can pretend to be multiple ones by using different domains for different users. (#3264)

* Introduce `nginx_conf.additional_external_env_domains` (*nginz* and *cannon*) setting to configure CORS headers for multiple domains. (#3368)

* Add configuration options to setup instances of the `nginx-ingress-services` chart to act as additional ingresses (with sourrounding infrastructure) to provide additional domains for the same backend. (#3375)

* Nonce base 64 encoding is now unpadded (#3255)

* `MlsE2EIdConfig` does now contain an ACME discovery URL and `verificationExpiration` is now a duration. (#3237, #3244)

* Functionality to determine the federation status between federating remote backends (#3290)

* Prevent conversation creation if any two federated backends are not connected to each other (#3382)

* Improve gundeck performance: notifications to multiple recipients are stored in a normalized manner. (#3403)

* When a proteus message is send and a remote user's backend is offline, the message will be enqueued and reported as `failed_to_confirm_clients` (#3460, #3474)

* Check if remote backends are connected on adding conversation members (#3483)

* In a setting where remote participants are included in a freshly created Proteus conversation, the backend now sends a conversation.create and a conversation.member-join event per user once all remote participants are confirmed.  This fixes a bug where remote conv members would get false entries in the member lists in these events. (#3359)

* Enable indexed billing members by default and remove the feature flag (#3434)

* stern/backoffice: read, update, delete domain login redirects to custom backends (#3471)


## Bug fixes and other updates


* If role is not set ([], null, or field missing) in scim-put-user, do not change role to default in brig (#3488)

* Do not accept federation traffic from not-federating backends (#3484)

* Bump coturn default image to upstream coturn 4.6.2 + custom Wire code including a bugfix for a bug that resulted in unstable operation during higher load. (#3250)

* Get the correct domain for DPoP access token generation (#3255)

* Correct http host is passed to proxy request (#3263)

* Use backend domain for DPoP access token request (#3267)

* The DPoP access token is now base64 encoded (once) (#3269)

* Fix `nginx.conf` for local integration tests (#3362)

* Fix cross domain user search (#3420)

* backoffice/stern
  - Fixed `/i/user/meta-info` (#3436)
  - Fixed `/i/user/meta-info` (#3281)
  - Register/Update OAuth client via backoffice/stern (#3305)

* Fix: When defederating, don't crash on already-deleted conversations. (#3478)

* No `conversation.delete` event is sent to users during de-federation clean up (#3485)


## Documentation


* Improve the cassandra developer guidelines under https://docs.wire.com/developer/developer/cassandra-interaction.html (#3342)

* Document crypto library dependencies and sources of randomness (#3254)

* Add 'grepinclude' sphinx directive to document with some code snippets. (#3256)

* swagger:
  - Render `Named` names as "internal route ID" in swagger UI. (#3319)
  - Make /api/swagger{-ui,.json} TOC html pages to all versions (#3259)
  - Explain links to swagger docs better on docs.wire.com (#3388)
  - Swagger docs for custom backends (#3415)

* SSO Faq entry on CSP (#3398, #3491)



## Internal changes


* Export `Data.String.Conversions.cs` from `Imports` (#3320)

* Metrics for federator are available at `GET /i/metrics` for both the internal and external servers. (#3467)

* Add the status endpoint to both federator ports (#3443)

* Better errors in golden tests (#3370)

* In CI integration tests, use redis-ephemeral in master mode (may be reverted in the future, see PR details) (#3446)

* Containers now run as non-root, to improve compatibility with default PodSecurityPolicies in more recent versions of Kubernetes. (#3352)

* By default, the coturn helm chart will no longer log verbosely. This can be enabled if desired. (#3238)

* Delete libraries api-bot and api-client. Also delete tools from api-simulation. (#3395)

* Use feature singletons in TeamFeatureStore (#3308)

* Adding a new internal API to Brig and Galley to defederate domains.  Background-Worker has been reworked to seperate AMQP channel handling from processing. This was done to allow a defederation worker to share the same connection management process with notification pusher. (#3378)

* Improved websocket tests:
  - better error reporting
  - choose the correct backend when establishing a websocket connection (#3393)

* /integration (#3293)
  - Add convenience getJSON and getBody functions (#3293)
  - baseRequest now adds Z headers automatically (#3293)
  - Add liftIO versions of putStrLn etc (#3293)
  - Add Show instances for MLSState (#3293)
  - Implement test listing (#3301)
  - Port MLS test framework (#3288)
  - Support spawning multiple dynamic backends (#3316)
  - Split App module in integration package (#3273)
  - Test swagger docs (#3367)
  - Add parametrised tests (#3296)

* On CI runs, provide additional context when 'helmfile install' fails. (#3400)

* [hscim] make `jsonLower` fail on duplicate fields (#3346)

* Clean up output and logs (#3371)
  - integration: Remove debug messages from ModService tests
  - Do not log rabbit MQ connection failures on async exceptions
  - cannon: Do not print uncaught SignalledToExit exceptions to stdout

* End-to-end test for creating a DPoP access token for the E2EID client certificate enrollment (#3255)

* backoffice/stern
  - more integration tests and fixes (#3232, #3239)
  - `stern` is added to the new run-services implementation for the integration tests (#3425)

* Fixed eventually function in test for potentially less flakiness (#3240)

* Script to bulk-change/-repair user's scim and brig email address (#3321, #3331)

* Servantify brig internal api (#3346, #3338, #3339)

* Updated rusty-jwt-tools and error mapping (#3348)

* Reuse HTTP2 connections from brig, galley, cargohold and federator (#3120, #3233)

* Add combinator for maps with arbitrary keys in `schema-profunctor` (#3372)

* Introduce SearchContacts permission (#3252)

* All wire-server containers now run in a restricted securityContext when run on k8s >= 1.24 (#3351)

* Adding graceful shutdown handling to background-worker to allow it to finish processing its current message before the service quits. (#3421)


# [2023-04-17] (Chart Release 4.35.0)

## Release notes


* Wire cloud operators only: Before deploying apply the changes from https://github.com/zinfra/cailleach/pull/1586 to production as well. (#3146)

* New 'ingress-nginx-controller' wrapper chart compatible with kubernetes versions [1.23 - 1.26]. The old one 'nginx-ingress-controller' (compatible only up to k8s 1.19) is now DEPRECATED.
  We advise to upgrade your version of kubernetes in use to 1.23 or higher (we tested on kubernetes version 1.26), and to make use of the new ingress controller chart. Main features:
  - up-to-date nginx version ('1.21.6')
  - TLS 1.3 support (including allowing specifying which cipher suites to use)
  - security fixes
  - no more accidental logging of Wire access tokens under specific circumstances

  The 'kind: Ingress' resources installed via 'nginx-ingress-services' chart remain compatible with both the old and the new ingress controller, and k8s versions [1.18 - 1.26]. In case you upgrade an existing kubernetes cluster (not recommended), you may need to first uninstall the old controller before installing the new controller chart.

  In case you have custom overrides, you need to modify the directory name and top-level configuration key:

  ```diff
  # If you have overrides for the controller chart (such as cipher suites), ensure to rename file and top-level key:
  -# nginx-ingress-controller/values.yaml
  +# ingress-nginx-controller/values.yaml
  -nginx-ingress:
  +ingress-nginx:
     controller:
       # ...
  ```

  and double-check if all overrides you use are indeed provided under the same name by the upstream chart. See also the default overrides in [the default values.yaml](https://github.com/wireapp/wire-server/blob/develop/charts/ingress-nginx-controller/values.yaml).

  In case you use helmfile change your ingress controller like this:

  ```diff
  # helmfile.yaml
  releases:
  -  - name: 'nginx-ingress-controller'
  +  - name: 'ingress-nginx-controller'
       namespace: 'wire'
  -    chart: 'wire/nginx-ingress-controller'
  +    chart: 'wire/ingress-nginx-controller'
       version: 'CHANGE_ME'
  ```

  For more information read the documentation under https://docs.wire.com/how-to/install/ingress.html (or go to https://docs.wire.com and search for "ingress-nginx-controller") (#3140)

* If you are using OAuth (`optSettings.setOAuthEnabled: true` in brig config): before the deployment of wire-server the private and public keys for OAuth have to be provided for `brig` and `nginz` (see `docs/src/developer/reference/oauth.md` for more information) (#2989)

* Upgrade webapp version to 2023-04-11-production.0-v0.31.13-0-bb91157 (#2302)


## API changes


* Adding a new version of /list-users that allows for partial success. (#3117)

* Added a `failed_to_send` field to response when sending mls messages. (#3081)

* List failed-to-add remote users in response to `POST /conversations` (#3150)

* Updating the V4 version of /users/list-prekeys to return partial successes, listing users that could not be listed. (#3108)

* Non-binding team endpoints are removed from API version V4 (#3213)


## Features


* Add TLS and basic authentication to the inbucket (fake webmailer) ingress. (#3161)

* OAuth support for authorization of a curated list of 3rd party applications (see <https://docs.wire.com/developer/reference/oauth.html> for details) (#2989)

* Enforce a minimum length of 8 characters when setting a new password (#3137)

* Optional password for guest links (#3149)

* Authorization Code Flow with PKCE support (#3165)

* `conversations/join` endpoint rate limited per IP address (#3202)


## Bug fixes and other updates


* coturn helm chart: use a memory-backed folder to store sqllite DB to improve performance (#3220)

* Coturn helm chart: Increase the default timeout of liveness/readiness probe and make it configurable (#3218)

* When using the (now deprecated) ingress controller on older versions of kubernetes, ensure query parameters are not logged in the ingress logs (#3139)

* Fix version parsing in swagger-ui end-points (#3152)

* Fix a rate-limit exemption whereby authenticated endpoints did not get the unlimited_requests_endpoint, if set, applied. This is a concern for the webapp and calls to /assets, which can happen in larger numbers on initial loading. A previous change in [this PR](https://github.com/wireapp/wire-server/pull/2786) had no effect. This PR also increases default rate limits, to compensate for [new ingress controller chart](https://github.com/wireapp/wire-server/pull/3140)'s default topologyAwareRouting. (#3138, #3201)


## Documentation


* Add a client API version bump checklist (#3135)

* Fix the Swagger documentation for the failed_to_send field in the response of the Proteus message sending endpoint (#3223)

* Extend docs to support render plantuml directly, rewrote the saml flow diagram in plantuml (#3226)

* Allow swagger on disabled versions. (#3196)

* Documentation of setting up SSO integration with Okta was outdated with images from Okta Classic UI, the new version was updated using Oktas latest design. (#3175)


## Internal changes


* When sending a push message, stop deleting the push token and start recreating
  ARN when ARN is reported as invalid on AWS, but push token still is present in
  Cassandra. This allows on-demand migrations from one AWS account used for push
  notifications to another one. (#3162)

* We don't explicitly set with-compiler inside the cabal.project file anymore, because the version of GHC is controlled by Nix, and our nixpkgs pin. (#3209)

* - integration tests on CI will use either the old or the new ingress controller; depending on which kubernetes version they run on.
  - upgrade `kubectl` to default from the nixpkgs channel (currently `1.26`) by removing the manual version pin on 1.19
  - upgrade `helmfile` to default from the nixpkgs channel by removing the manual version pin
  - upgrade `helm` to default from the nixpkgs channel by removing the manual version pin
  - add `kubelogin-oidc` so the kubectl in this environment can also talk to kubernetes clusters using OIDC (#3140)

* Make new record syntax a language default (#3192)

* nixpkgs has been bumped to a more recent checkout (8c619a1f3cedd16ea172146e30645e703d21bfc1 -> 402cc3633cc60dfc50378197305c984518b30773, 2023-02-12 -> 2023-03-28). (#3206)

* Introduce VersionNumber newtype (see `/libs/wire-api/src/Wire/API/Routes/Version.hs` for explanation) (#3075)

* Fix a memory leak in `gundeck` when Redis is offline (#3136)

* Rust library `rusty-jwt-tools` upgraded to latest version (#3142)

* Updated rusty-jwt-tools to version 0.3.4 (#3194)

* Integration tests for backoffice/stern (#3216)

* ormolu: don't redundantly add language extensions from dead package-defaults.yaml (#3193)

* Stop support for versions on internal APIs (#3200)

* helm charts: bump kubectl docker images from 1.19.7 to 1.24.12 (#3221)

* Add an option (`UPLOAD_LOGS`) to upload integration test logs to AWS S3. (#3169)


## Federation changes


* Do not cause denial of service when creating a conversation with users from an unreachable backend (#3150)

* Report federated Proteus message sending errors to clients (#3097)

* Fix bug with asset downloads and large federated responses (#3154)


# [2023-03-06] (Chart Release 4.34.0)

## Release notes


* In (the unlikely) case your server config file contains `setWhitelist:`, you need to change this before the upgrade!  It used to refer to a whitelisting service, which is now replaced with a local list of allowed domains and phone numbers.  See [docs](https://docs.wire.com/developer/reference/user/activation.html?highlight=whitelist#phone-email-whitelist) for details.  Migration path: add new config fields; upgrade, remove old config fields. (#3043)

* The coturn Helm chart has been promoted to *beta* level stability. (#3078)


## API changes


* API v3 is now supported. The new MLS endpoints introduced in API v3 have been removed, and are now only available under v4. (#3122)


## Features


* Add internal endpoints of `cargohold`, `galley`, `legalhold` and `spar` to the Swagger docs for internal endpoints. (#3007)

* The coturn container image included in the coturn Helm chart was updated to
  version `4.6.0-wireapp.4`.

  With this version of coturn, the Prometheus metrics endpoint has been
  updated, and the `turn_active_allocations` metric label has been *renamed* to
  `turn_total_allocations`. (#3078)

* Better error message for invalid ID in a credential when uploading MLS key packages (#3102)

* Add Swagger documentation for internal endpoints. It's reachable at the path `/v<n>/api-internal/swagger{-ui,.json}`. (#3003)

* Render one Swagger page per internal endpoint. This superseeds the previous Swagger docs page for all internal endpoints. (#3094)

* Feature flag for Outlook calendar integration (#3025)

* Team feature setting for MLS end-to-end identity was added and server setting `setEnableMls` is exposed via new authorized endpoint `GET /system/settings` (#3082)


## Bug fixes and other updates


* The container image used for handling online TLS certificate updates in the
  coturn Helm chart was updated to a version with metadata compatible with
  containerd. (#3078)

* Fix a bug in the helm chart's nginx-ingress-services / federator Ingress resource introduced in the last release. (#3034)

* Remove overly restricte api check (#3131)

* Typing indicators not working accross federated backends (#3118)


## Documentation


* Extend the docs on the federation error type (#3045)

* Update SAML/SCIM docs (#3038)


## Internal changes


* - use exponential backoff for retrying requests to Amazon
  - also retry in case of server-side rate limiting by Amazon (#3121)

* Also run the 'backoffice' pod in CI (to test it can successfully start) (#3130)

* Make brig-schema a little faster by merging the first 34 schema migrations on fresh installations. (#3099)

* Deflake integration test: metrics (#3053)

* Lower the log level of federator inotify (#3056)

* use Wai's settings for graceful shutdown (#3069)

* CI integration setup time should be reduced: tweak the way cassandra-ephemeral is started (#3052)

* charts: Mark all service/secret/configmap test resources to be re-created by defining them as helm hooks (#3037, #3049)

* New integration test script with support for running end2end tests locally (#3062)

* Bump nixpkgs to latest commit on nixpkgs-unstable branch (#3084)

* Add config to allow to run helm tests for different services in parallel; improve integration test output logs (#3040)

*  Run brig and galley integration tests concurrently (#2825)

* Add wrapper for bitnami/postgresql chart. (#3012)

* Branch on performAction tags for finer-grained CallsFed constraints (#3030)

* Fixed broken stern endpoint `POST i/user/meta-info` (#3035)

* Make stern fail on startup if supported backend api version needs bumping (#3035)

* Automatically track CallsFed constraints via a GHC plugin (#3083)

* Rust library `rusty-jwt-tools` upgraded to latest version (#3112)

* Fixed test of jwt-tools Rust FFI (#3125)

* Enabling warnings for redundant constraints and removing the redundant
  constraints. (#3009)

* Migrate `/teams/notifications` to use the Servant library. (#3020)

* Split polysemy `Members` constraints into multiple `Member` constraints (#3093)


## Federation changes


* Use `HsOpenSSL` instead of `tls` for federation communication. (#3051)


# [2023-01-26] (Chart Release 4.31.0)

## Release notes


* wire-server helm charts using Ingress resources are now compatible with kubernetes versions 1.22, 1.23 and 1.24 (but remain compatible with older versions of kubernetes).

  If you upgrade to this version of helm charts and/or you upgrade your version of kubernetes while wire-server is deployed, you may find that `helm update` or `helmfile apply/sync` gives an error like this:

  > Error: UPGRADE FAILED: current release manifest contains removed kubernetes api(s) for this kubernetes version and it is therefore unable to build the kubernetes objects for performing the diff. error from kubernetes: unable to recognize "": no matches for kind "Ingress" in version "extensions/v1beta1"

  In which case you can use the [helm mapkubeapis plugin](https://github.com/helm/helm-mapkubeapis) to upgrade an existing release with the following command:

  ```sh
  # install plugin version 0.1.0 (more recent may not work)
  helm plugin install --version v0.1.0 https://github.com/helm/helm-mapkubeapis
  # adjust helm release name and namespace as required
  helm mapkubeapis --namespace wire nginx-ingress-services
  ```

  Alternatively, if a few minutes of downtime are not a problem; you can `helm delete` a release and re-install it again, which will work without the above plugin. (#3002)

* Upgrade team-settings version to 4.14.0-v0.31.9-0-bf82b46 (#2180)

* Upgrade webapp version to 2023-01-24-production.0-v0.31.9-0-17b742f (#2302)


## API changes


* The unqualified `GET /conversations/:id` endpoint has been removed from API v3, and is restored to the previous behaviour of returning a Conversation using the v2 schema. Similarly, its qualified counterpart `GET /conversations/:domain/:id` now returns a v2 Conversation when accessed through API v2. (#2992)


## Bug fixes and other updates


* Fix pagination in team user search (make search key unique) (#2968)

* Update `inbucket` (fake smtp server) chart dependency: The prior version relied on an image that has been removed from docker hub. Thus, our own `inbucket` chart could not be deployed anymore. (#2998)


## Documentation


* Add sphinx-copybutton plugin to make copying snippets of code from docs.wire.com easier. (#2900)

* Hook federated API call documentation into docs.wire.com (manually). (#2988)

* Tool for dumping fed call graphs (dot/graphviz and csv); see README for details (#2973)


## Internal changes


* Add Helm chart to configure clusters managed by k8ssandra-operator for test environments. (#2981)

* Fix kind setup for running end-to-end federation tests locally. (#3008)

* Fix Makefile target kind-restart-all. (#3015)

* Add combinators for creating mocked federator responses in integration tests (#3014)

* Add two integration tests arounds last prekeys (#2694)

* Fix `make clean` (#2965, #2978)

* Make ID tags more readable by expanding abbreviations to full names. (#2991)

* Unused old swagger code removed from stern and team features (#3017)

* Refactor Writetime from Int64 to wrapper of UTCTime (#2994)

* Restructure docs.wire.com (#2986)

* Fixed flaky team user search integration test (#2996)


# [2023-01-12] (Chart Release 4.30.0)

## Release notes


* This realease migrates data from `galley.member_client` to `galley.mls_group_member_client`. When upgrading wire-server no manual steps are required. (#2859)

* Upgrade webapp version to 2022-12-19-production.0-v0.31.9-0-6b2f2bf (#2302)


## API changes


* - The endpoints `POST /conversations/list` and `GET /conversations` have been removed. Use `POST /conversations/list-ids` followed by `POST /conversations/list` instead.
  - The endpoint `PUT /conversations/:id/access` has been removed. Use its qualified counterpart instead.
  - The field `access_role_v2` in the `Conversation` type, in the request body of `POST /conversations`, and in the request body of `PUT /conversations/:domain/:id/access` has been removed. Its content is now contained in the `access_role` field instead. It replaces the legacy access role, previously contained in the `access_role` field.
  - Clients implementing the V3 API must be prepared to handle a change in the format of the conversation.access_update event. Namely, the field access_role_v2 has become optional. When missing, its value is to be found in the field access_role. (#2841)

* Added a domain parameter to the typing indicator status update API (#2892)

* Support MLS self-conversations via a new endpoint `GET /conversations/mls-self`. This removes the `PUT` counterpart introduced in #2730 (#2839)

* List the MLS self-conversation automatically without needing to call `GET /conversations/mls-self` first (#2856)

* Fail early in galley when the MLS removal key is not configured (#2899)

* Introduce a flag in brig to enable MLS explicitly. When this flag is set to false or absent, MLS functionality is completely disabled and all MLS endpoints fail immediately. (#2913)

* Conversation events may have a "subconv" field for events that originate in a MLS subconversation (#2933)

* `GET /system/settings/unauthorized` returns a curated set of system settings from brig. The endpoint is reachable without authentication/authorization. It's meant to be used by apps to adjust their behavior (e.g. to show a registration dialog if registrations are enabled on the backend.) Currently, only the `setRestrictUserCreation` flag is exported. Other options may be added in future (in consultation with the security department.) (#2903)


## Features


* The coturn Helm chart now has a `.tls.ciphers` option to allow setting
  the cipher list for TLS connections, when TLS is enabled. By default,
  this option is set to a cipher list which is compliant with [BSI
  TR-02102-2](https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/TechGuidelines/TG02102/BSI-TR-02102-2.pdf). (#2924)

* **Nginz helm chart**: The list of upstreams is split into `nginx_conf.upstreams` and
  `nginx_conf.extra_upstreams`. Extra upstreams are disabled by default. They can
  be enabled by adding their name (entry's key) to
  `nginx_conf.enabled_extra_upstreams`. `nginx_conf.ignored_upstreams` is only
  applied to upstreams from `nginx_conf.upstreams`. In the default configuration
  of `nginz` extra upstreams are `ibis`, `galeb`, `calling-test` and `proxy`. If one
  of those is deployed, its name has be be added to
  `nginx_conf.enabled_extra_upstreams` (otherwise, it won't be reachable). Unless
  `nginx_conf.upstreams` hasn't been changed manually (overriding its default),
  this should be the only needed migration step. (#2849)

* A team member's role can now be provisioned via SCIM (#2851, #2855)

* Team search endpoint now supports pagination (#2898, #2895)

* Introduce optional disabledAPIVersions configuration setting (#2951)

* Add more logs to SMTP mail sending. Ensure that logs are written before the application fails due to SMTP misconfiguration. (#2818)

* Added typing indicator status propagation to federated environments (#2892)

* Allow vhost style addressing for S3 as path style is not supported for newer buckets.

  More info: https://aws.amazon.com/blogs/aws/amazon-s3-path-deprecation-plan-the-rest-of-the-story/ (#2955)


## Bug fixes and other updates


* Fix typo for Servicemonitor enable var in default values for helm charts. (#2896)

* The parser for the AWS/SNS error message to explain that an endpoint is already in use was incorrect. This lead to an "invalid token" error when registering push tokens for multiple user accounts (user ids) instead of updating the SNS endpoint with an additional user id. (#2921)

* Avoid client deletion edge case condition which can lead to inconsistent data between brig and galley's clients tables. (#2830)

* Conversations inside events are now serialised using the format of API V2 (#2971)

* Do not throw 500 when listing conversations and MLS is not configured (#2893)

* Do not list MLS self-conversation in client API v1 and v2 if it exists (#2872)

* Limit 2FA code retries to 3 attempts (#2960)

* Fix bug in MLS user removal from conversation: the list of removed clients has to be compared with those in the conversation, not the list of *all* clients of that user (#2817)

* Due to `sftd` changing how configuration is handled for "multi-SFT" calling (starting with version 3.1.10), new options have been added to the `sftd` Helm chart for compatibility with these newer versions. (#2886)

* For sftd/coturn/restund, fixed a bug in external ip address lookup, in case Kubernetes Node Name doesn't equal hostname. (#2837)

* Requesting a new token with the client_id now works correctly when the old token is part of the request (#2860)


## Documentation


* Add extra section to the deeplink docs to explain the socks proxy support while login. (#2885)

* Describe the auth cookie throttling mechanism. And overhaul the description of auth cookies in general. (#2941)

* PR guidelines docs are updated with correct helm configuration syntax (#2889)


## Internal changes


* Log AWS / SNS invalid token responses. This is helpful for native push notification debugging purposes. (#2908)

* Add tests for invitation urls in team invitation responses. These depend on the settings of galley. (#2797)

* brig: Allow multiple threads to run simultaneously (#2972)

* Remove support for compiling local docker images with buildah. Nix is used to build docker images these days (#2822)

* Nix-created docker images: add some debugging tools in the containers, and add 'make build-image-<packagename>' for convenience (#2829)

* Added typeclasses to track uses of federated calls across the codebase. (#2940)

* Split galley API routes and handler definitions into several modules (#2820)

* Default intraListing to true. This means that the list of clients, so far saved in both brig's and galley's databases, will still be written to both, but only read from brig's database. This avoids cases where these two tables go out of sync. Brig becomes the source of truth for clients. In the future, if this holds, code and data for galley's clients table can be removed. (#2847)

* Introduce the `MakesFederatedCall` Servant combinator (#2950)

* Bump nixpkgs to latest unstable. Stop using forked nixpkgs. (#2828)

* Optimize memory usage while creating large conversations (#2970)

* Reduce Polysemy-induced high memory requirements (#2947)

* Brig calling API is now migrated to servant (#2815)

* Fixed flaky feature TTL integration test (#2823)

* Brig teams API is now migrated to servant (#2824)

* Add 'inconsistencies' tool to check for, and repair certain kinds of data inconsistencies across different cassandra tables. (#2840)

* Backoffice Swagger 2.x docs is exposed on `/` and the old Swagger has been removed. Backoffice helm chart only runs stern without an extra nginx. (#2846)

* Give proxy service a servant routing table for swagger (not for replacing wai-route; see comments in source code) (#2848)

* Stern API endpoint `GET ejpd-info` has now the correct HTTP method (#2850)

* External commits: add additional checks (#2852)

* Golden tests for conversation and feature config event schemas (#2861)

* Add startup probe to brig helm chart. (#2878)

* Track federated calls in types across the codebase. (#2940)

* Update nix pins to point at polysemy-1.8.0.0 (#2949)

* Add MakesFederatedCall combinators to Galley (#2957)

* Fix `make clean`; allow new data constructors in `ToSchema Version` instance (#2965)

* Refactor and simplify MLS message handling logic (#2844)

* Remove cassandra queries to the user_keys_hash table, as they are never read anymore since 'onboarding' / auto-connect was removed in https://github.com/wireapp/wire-server/pull/1005 (#2902)

* Replay external backend proposals after forwarding external commits.
  One column added to Galley's mls_proposal_refs. (#2842)

* Remove an unused effect for remote conversation listing (#2954)

* Introduce types for subconversations (#2925)

* Use treefmt to ensure consistent formatting of .nix files, use for shellcheck too (#2831)


## Federation changes


* Honour MLS flag in brig's federation API (#2946)

* Split the Proteus and MLS message sending requests into separate types. The MLS request now supports MLS subconversations. This is a federation API breaking change. (#2925)

* Injects federated calls into the `x-wire-makes-federated-calls-to` extension of the swagger Operations (#2950)


# [2022-12-09] (Chart Release 4.29.0)

## Bug fixes and other updates

* Prevention of storing unnecessary data in the database if adding a bot to a conversation fails. (#2870)

## Internal changes

* bump nginx-module-vts from v0.1.15 to v0.2.1 (#2827)
* Build nginz and nginz_disco docker images using nix (#2796)

# [2022-11-03] (Chart Release 4.26.0)

## Release notes


* If you have not upgraded to [release 2021-03-21 (Chart Release 2.103.0)](https://github.com/wireapp/wire-server/releases/tag/v2021-03-21) yet, please do that now!

  NB: we only support releases 6 months back, so this should not be an issue.  But in this particular case we are positive that things will break if you don't do an intermediate upgrade. (#2768)

* Build docker images using nix derivations instead of Dockerfiles (#2331, #2771, #2772, #2775, #2776)

* Upgrade team-settings version to 4.13.0-v0.31.5-0-4754212 (#2180)

* Upgrade webapp version to 2022-11-02-production.0-v0.31.9-0-337e400 (#2302)

* The experimental wire-server-metrics helm chart has been removed.

  These were mostly a wrapper around prometheus operator. It makes more sense to
  refer to the upstream docs of Prometheus Operator or Grafana Agent Operator for
  installation instead. (#2740)


## API changes


* Do not expose swagger-ui on prod systems (to minimize attack surface) (#2800)

* Change mime type of body of /v3/mls/commit-bundles endpoint (#2773)

* Stop rate-limiting asset-signed-url requests on /assets/.* (#2786)

* The `/access` endpoint now takes an optional `client_id` query parameter. The first time it is provided, a new user token will be generated containing the given client ID. Successive invocations of `/access` will ignore the `client_id` parameter. Some endpoints can now potentially require a client ID as part of the access token. When trying to invoke them with an access token that does not contain a client ID, an authentication error will occur. (#2764)


## Features


* Introduce support for external commits in MLS (#2765)

* The `GET /teams/{tid}/members` endpoint now supports pagination (#2802)


## Bug fixes and other updates


* Clients without any prekeys are not deleted completely (#2758)


## Documentation


* tentatively allow `GET /api/event-notification-schemas` for json schemas of server-initiated events (missing pieces tracked in https://wearezeta.atlassian.net/browse/FS-1008) (#2739)

* Fix copyright date on docs.wire.com (#2792)

* Improve and cross-link documentation on SNS / push notifications. (#2781)

* Add extension sphinx-reredirects and configuration to generate simple JavaScript based redirects to new locations of previously inconsistently named files/URLs. (#2811)


## Internal changes


* Convert brig's auth endpoints to servant (#2750)

* Remove deprecated table for storing scim external_ids.

  Data has been migrated away in [release 2021-03-21 (Chart Release 2.103.0)](https://github.com/wireapp/wire-server/releases/tag/v2021-03-21) (see `/services/spar/migrate-data/src/Spar/DataMigration/V1_ExternalIds.hs`); last time it has been touched in production is before upgrade to [release 2021-03-23 (Chart Release 2.104.0)](https://github.com/wireapp/wire-server/releases/tag/v2021-03-23). (#2768)

* Refactor some internal Scim user tests (#2762)

* Reduce the payload size of internal `client.delete` event (#2807, #2816)

* Bump servant-swagger-ui package. (#2747)

* Increase charts/galley memory limit to 500M. (#2798)

* Add RPC, ServiceRPC and GalleyProvider effects to brig (#2653)

* Use locally build schema binaries for db migrations and execute them right before running integration tests. (#2791)

* Rename the make targets from `db-migrate-package` and `db-reset-package` to `db-migrate` and `db-reset` and allow migrating and resetting all keyspaces. (#2791)

* Add a Make target for ghci (#2749)

* Upgrade nginz/nginx to 1.22.1 (#2777)

* The dev environment provided by nix now contains all the haskell packages
  compiled by nix. This could casue linker errors while compiling haskell code in
  this repo. One way to get resolve them is to delete the 'dist-newstyle'
  directory. (#2331)

* Implemented a new intersperse combinator for Polysemy (#2767)

* Add a Concurrency effect for Polysemy (#2748)

* Don't fail client deletion when mls remove key is undefined (#2738)

* Migrate stern to swagger2-ui (remaining backwards compatible with circulating backoffice images) (see also #2742 from last release) (#2744)

* Gundeck push token API and notification API is migrated to Servant (#2769)

* Delete `deploy/services-demo` directory (#2789)

* Upgrade Servant to 0.19 (#2809)


# [2022-10-04] (Chart Release 4.25.0)

## Release notes


* Upgrade webapp version to 2022-10-04-production.0-v0.31.2-0-a438b30 (#2302)


## API changes


* Remove /legalhold/conversation alias from v2 (#2734)

* Make v2 a supported version and start v3 (#2734)


## Features


* Allow deletion of MLS team conversations (#2733)


## Bug fixes and other updates


* Revert synchronous semantics of client deletion endpoint (#2737)


## Documentation


* JCT-146 - update outdated info
  SER-211 - update new info regarding nodetool use (#2736)


## Internal changes


* Skeleton implementation of new endpoint for JWT DPoP access token generation (#2652, #2686)

* Add swagger2-ui to stern (#2742 ...)


# [2022-09-27] (Chart Release 4.24.0)

## Release notes


* For users of the (currently alpha) coturn Helm chart, **manual action is
  required** when upgrading to this version. The labels applied to the Kubernetes
  manifests in this chart have changed, in order to match the conventions used
  in the wire-server charts. However, this may mean that upgrading with Helm can
  fail, due to changes to the `StatefulSet` included in this chart -- in this
  case, the `StatefulSet` must be deleted before the chart is upgraded. (#2677)

* wire-server helm charts: Adjust default CPU/Memory resources: Remove CPU limits to avoid CPU throttling; adjust request CPU and memory based on observed values. Overall this decreases the amount of CPU/memory that the wire-server chart needs to install/schedule pods. (#2675)

* Upgrade team-settings version to 4.12.1-v0.31.5-0-0167ea4 (#2180)

* Upgrade webapp version to 2022-09-20-production.0-v0.31.2-0-7f74074 (#2302)


## API changes


* Add new endpoint `/mls/commit-bundles` for submitting MLS `CommitBundle`s. A `CommitBundle` is a triple consisting of a commit message, an optional welcome message and a public group state. (#2688)

* MLS: Store and expose group info via `GET /conversations/:domain/:id/groupinfo` (#2721)

* Add /mls/public-keys to nginz chart (#2676)

* Users being kicked out results in member-leave events originating from the user who caused the change in the conversation (#2724)

* Leaving an MLS conversation is now possible using the regular endpoint `DELETE /conversations/{cnv_domain}/{cnv}/members/{usr_domain}/{usr}`. When a user leaves, the backend sends external remove proposals for all their clients in the corresponding MLS group. (#2667)

* Validate remotely claimed key packages (#2692)


## Features


* The coturn chart now has support for exposing its metric endpoint with a
  ServiceMonitor, which can be ingested by third-party metrics collection tools. (#2677)

* Deleting clients creates MLS remove proposals (#2674)

* External remove proposals are now sent to a group when a user is deleted (#2650)

* Allow non-admins to commit add proposals in MLS conversations (#2691)

* Optionally add invitation urls to the body of `/teams/{tid}/invitations`. This allows further processing; e.g. to send those links with custom emails or distribute them as QR codes. See [docs](https://docs.wire.com/developer/reference/config-options.html#expose-invitation-urls-to-team-admin) for details and privacy implications. (#2684)


## Bug fixes and other updates


* SCIM user deletion suffered from a couple of race conditions. The user in now first deleted in spar, because this process depends on data from brig. Then, the user is deleted in brig. If any error occurs, the SCIM deletion request can be made again. This change depends on brig being completely deployed before using the SCIM deletion endpoint in brig. In the unlikely event of using SCIM deletion during the deployment, these requests can be retried (in case of error). (#2637)

* The 2nd factor password challenge team feature is disabled for SSO users (#2693)

* Less surprising handling of SIGINT, SIGTERM for proxy, stern. Increase grace period for shutdown from 5s to 30s for all services.  (#2715)


## Documentation


* Drop Client model (unused) from old swagger.
  Add a description and example data for mls_public_keys field in new swagger. (#2657)

* Document user deactivation (aka suspension) with SCIM. (#2720)

* Monitoring page showed wrong wrong configuration charts. Updated prometheus-operator to kube-prometheus-stack chart in the documentation.  (#2708)


## Internal changes


* Make client deletion asynchronous (#2669)

* Allow external add proposals without previously uploading key packages. (#2661)

* Allow legalhold tokens access to `/converations/<uuid>` endpoint (#2682, #2726)

* Move Brig.Sem.* modules to Brig.Effects (consistency) (#2672)

* The labels applied to resources in the coturn chart have been changed to
  reflect the conventions in the wire-server charts. (#2677)

* Drop the `managed` column from `team_conv` table in Galley (#2127)

* Fix link in PR template (#2673)

* In Gundeck's 'notifications' cassandra table, switch to [TWCS](https://cassandra.apache.org/doc/latest/cassandra/operating/compaction/twcs.html) compaction strategy, which should be more efficient for this workload, and possibly bring performance benefits to latencies.
  It may be beneficial to run a manual compaction before rolling out this
  change (but things should also work without this manual operation).
  In case you have time, run the following from a cassandra machine before deploying this update: `nodetool compact gundeck notifications`. (#2615)

* Add regular expression support to libzauth ACL language (#2714)

* Make test API calls point to the most recent version by default (#2695)

* Clients and key package refs in an MLS conversation are now stored in their own table. (#2667)

* Refactor MLS test framework (#2678)

* Update mls-test-cli to version 0.5 (#2685)

* Added rusty-jwt-tools to docker images (#2686)

* The account API is now migrated to servant. (#2699, #2700, #2701, #2702, #2703, #2704, #2705, #2707)

* Update nginz and cannon ACLs to match api-versioned paths (#2725)

* For wire-server cloud, on kubernetes 1.21+, favour topology-aware routing, which reduces unnecessary inter-availability-zone traffic, reducing latency and cloud provider cross-AZ traffic costs. (#2723)


# [2022-09-01] (Chart Release 4.23.0)

## Release notes


* The internal endpoint `GET i/mls/clients` has been changed, and it now returns a list of `ClientInfo` instead of a list of `ClientId`. (#2631)


## API changes


* Fix key package error description (#2651)

* Expose MLS public keys in a new endpoint `GET /mls/public-keys`. (#2602)


## Features


* The coturn chart now supports exposing the control port over TLS. (#2620)

* Forward all MLS default proposal types (#2628)

* New endpoints `HEAD` and `GET /nonce/clients` to request new nonces for client certificate requests (coming up soon). (#2641, #2655)

## Bug fixes and other updates


* Fix cql-io bug where restarting whole cassandra cluster could cause downtime. Upstream changes in https://gitlab.com/twittner/cql-io/-/merge_requests/20 (#2640)

* Improve client check when adding clients to MLS conversations (#2631)


## Documentation


* Move developer docs onto docs.wire.com (instead of exposing them on github only) (#2622, #2649)

* Add build instructions for developers (#2621)

* Make target audience explicit on docs.wire.com (#2662)


## Internal changes


* Support for external Add proposals (#2567)

* Add additional checks on incoming MLS messages:
  * if the sender matches the authenticated user
  * if the sender of message to a remote conversation is a member
  * if the group ID of a remote conversation matches the local mapping (#2618)

* Apply changes introduced by cabal-fmt. (#2624)

* Remove some redudant constraints in brig (#2638)

* Brig Polysemy: Port UserPendingActivationStore to polysemy (#2636)


* Add make target `delete-cache-on-linker-errors` to delete all Haskell compilation related caches. This is useful in cases where the development environment gets into an inconsistent state. (#2623)


* Move Paging effect from galley into polysemy-wire-zoo (#2648)

* Fix broken hls-hlint-plugin in nix env (#2629)

* Adjust developer PR template and document config and API procedures in-tree. (#2617)

* Add mls-test-cli to builder image (#2626)

* Add mls-test-cli to deps image (#2630)

* mls-test-cli: Use Cargo.lock file when building (#2634)

* Move common Arbitrary instances to types-common package for compilation speed (#2658)

* `LoginId` migrated to schema-profunctor (#2633, #2645)

* Improve cleaning rules in Makefile. (#2639)

* Fix typos, dangling reference in source code haddocs, etc. (#2586)

* Update the Elastic Search version used for running integration tests to the one that is delivered by wire-server-deploy. (#2656)


## Federation changes


* Add mlsPrivateKeyPaths setting to galley (#2602)


# [2022-08-16] (Chart Release 4.22.0)

## API changes


* Drop the deprecated member removal endpoint (#2593)


## Features


* charts/cannon: Ensure HSTS headers are set for all endpoints (#2574)

* Expired MLS key packages are deleted from the database (#2582)

* Add support for MLS Remove proposals (#2561)

* Human readable names for SAML IdPs (#2565)

* The `preferredLanguage` field from SCIM now maps to the user locale in BRIG and will be set and updated on post SCIM user and on update SCIM user using SAML. (#2605)

* For TLS1.2, by default, remove ECDHE-ECDSA-AES128-GCM-SHA256 and ECDHE-RSA-AES128-GCM-SHA256 ciphers for ingress traffic. (#2528)


## Bug fixes and other updates


* Allow deleting existing splash screens in `PUT /teams/:tid (see also PR#2474 in Release 4.18.0) (#2588)

* Backoffice: Fix an issue where in some deployments ibis/galeb (Wire Cloud internal services) are unreachable from backoffice if deployed in a different namespace. (#2610)

* Fix an issue for larger client requests on e.g. /list-users and /list-conversations, which were giving 413 errors for some users. Allow client requests of 256k by default (was 64k). (#2579)


## Internal changes


* Add shellcheck, libstdc++ to nix env; handle emacs auto-save files better (#2609)

* Allow features to be set with HTTP method PATCH. This reflects a prior behavior
  that is used by Ibis. Additionally, it's more consistent when all setters can be
  called with PUT and PATCH. As this will fix calls by Ibis, the deployment order
  doesn't matter. (#2575)

* Brig Polysemization: introduce BlacklistStore and BlacklistPhonePrefixStore effects (#2590)

* Add cabal-fmt development tool (#2601)

* Reformat all cabal files with cabal-fmt (#2603)

* Delete tools: bonanza and makedeb (#2600)

* No more package.yaml / hpack, and stick with cabal files as the single (and only) source of truth (#2596)

* Port Brig SearchAPI and UserRichInfo endpoints to Servant (#2580)

* Added TTL data to stern feature flag GET endpoint. (#2564)

* Prepare removing deprecated non-binding teams (no more used in integration tests) (#2514, #2607)

* Add internal endpoint in Brig to update clients' key package refs in DB upon committing.
  Brig should be deployed before Galley. (#2604)

* Improved the resilience of provisioning new users via SAML by combining two persistence calls into one, preventing a creation failure from locking a user handle with no corresponding user. (#2526)


## Federation changes


* Fix TBS field in MLS Message type (#2599)


# [2022-07-19] (Chart Release 4.21.0)

## Release notes


* Users of the (currently alpha) coturn Helm chart must **manually update
  their configuration** due to changes in how the chart handles authentication
  secrets. Please see below for further details. (#2553)


## API changes


* The response to POST /mls/messages adds a timestamp (#2560)


## Features


* charts/wire-server: default log format everywhere to StructuredJSON format (introduced in #1951 and #1959) (#2559)

* The coturn chart now supports multiple authentication secrets, which permits
  multiple backend instances to use the same TURN servers without needing to
  share authentication secrets between the backend instances.

  Correspondingly, the `.Values.secrets.zrestSecret` configuration option, which
  took a single authentication secret as its argument, has been replaced with the
  option `.Values.secrets.zrestSecrets` (note spelling!), which instead takes a
  *list* of authentication secrets as its argument. (#2553)

* Add support for bare MLS proposals (#2436)


## Bug fixes and other updates


* Fix a bug in charts/cannon. It's now possible to use a custom TLS certificate when enabling cannon's nginz sidecar container. (Previously only letsencrypt certificates worked, and were tested) (#2558)

* Minor fixes in helmcharts:
  - charts/nginz: Rate limit SSO endpoints less
  - charts/nginz: Ensure rate limiting isn't commented out
  - charts/galley: Honour .setttings.httpPoolSize
  - charts/galley: Fix typo in settings.featureFlags.validateSAMLEmails
  - charts/gundeck: Remove aws.connectionLimit
  - charts/brig: Fix default brandLabelUrl and remove brandLabel (#2563)


## Internal changes


* Port brig UserHandle API to servant (#2556)

* Bump timeout for integration tests to 15 minutes (from 10 minutes), as 10 minutes is no longer enough. (#2570)

* Internal endpoints to `PATCH` feature status (#2555)

*  Change the proposal hold time to 28 days (#2568)


# [2022-07-12] (Chart Release 4.19.0)

## Release notes


* Deploy spar before galley (#2543)

* Upgrade team-settings version to 4.11.0-v0.31.1-0-9e64150 (#2180)


## API changes


* Restore PUT /v2/connections endpoint (#2539)


## Features


* 2nd factor authentication code generation is rate limited now (#2522)

* The team member CSV export now fills `created_on` for SCIM users (#2543)


## Internal changes


* Add Helm chart for inbucket. Inbucket is a SMTP server that does not relay, but instead display received mail in a webapp and make them accessible via an API. (#2544)

* Bump saml2-web-sso (#2545, #2546)

* use checkedConnectCluster to avoid dropping requests to Redis when Gundeck reconnects to the Redis cluster (#2542)

* Do not log polysemy errors in Galley (#2531)

* Remove old crypto-cli tool from the ubuntu image (#2538)


## Federation changes


* Added new-remote-conversation RPC, used to notify a backend of a remote conversation the first time any user from that backend is added to it. (#2378)

* Added federation endpoint `send-mls-message` used to send messages to remote converesations (#2378)


# [2022-07-05] (Chart Release 4.18.0)

## Release notes


* For users of the (currently alpha) coturn Helm chart:
  **manual intervention may be required** when upgrading to
  this version of the chart from a prior version, due to [a bug in
  Kubernetes](https://github.com/kubernetes/kubernetes/issues/39188) which
  may interfere with applying changes to pod and service port configuration
  correctly.

  If, after updating this chart, the coturn pods do not have both a `coturn-udp`
  port and a `coturn-tcp` port, then the coturn `StatefulSet` must be manually
  deleted from the cluster, and then recreated by re-running Helm. Similarly, if
  the coturn `Service` does not have both a `coturn-udp` port and a `coturn-tcp`
  port, this `Service` must also be deleted and recreated. (#2500)

* The `nginz{-tcp,-http}` services have been unified into a `nginz` service, and
  moved into the nginz chart.

  The nginz-ingress-services chart simply targets the `nginz` service, so there's
  no need to set matching `service.nginz.external{Http,Tcp}Port` inside the
  `nginx-ingress-services` chart anymore.

  The `config.http.httpPort` and `config.ws.wsPort` values in the `nginz` chart
  still configure the ports the `nginz` service is listening on.

  Metrics were moved from `config.http.httpPort` to a new `http-metrics` port.

  The `nginz` chart also gained support for `metrics.serviceMonitor.enabled`,
  creating a `ServiceMonitor` resource to scrape metrics, like for other wire
  services.

  (#2476)

* Upgrade webapp version to 2022-06-30-production.0-v0.30.5-0-3e2aaf6 (#2302)

* In the helm charts, the `wireService` label has been removed.

  In some cases, we were already setting the `app` label too.

  Now we consistently use the `app` label to label different wire services.

  The `wireService` label was also used in the `spec.selector.matchLabels` field
  on existing `Deployment` / `StatefulSet` resources.
  As these fields being immutable, changing them isn't possible without recreation.

  If you encounter an issue like

  > field is immutable && cannot patch "*" with kind *

  you need to manually delete these StatefulSet and Deployment resources, and apply helm again, which will recreate them.

  This means downtime, so plan a maintenance window for it.

  The `wire-server-metrics` chart was previously running some custom
  configuration to automatically add all payloads with a `wireService` label into
  metrics scraping.

  With the removal of the `wireService` label, this custom configuration has been
  removed.

  Instead, all services that expose metrics will now create `ServiceMonitor`
  resources, if their helm chart is applied with `metrics.serviceMonitor.enabled`
  set to true.

  This prevents scraping agents from querying services that don't expose metrics
  at /i/metrics unnecessarily.

  Additionally, makes it easier to run other metric scraping operators, like
  `grafana-agent-operator`, without the need to also create some custom
  `wireService` label config there.

  Generally, if you have any monitoring solution installed in your cluster that
  uses the Prometheus CRDs, set `metrics.serviceMonitor.enable` for the following charts:

   - brig
   - cannon
   - cargohold
   - galley
   - gundeck
   - proxy
   - spar (#2413)


## API changes


* The request body of `POST /conversations` endpoint can now contain an optional `creator_client` field. The `creator_client` field is only relevant for MLS conversations, in which case it must be set to the ID of the client making the request. (#2486)

* Retire deprecated feature config API endpoints for API version V2 (#2492)


## Features


* Prevent race conditions in concurrent MLS commit requests. (#2525)

* charts/wire-server: Optionally include backoffice (#2490)

* The coturn chart has new functionality to enable graceful pod termination, by
  waiting for all active allocations on a coturn instance to drain first. When
  combined with a suitable external service discovery mechanism which can steer
  client traffic away from terminating coturn pods, this can be used to implement
  graceful rolling restarts of clusters of coturn instances. (#2456)

* `./deploy/services-demo/create_team_members.sh` creates users with given roles now (#2137)

* MLS implementation progress:
   - Remote users can be added to MLS conversations
   - MLS messages (both handshake and application) are now propagates to remote
     conversation participants. (#2415)

* charts/nginz: Serve swagger-ui for viewing swagger-1.2 docs (#2466)

* `GET teams/:tid` response now contains an optional field `splash_screen` which contains the asset key of the team's splash screen. `PUT teams/:tid` now supports updating the splash screen asset key. (#2474)

* Missing feature config mapping added (#2494)

* Add MLS team feature configuration (#2499)

* Team feature API now includes endpoints to get and set the `searchVisibilityInbound` feature (#2503)


## Bug fixes and other updates


* charts/backoffice: Fix version of frontend and auto-bump version of stern on every release (#2490)

* The service definitions in the coturn Helm chart were missing the control plane
  UDP port used by coturn. (#2500)

* In nginx-ingress-services chart, when enabling useCertManager, now correctly creates the required issuer by default. (#2532)

* Fix handling of creator client in MLS conversations (#2486)

* Fix all clients having the same MLS public key (#2501)

* A user now cannot delete an identity provider that they are authenticated with any more (#2519)


## Internal changes


* brig-types: remove all re-exports (#2505)

* Fixed flakiness of email update test, related to the test user account being suspended, causing subsequent runs of the test to fail. (#2497)

* galley-types: remove all re-exports (#2504)

* Enforce some IdP invariants (#2533)

* Switch to new MLS test CLI (https://github.com/wireapp/mls-test-cli) (#2508)

* Forward /i/users/:uid/features/:feature to brig (#2468)

* charts/nginz: Forward `/i/legalhold/whitelisted-teams` to galley instead of brig (#2460)

* make the ldap-scim-bridge chart deployable once per team, and improve docs. (#1843)

* Refactored and simplified the feature config API (#2435)

* Removed deprecated internal feature config API endpoints (#2496)

* Deactivated gundeck's integration tests for local steps (`make ci`). (#2510)

* retry gundeck's Redis connection in case of network errors such as IP changes or network outages (#2512)

* Add AWS security token metrics to all services (#2473)


# [2022-06-14] (Chart Release 4.14.0)

## Release notes


* Upgrade team-settings version to 4.10.0-v0.29.7-0-3be8ca3 (#2180)

* Upgrade webapp version to 2022-06-13-production.0-v0.29.7-0-2819b90 (#2302)


## Documentation


* Docs for guest links server and team feature settings added (#2480)

## Internal changes


* All feature configs like guest links e.g. can now be overridden in the helm configuration, so that they can be disabled/enabled and configured server wide (#2479)



# [2022-06-08] (Chart Release 4.13.0)

## Release notes


* The `.cannon.drainTimeout` setting on the wire-server helm chart has been
  removed and replaced with `.cannon.config.drainOpts`. (#2416)

* Note for wire.com operators: deploy nginz (#2439)


## API changes


* The back-office (aka stern) team feature API now accenpts an optional TTL parameter (in days), so features can be activated for a limited period. (#2417)

* Disable rate limiting for /api-version (#2439)


## Features


* Drain websockets in a controlled fashion when cannon receives a SIGTERM or
  SIGINT. Instead of waiting for connections to close on their own, the websockets
  are now severed at a controlled pace. This allows for quicker rollouts of new
  versions. (#2416)

* Optionally allow to run cannon with its own nginz inside the same pod; and connect to a load balancer directly.
  This allows the cannon-slow-drain behaviour implemented in #2416 to take effect by not having other intermediate network hops which could break websocket connections all at once.
  Some (internal) context: https://wearezeta.atlassian.net/wiki/spaces/PS/pages/585564424/How+to+gracefully+drain+cannon+but+not+so+slowly
  For details on how to configure this, see docs/src/how-to/install/configuration-options.rst (#2421)

* Support running brig with GeoIP database when using helm charts (#2406)

* charts/nginz: Add upstream configuration for galeb (#2444)

* charts/nginz: Allow upstreams to be in other namespaces (#2444)

* CSV export in team management now includes the number of devices per user (#2407)


## Bug fixes and other updates


* charts/nginz: Resolve collision between brig and galeb endpoints. Ensure
  /self/consent and /signatures endpoints are configured in all environments (#2457)

* When an IdP issuer (aka entity ID) is updated, the old issuer was still marked as "in use". (#2400)

* On actions that require re-authentication a password is not required if the user has SAML credentials (#2430, #2434, #2437)

* Use SCIM's preferred language as a fallback when privisioning users without a locale. (#2445)


## Documentation


* Feature configs should have different swagger schema names (#2425)


## Internal changes


* `AllFeatureConfigs` is now typed (#2403)

* Type class for default team feature status (#2404)

* charts/{redis-ephemeral,legalhold}: Use old index for bitnami repo as the new index doesn't have old versions of postgresql and redis helm charts (#2448)

* Bump haskell/zlib version to 0.6.3.0 (#2431)

* New internal brig endpoints for MLS KeyPackage -> Conversation association query/update (#2375)

* galley: refactor withSettingsOverrides (#2381)

* charts/{nginz,cannon}: Increase map_hash_bucket_size for nginx to 128 (#2443)

* charts/{cannon,nginz}: values listed in
  `nginx_conf.randomport_allowlisted_origins` must be full hostnames. Hostnames
  listed here will be allowlisted with and without TLS. (#2438)

* Remove binding of users to saml idps using saml (this has never been picked up by clients; use scim instead) (#2441)

* Remove golden test case generator

   (#2442)

* Convert Team CSV endpoint to Servant (#2419)


## Federation changes


* Send only the raw welcome message in the Galley "mls-welcome" federation endpoint (#2412)


# [2022-05-18] (Chart Release 4.12.0)

## Release notes


* If using [cert-manager](https://github.com/cert-manager/cert-manager), you need to have least version 1.0.0 (1.8.0 works at the time of writing) installed. Older cert-manager 0.15.X will no longer work. (#2401)

* Upgrade team-settings version to 4.9.0-v0.29.7-0-142a76f (#2180)


## API changes


* Start version 2 of the public API. Main changes:

   - Asset endpoints have lost their `v3` and `v4` suffixes. So for example
     `/assets/v3` has been replaced by `/assets`.
   - `GET /conversations/:conv/assets/:id` and `GET
     /conversations/:conv/otr/assets/:id` have been removed.
   - `GET /assets/:key/v3` has been removed. Use the qualified endpoint `GET
     /assets/:domain/:key` instead.
   - `DELETE /assets/:key/v3` has been removed. Use the qualified endpoint
     `DELETE /assets/:domain/:key` instead.
   - `GET /connections` has been removed. Use `POST /list-connections` instead.
   - `POST /connections` has been removed. Use `POST /connections/:domain/:user` instead.
   - `PUT /connections/:domain/:user` has been removed: use `POST` instead.
   - `GET /conversations` has been removed. Use `POST /conversations/list-ids`
     followed by `POST /conversations/list` instead.
   - `POST /conversations/list/v2` has been replaced by `POST
     /conversations/list`.
   - `POST /conversations/:domain/:conv/members/v2` has lost its `v2` suffix, so
     it is now `POST /conversations/:domain/:conv/members`.
   - `GET /users`, `GET /users/by-handle` and `GET /users/handles` have been
     removed. Use `POST /search/contacts` instead.
   - `GET /users/:id` has been removed. Use the qualified endpoint `GET
     /users/:domain/:id` instead.
   - `GET /users/:id/clients` has been removed. Use the qualified endpoint `GET
     /users/:domain/:id/clients` instead.
   - `GET /users/:id/clients/:client` has been removed. Use the qualified
     endpoint `GET /users/:domain/:id/clients/:client` instead.

  Swagger documentation for the previous version of the API can be accessed at
  `/v1/api/swagger-ui`. (#2297)

* A new field `development` has been added to the object returned by `GET
  /api-version`. Versions listed there are considered in flux, meaning that the
  corresponding API contracts can change arbitrarily over time. Clients are free
  to use development versions, as long as they are also listed in `supported`,
  and failures due to incompatibilities are acceptable (e.g. in testing
  environments). Backends are the authoritative source on whether a development
  version can be used at all. If a development version should not be used, the
  backend will not list it among the supported versions at all. (#2297)


## Features


* charts: Various new values can now be configured and some got changed

  Allow new configurations in the brig chart:
  * `config.emailSMS.user.invitationUrl`
  * `config.emailSMS.team.tInvitationUrl`
  * `config.emailSMS.team.tActivationUrl`
  * `config.emailSMS.team.tCreatorWelcomeUrl`
  * `config.emailSMS.team.tMemberWelcomeUrl`
  * `config.setProviderSearchFilter`
  * `config.setWhitelist`
  * `config.setFeatureFlags`
  * `config.setCustomerExtensions`

  If any values in config.emailSMS.team are specified, all must be specified.

  Allow new configurations in the gundeck chart:
  * `config.perNativePushConcurrency`
  * `config.maxConcurrentNativePushes.soft`
  * `config.maxConcurrentNativePushes.hard`

  Other changes:
  * Default `maxTeamSize` changed to 10000 from 500. (#2347)

* charts/nginx-ingress-services: Allow more fine-grained control over what services are installed. Upgrade Certificate/Issuer resources to 'cert-manager.io/v1' (#2401)

* MLS implementation progress:

   - remote key package claim is now supported (#2353)

* charts/{brig,cargohold,galley,gundeck}: Allow not configuring AWS credentials and allow using a special service account.
  This way, when operating wire in AWS cloud either instance profiles or IAM role attached to a service account can be used to communicate with AWS. (#2347)

* Implement TURN service discovery using SRV records (#2389)


## Bug fixes and other updates


* When `config.enablePayment` and `FEATURE_ENABLE_PAYMENT` (`envVars`) were set,
  the team-settings feature flag `FEATURE_ENABLE_PAYMENT` was rendered two times.
  The new behavior is to give the `envVars` entry priority. I.e. when it's set,
  it's used instead of the `config.enablePayment` value. (#2332)

* Modify the nginz access control configuration to prevent clients connecting
  to listeners with PROXY protocol enabled (such as the websocket listener) from
  accessing a private metrics endpoint. (#2307)

* Verification email is sent when external id is updated via SCIM (#2374)


## Documentation


* Move old /docs to /docs/legacy (leaving references). (#2328)

* Fixup for #2321 (#2323)

* Add pagination docs to `POST /list-connections` (#2369)

* Documentation for the 2nd factor password challenge feature (#2329)

* Documentation on how to enforce desktop application only for web app (#2334)

* Documentation on how to enforce constant bit rate for all calls (#2336)

* Documentation on how to disable media plugins for the web app (#2337)

* Documentation on how to extra entropy in the web app (#2338)

* Documentation on how to set the instance connection parameters and proxy settings (#2340)

* Merged SAML/SCIM docs with its main documentation (#2356)


## Internal changes


* View and change team feature permissions apply to all features now (#2402)

* Add sed to direnv (#2319)

* Add python3 to nix development environment. It's needed by hack/bin/serve-charts.sh . (#2333)

* Add a target to the Makefile to run ShellCheck. I.e. to run a linter on shell scripts. This will be used in the CI. For now, all scripts with linter issues are excluded from this check. (#2361)

* Drop snappy support from bonanza (#2350)

* Use cabal in buildah-based builds (#2341)

* Fix flakyness of path traversal test (#2387)

* Github Actions: disable mac builds (#2355)

* Apply `versionMiddleware` last. This makes sure that every other middleware sees
  the rewritten (unversioned) path. In particular, the prometheus middleware will
  now only see paths it knows about, which prevents it from reporting "N/A" as the
  path. (#2316)

* Upgrade version of libzauth dependencies, notably sodiumoxide bindings to libsodium, and fix resulting errors and warnings. (#2327)

* libzauth: Update sha256 for source in nix expression (#2354)

* Log IO exceptions in Galley and Brig (#2385)

* Generalise and move the Logger effect (#2306)

* Fix a comment in a Makefile target (#2330)

* Fix flaky MLS conversation creation test (#2386)

* Fix flaky key package test (#2384)

* Fix locale variables in Nix and .envrc (#2393)

* Team Member API has been migrated to Servant (#2309)

* Integration test for edge case: change external id before account registration (#2396)

* Allow specifying 'redisAdditionalWrite' for a secondary redis to which gundeck will write in the context of a redis migration without downtime. (#2304)

* Start TURN discovery only when the app starts and not when the Env is created (#2376)

* Avoid using IN queries for fetching multiple conversations (#2397)

* Remove oromolu GH action (has been moved to concourse https://github.com/zinfra/cailleach/pull/1033) (#2320)

* Remove unused data type AllowedUserSearch (#2373)

* docs: add latex to docs and publish pdf if exists (#2321)


## Federation changes


* We now fetch version information from other backends and negotiate a version to use. (#2297)

* Fix assertion in testWelcomeNoKey (#2372)

* Support remote welcome messages (#2368)

* Implement remote admin action: Update receipt mode (#2141)


# [2022-05-04] (Chart Release 4.11.0)

## Release notes


* Upgrade webapp version to 2022-05-04-production.0-v0.29.7-0-a6f2ded (#2302)


# [2022-04-25] (Chart Release 4.10.0)

## Release notes


* Note for wire.com operators: deploy nginz (#2270)

* Wire cloud operators: [Update brig's ES index mapping before deploying. After deploying, run a re-index](https://github.com/wireapp/wire-server/blob/master/docs/reference/elastic-search.md) (#2213, #2220)

* Upgrade webapp version to [2022-04-21-production.0](https://github.com/wireapp/wire-webapp/releases/tag/2022-04-21-production.0) (#2302)

* Upgrade team-settings version to [4.7.0-v0.29.7-0-74b81b8](https://github.com/wireapp/wire-team-settings/releases/tag/v4.7.0) (#2180)


## Features


* [helm-charts] Allow filtering cassandra nodes by datacenter (#2273)

* MLS implementation progress:
   - commit messages containing add proposals are now processed (#2247)
   - do initial validation and forwarding of all types of messages via POST /mls/messages (#2253)
   - fixed bug where users could not be added to MLS conversations if they had non-MLS clients (#2290)
   - MLS/Proteus mismatches (e.g. sending a proteus message to an MLS conversation) are now handled (#2278)
   - the `POST /mls/key-packages/claim` endpoint gained a `skip_own` query parameter, which can be used to avoid claiming a key package for the requesting client itself (#2287)

* The user profiles that are returned by a team admin search now contain the additional fields SAML NameID, IdP Issuer, and SCIM externalId (#2213), and  unvalidated email address (#2220)

* *  Avoid dropping messages when redis is down. (#2295)


## Bug fixes and other updates


* Add missing helm chart mapping for inbound search visibility (#2265)

* Fix bug: User search endpoint hides exact handle results in SearchVisibilityNoNameOutsideTeam setting (#2280)

* backoffice app (aka stern):
    - Suspending a non-existing user now returns 404 and does not create an empty entry in the DB (#2267)
    - Support for deleting teams with more than one member (#2275)
    - Fix update of user email (#2281)


## Documentation


* Import wire-docs to docs/ (see also #2258)


## Internal changes


* Migrate API routes from wai-route to servant for better Swagger (#2284, #2277, #2266, #2286, #2294, #2244

* Update nginx to latest stable: v1.20.2 (#2289)

* Allow additional origins at random ports in nginz Helm chart. This is useful for
  testing with an HTTP proxy. It should not be used in production. (#2283)

* makdeb and bonanza: remove stack-based Makefiles (#2311)

* Add `skip_reauth` param to internal API for creating clients. This is intended to be used in test. (#2260)

* Removes an unused function in Brig and relocates another one (#2305)

* Print more logs while migrating data in Elasticsearch (#2279)

* Replace the base monad in Brig with the Polysemy Sem monad (#2264, #2288)

* Move the Random effect from Spar to the polysemy-wire-zoo library (#2303)

* Move the Now effect from Spar to a library (#2292)

* Improve readability of user search test cases (#2276)

* Chart/gundeck's 'bulkpush' optimization is now activated by default (after using it in production for some time) (#2293)

* Add an alpha version of a Helm chart for coturn. (#2209)

* Document error handling and simplify error logging (#2274)

* Improve speed of reindexing by increasing the batch size of processing users. (#2200)

* Fix federator integration tests (#2298)

* Switch the Haskell driver used in Gundeck to connect to Redis from 'redis-io' to '[hedis](https://hackage.haskell.org/package/hedis)', which now supports cluster mode. (#2151)

* Various Galley MLS test improvements and cleanups (#2278)

* Flag for sending a validation email when updating a user's email address via backoffice/stern (#2301)

* Remove stack from all builder docker images (#2312)

* Make internal search-visibility endpoint available to staging environments (#2282)

* Remove TemplateHaskell as a global default extension (#2291)


# [2022-04-04] (Chart Release 4.9.0)

## Release notes


* Note for wire.com operators: deploy nginz (#2175)

* Deploy galley before brig (#2248)

* Wire cloud operators: [Update brig's ES index mapping before deploying. After deploying run a reindex](https://github.com/wireapp/wire-server/blob/master/docs/reference/elastic-search.md). (#2241)

* Upgrade webapp version to 2022-03-30-production.0-v0.29.2-0-d144552 (#2246)


## API changes


* New endpoint to get the status of the guest links feature for a conversation that potentially has been created by someone from another team. (#2231)


## Features


* Cross-team user search (#2208)

* restund chart: add dtls support (#2227)

* MLS implementation progress:

   - welcome messages are now being propagated (#2175)

* The bot API will be blocked if the 2nd factor authentication team feature is enabled. Please refer to [/docs/reference/config-options.md#2nd-factor-password-challenge](https://github.com/wireapp/wire-server/blob/develop/docs/reference/config-options.md#2nd-factor-password-challenge). (#2207)

* Translations for 2nd factor authentication email templates (#2235)

* Script for creating a team with owner via the public API (#2218)


## Bug fixes and other updates


* Conversation rename endpoints now return 204 instead of 404 when the conversation name is unchanged (#2239)

* Revert temporary sftd bump (#2230)


## Internal changes


* Remove the MonadMask instance for AppT in Brig (#2259)

* Remove the MonadUnliftIO instance for the app monad in Brig (#2233)

* Bump hsaml2 version (#2221)

* Fix: cabal-install-artefacts.sh fails if not run from root of wire-server (#2236)

* Fix: pushing to cachix not working (#2257)

* Cannon has been fully migrated to Servant (#2243)

* Refactor conversation record and conversation creation functions. This removes a lot of duplication and makes the types of protocol-specific data in a conversation tighter. (#2234)

   - Move conversation name size check to `NewConv`
   - Make the `NewConversation` record (used as input to the data
     function creating a conversation) contain a `ConversationMetadata`.
   - Implement all "special" conversation creation in terms of a general `createConversation`
   - Move protocol field from metadata to Conversation
   - Restructure MLS fields in Conversation record
   - Factor out metadata fields from Data.Conversation

* Fix Docs: real-world domain used in examples (#2238)

* The `CanThrow` combinator can now be used to set the corresponding error effects in polysemy handlers. (#2239)

* Most error effects in Galley are now defined at the granularity of single error values. For example, a handler throwing `ConvNotFound` will now directly declare `ConvNotFound` (as a promoted constructor) among its error effects, instead of the generic `ConversationError` that was used before. Correspondingly, all such fine-grained Galley errors have been moved to wire-api as constructors of a single enumerated type `GalleyError`, and similarly for Brig, Cannon and Cargohold. (#2239)

* Add a column for MLS clients to the Galley member table (#2245)

* Pin direnv version in nix-hls.sh script (#2232)

* nginx-ingress-services chart: allow for custom challenge solvers (#2222, #2229)

* Remove unused debian Makefile targets (#2237)

* Use local serial consistency for Cassandra lightweight transactions (#2251)


# [2022-03-30] (Chart Release 4.8.0)

## Release notes

* Upgrade webapp version to 2022-03-30-production.0-v0.29.2-0-d144552 (#2246)
# [2022-03-18] (Chart Release 4.7.0)

## Release notes

* Deploy Brig before Spar. (#2149)
* If you are in a federated network of backends (currently beta), you need to update all participating instances at the same time. (#2173)

## API changes

* The `client` JSON object now has an additional field `mls_public_keys`, containing an object mapping signature schemes to public keys, e.g.
  ```
  {
    ...
    "mls_public_keys": { "ed25519": "GY+t1EQu0Zsm0r/zrm6zz9UpjPcAPyT5i8L1iaY3ypM=" }
    ...
  }
  ```
  At the moment, `ed25519` is the only supported signature scheme, corresponding to MLS ciphersuite 1.

  When creating a new client with `POST /clients`, the field `mls_public_keys` can be set, and the corresponding public keys are bound to the device identity on the backend, and will be used to verify uploaded key packages with a matching signature scheme.

  When updating a client with `PUT /clients/:client`, the field `mls_public_keys` can also be set, with a similar effect. If a given signature scheme already has a public key set for that device, the request will fail. (#2147)

* Introduce an endpoint for creating an MLS conversation (#2150)

* The `/billing` and `/teams/.*/billing` endpoints are now available on a versioned path (e.g. `/v1/billing`) (#2167)


## Features


* MLS implementation progress:

   - key package refs are now mapped after being claimed (#2192)

* 2nd factor authentication via 6 digit code, sent by email:
   - for login, sent by email. The feature is disabled per default and can be enabled server or team wide. (#2142)
   - for "create SCIM token". The feature is disabled per default and can be enabled server or team wide. (#2149)
   - for "add new client" via 6 digit code, sent by email. This only happens inside the login flow (in particular, when logging in from a new device).  The code obtained for logging in is used a second time for adding the device. (#2186)
   - 2nd factor authentication for "delete team" via 6 digit code, sent by email. (#2193)
   - The `SndFactorPasswordChallenge` team feature is locked by default. (#2205)
   - Details: [/docs/reference/config-options.md#2nd-factor-password-challenge](https://github.com/wireapp/wire-server/blob/develop/docs/reference/config-options.md#2nd-factor-password-challenge)

## Bug fixes and other updates


* Fix data consistency issue in import of users from TM invitation to SCIM-managed (#2201)

* Use the same context string as openmls for key package ref calculation (#2216)

* Ensure that only conversation admins can create invite links.  (Until now we have relied on clients to enforce this.) (#2211)


## Internal changes


* account-pages Helm chart: Add a "digest" image option (#2194)

* Add more test mappings (#2185)

* Internal endpoint for re-authentication (`GET "/i/users/:uid/reauthenticate"`) in brig has changed in a backwards compatible way. Spar depends on this change for creating a SCIM token with 2nd password challenge. (#2149)

* Asset keys are now internally validated. (#2162)

* Spar debugging; better internal combinators (#2214)

* Remove the MonadClient instance of the Brig monad

  - Lots of functions were generalized to run in a monad constrained by
    MonadClient instead of running directly in Brig's `AppIO r` monad. (#2187)


## Federation changes


* Refactor conversation actions to an existential type consisting of a singleton tag (identifying the action) and a dedicated type for the action itself. Previously, actions were represented by a big sum type. The new approach enables us to describe the needed effects of an action much more precisely. The existential type is initialized by the Servant endpoints in a way to mimic the previous behavior. However, the messages between services changed. Thus, all federated backends need to run the same (new) version. The deployment order itself does not matter. (#2173)


# [2022-03-09] (Chart Release 4.6.0)

## Release notes


* Upgrade team-settings version to 4.6.2-v0.29.7-0-4f43ee4 (#2180)


# [2022-03-07] (Chart Release 4.5.0)

## Release notes


* For wire.com operators: make sure that nginz is deployed (#2166)


## API changes


* Add qualified broadcast endpoint (#2166)


## Bug fixes and other updates


* Always create spar credentials during SCIM provisioning when applicable (#2174)


## Internal changes


* Add tests for additional information returned by `GET /api-version` (#2159)

* Clean up `Base64ByteString` implementation (#2170)

* The `Event` record type does not contain a `type` field anymore (#2160)

* Add MLS message types and corresponding deserialisers (#2145)

* Servantify `POST /register` and `POST /i/users` endpoints (#2121)


# [2022-03-01] (Chart Release 4.4.0)

## Release notes


* Upgrade webapp version to 2022-02-22-production.0-v0.29.2-0-abb34f5 (#2148)


## API changes


* The `api-version` endpoint now returns additional information about the backend:

    - whether federation is supported (field `federation`);
    - the federation domain (field `domain`).

  Note that the federation domain is always set, even if federation is disabled. (#2146)

* Add MLS key package API (#2102)


## Internal changes


* Bump aeson to v2.0.3.0 and update amazonka fork from upstream repository.  (#2153, #2157, #2163)

* Add schema-profunctor instances for `QueuedNotification` and `QueuedNotificationList` (#2161)

* Dockerfile.builder: Add cabal update (#2168)


## Federation changes


* Make restrictions on federated user search configurable by domain: `NoSearch`, `ExactHandleSearch` and `FullSearch`.
  Details about the configuration are described in [config-options.md](docs/reference/config-options.md).
  There are sane defaults (*deny to find any users as long as there is no other configuration for the domain*), so no measures have to be taken by on-premise customers (unless the default is not the desired behavior). (#2087)


# [2022-02-21] (Chart Release 4.2.0)

## Release notes

* Upgrade team-settings version to 4.6.1-v0.29.3-0-28cbbd7 (#2106)
* Upgrade webapp version to 2022-02-08-production.0-v0.29.2-0-4d437bb (#2107)
* Change the default set of TLS ciphers (both for the client and the federation APIs) to be compliant to the recommendations of [TR-02102-2](https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/TechGuidelines/TG02102/BSI-TR-02102-2.html). (#2112)
* For wire.com operators: make sure that nginz is deployed. (#2116, #2124)
* Optional team feature config `validateSAMLEmails` added to galley.yaml.
  The feature was disabled by default before this release and is now enabled by default. The server wide default can be changed in galley.yaml. Please refer to [/docs/reference/config-options.md#validate-saml-emails](https://github.com/wireapp/wire-server/blob/develop/docs/reference/config-options.md#validate-saml-emails) (#2117)

## API changes

* Added minimal API version support: a list of supported API versions can be found at the endpoint `GET /api-version`. Versions can be selected by adding a prefix of the form `/vN` to every route, where `N` is the desired version number (so for example `/v1/conversations` to access version 1 of the `/conversations` endpoint). (#2116)
* Delete `GET /self/name` endpoint (#2101)
* New endpoint (`POST /verification-code/send`) for generating and sending a verification code for 2nd factor authentication actions. (#2124)

## Features

* Add freetext search results to "search-users" federation endpoint (#2085)

## Bug fixes and other updates

* Ensure empty responses show up without a schema in swagger. They were shown as empty arrays before. (#2104)
* Require the guest links feature is enabled when someone joins by code. (#2084)
* Escape disallowed characters at the beginning of CSV cells to prevent CSV injection vulnerability. (#2096)
* The field `icon` in the body of the `PUT /team/:tid` endpoint is now typed to prevent potential injection attacks. (#2103)

## Internal changes

* Enforce conversation access roles more tightly on the backend (was previously only enforce on client): if a guests or non-team-members are not allowed, block guest link creation (new behavior) as well as ephemeral users joining (old behavior). (#2076)
* Remove uses of servant-generics from brig (#2100, #2086)
* Migrate more API end-points to servant. (#2016, #2081, #2091)
* Introduce the row type variable in Brig monads (#2140)
* Build ubuntu20 docker images with cabal instead of stack (#2119, #2060)
* Drop managed conversations (#2125)
* To investigate issues related to push notifications, adjust Gundeck `Debug` leveled logs to not print the message itself. So, that it can safely be turned on in production environments. Add a log entry when a bulk notification is pushed to Cannon. (#2053)
* Add integration tests for scim/saml user creation (#2123)
* Wrap stack with NIX_BUILD_SHELL set to LD_LIBRARY_PATH compatible shell (#2105)
* Removed redundant `setDefaultTemplateLocale` config from the brig helm template. (#2099)
* [not done yet, please do not enable] Optional team feature config `sndFactorPasswordChallenge` added to galley.yaml.
  The feature is disabled by default. The server wide default can be changed in galley.yaml. Please refer to [/docs/reference/config-options.md#2nd-factor-password-challenge](https://github.com/wireapp/wire-server/blob/develop/docs/reference/config-options.md#2nd-factor-password-challenge) (#2138)
* Prometheus: Ignore RawResponses (e.g. cannon's await responses) from metrics (#2108)
* Refactor internal handlers for Proteus conversation creation (#2125)
* Specify (in a test) how a message to a deleted legalhold device is refused to be sent. (#2131)

## Federation changes

* Add `setSftListAllServers` config flag to brig (#2139)
* Revert restund to 0.4.17. (#2114)



# [2022-02-02] (Chart Release 4.0.0)

## Release notes


* Upgrade webapp version to 2022-01-27-production.0-v0.28.29-0-42c9a1e (#2078)


## Features


* Allow brig's additionalWriteIndex to be on a different ElasticSearch cluster.
  This allows migrating to a new ElasticSearch cluster. (#2063)

* The file sharing team feature now has a server wide configurable lock status. For more information please refer to [/docs/reference/config-options.md#file-sharing](https://github.com/wireapp/wire-server/blob/develop/docs/reference/config-options.md#file-sharing). (#2059)


## Internal changes


* Remove non-existing functions from module export lists (#2095)

* Rename Spar.Sem.IdP to Spar.Sem.IdPConfigStore (#2067)

* Endpoints based on `MultiVerb` can now be made to return content types not listed in the `Accept` header (#2074)

* The lock status of the file sharing team feature can be updated via the internal API (`PUT /i/teams/:tid/features/fileSharing/(un)?locked`). (#2059)

* Servantify Galley Teams API (`GET /teams/:tid` and `DELETE /teams/:tid`). (#2092)

* Add explicit export lists to all Spar.Sem modules (#2070)

* Separate some Spar.Sem utility functions into their own module (#2069)


# [2022-01-28] (Chart Release 2.125.0)

## Release notes

* Bump the webapp version. (#2082)

## Internal changes

* Additional integration testing for conversation access control. (#2057)


# [2022-01-27] (Chart Release 2.124.0)

## Release notes

* The `nginz` chart now configures nginx to only allow cross-origin requests from an explicit allow list of subdomains. By default these are:

  ```yaml
  nginz:
    nginx_conf:
      allowlisted_origins:
      - webapp
      - teams
      - account
  ```

  If you changed the names of these services, you must adjust those names in the nginz config as well. (#1630, #2073, 116988c62732)

* Backend now separates conversation access control for guests and services. The old access roles are still supported but it is encouraged to upgrade clients since mapping between the old access roles and the new access roles is not isomorphic. For more details refer to the API changes below or the Swagger docs.
  Old clients are fully supported; if new clients and old clients are mixed, to old clients, either guests of services may appear to be enable if they are not, which may lead to error messages (confusing but harmless). (#2035)

## API changes

* Endpoints that recently have accepted `access_role` in their payload will now accept `access_role_v2` as well which will take precedence over `access_role`. See Swagger docs for how values are mapped. Endpoints that recently have returned `access_role` in their payload will now additionally return the `access_role_v2` field. (#2035)

## Features

* Conversation access roles now distinguish between guests and services. (#2035)

## Bug fixes and other updates

* There is now an explicit CORS allow list for *all* endpoints. In previous releases, all subdomains were accepted, however they must now be listed explicitly. This is a **breaking change**, as now only known Javascript applications may access the backend. (#1630, #2073, 116988c62732)
* Prevent 500s when SFTs are not reachable from Backend (#2077)

## Internal changes

* Bump hsaml2 package version (#2075)
* Separate Spar.Data module into smaller Cassandra interpreters (#2064)
* Fix some HLint issues in libs/wire-api. (#2065)
* Fix broken build process of package "old-time" for some environments (#2056)
* Refresh license headers (#2062)
* Rename Spar.Sem.ScimTokenStore.GetByTeam to LookupByTeam (#2068)

## Federation changes

* Tag several federation tests cases for the M2 release (#2045)


# [2022-01-18] (Chart Release 2.122.0)

## Release notes

* This release introduces a mandatory `federationDomain` configuration setting to cargohold. Please update your `values/wire-server/values.yaml` to set `cargohold.settings.federationDomain` to the same value as the corresponding option in galley (and brig). (#1990)
* The brig server config option `setDefaultLocale` has been replaced by `setDefaultUserLocale` and `setDefaultTemplateLocale` (see docs/reference/config-options.md for details) (#2028)
* From this release onwards, the images for haskell components (brig, galley,
  cargohold, etc.) will be using Ubuntu 20.04 as the base. The images are about
  30-35 MB larger than the previous alpine based images. (#1852)
* Wire cloud operators: Make sure [#35](https://github.com/wireapp/ansible-sft/pull/35) is applied to all SFT servers before deploying. (#2030)

## API changes

* The deprecated endpoint `GET /teams` now ignores query parameters `ids`, `start` (#2027)
* Add qualified v4 endpoints for downloading and deleting assets. The upload API is still on the same path, but the asset object it returns now contains a `domain` field. (#2002)
* Remove resumable upload API (#1998)

## Features

* Allow configuring setDefaultLocale in brig using helm chart (#2025)
* If the guest links team feature is disabled guest links will be revoked. (#1976)
* Revoke guest links if feature is disabled. If the guest links team feature is disabled `get /conversations/join`, `post /conversations/:cnv/code`, and `get /conversations/:cnv/code` will return an error. (#1980)
* Specialize `setDefaultLocale` to distinguish between default user locale and default template locale if the user's locale is n/a. (#2028)

## Bug fixes and other updates

* Fix an issue with remote asset streaming (#2037, #2038)

## Documentation

* Annotate a first batch of integration and unit tests to map them to externally-facing documentation (#1869)
* Add the description to several test cases (#1991)
* Improve documentation for stern tool and helm chart (#2032)

## Internal changes

* Replace servant-generic in Galley with a custom `Named` combinator (#2022)
* The Swagger documentation module is not regenerated anymore if its content is unchanged (#2018)
* cabal-run-integration.sh - remove Makefile indirection (#2044)
* Fix test runner for global cabal make target (#1987)
* The `cabal-install-artefacts.sh` script now creates the `dist` directory if it does not exist (#2007)
* Set `purge: false` in fake-s3 chart (#1981)
* Add missing backendTwo.carghold in integration.yaml (#2039)
* Use GHC 8.10.7 and stack 2.7.3 for builds (#1852)
* Fix non-controversial HLint issues in federator to improve code quality (#2011)
* Added laws for DefaultSsoCode, Now, IdP and ScimExternalIdStore (#1940)
* Moved specifications for Spar effects out of the test suite and into the library (#2005)
* Tag integration tests for security audit. (#2000)
* Upgrade nixpkgs pin used to provision developement dependencies (#1852)
* Servantify Galley Teams API. (#2008, #2010, #2027)
* When sending an activation code, the blocked domains are checked before the whitelist. This only affects the wire SaaS staging environment (there is no whitelist configuration in prod, and blocked domains are not applicable to on-prem installations). (#2023)
* Add a helm chart that deploys [restund](https://docs.wire.com/understand/restund.html) (#2003)
* Publish restund helm chart (#2036)
* Improve optional field API in schema-profunctor (#1988)
* Migrate the public API of Cannon to Servant. (There is an internal API that is not yet migrated.) (#2024)
* sftd chart: Add multiSFT option, remove additionalArgs option (#1992)
* sftd chart: Fix quoted args for multiSFT option (#1999)
* `rangedSchema` does not need to be passed singletons explicitly anymore (#2017)
* Split cannon benchmarks and tests (#1986)
* Tag integration tests for certification. (#1985)
* Tag integration tests for certification. (#2001)
* New internal endpoint to configure the guest links team feature. (#1993)

## Federation changes

* Make federator capable of streaming responses (#1966)
* Use `Named` routes for the federation API (#2033)
* Fix Brig's configmap for SFT lookups (#2015)
* SFTD chart: provide a /sft_servers_all.json url that can be used by brig to populate /calls/config/v2 (#2019)
* Allow making HTTP-only requests to SFTs via an IPv4 address (#2026)
* Replace IPv4-HTTP-only Approach to SFT Server Lookup with /sft_servers_all.json (#2030)
* Extend GET /calls/config/v2 to include all SFT servers in federation (#2012)
* Improve Brig's configuration for SFTs and fix a call to SFT servers (#2014)
* Enable downloading assets from a remote (federated) cargohold instance via the v4 API. The content of remote assets is returned as stream with content type `application/octet-stream`. Please refer to the Swagger API documentation for more details. (#2004)

# [2021-12-10] (Chart Release 2.121.0)

## Release notes

* If you have `selfDeletingMessages` configured in `galley.yaml`, add `lockStatus: unlocked`. (#1963)
* Upgrade SFTD to 2.1.19. (#1983)

## API changes

* A new endpoint is added to Brig (`put /users/:uid/email`) that allows a team owner to initiate changing/setting a user email by (re-)sending an activation email. (#1948)
* get team feature config for self deleting messages response includes lock status (#1963)
* A new public Galley endpoint was added to dis-/enable the conversation guest link feature. The feature can only be configured through the public API if the lock status is unlocked in the server config. (#1964)
* new internal endpoints for setting the lock status of self deleting messages (#1963)

## Features

* Team and server wide config for conversation guest link feature to configure feature status and lock status (#1964). If the feature is not configured on the server, the defaults will be:

  ```txt
    featureFlags:
      ...
      conversationGuestLinks:
        defaults:
          status: enabled
          lockStatus: unlocked
  ```
* Lock status for the self deleting messages feature can be set internally by ibis and customer support (#1963)

## Bug fixes and other updates

* Correctly detect log level when rendering logs as structured JSON (#1959)

## Documentation

* Fix typo in swagger. (#1982)
* Proposal for API versioning system. (#1958)
* Update federation error documentation after changes to the federation API (#1956, #1975, #1978)

## Internal changes

* Suspend/unsuspend teams in backoffice/stern. (#1977)
* Set request ID correctly in galley logs (#1967)
* Improve cabal make targets: faster installation and better support for building and testing all packages (#1979)
* sftd chart: add config key `additionalArgs` (#1972)

## Federation changes

* Add cargohold as a new federated component (#1973)


# [2021-12-02]

## Release notes

* Breaking change to the `fake-aws-s3` (part of `fake-aws`) helm chart. We now use minio helm chart from https://charts.min.io. The options are documented [here](https://github.com/minio/minio/tree/master/helm/minio) (#1944)

  Before running the upgrade, the operators must use `kubectl edit deployment fake-aws-s3` and explicitly set `spec.template.spec.containers[0].serviceAccount` and `spec.template.spec.containers[0].serviceAccountName` to null. (#1944)
* Upgrade team-settings version to 4.3.0-v0.28.28-a2f11cf (#1856)
* Upgrade webapp version to 2021-12-02-production.0-v0.28.29-0-ec2fa00 (#1954)

## Features

* By default install elasticsearch version 6.8.18 when using the elasticsearch-ephemeral chart (#1952)
* Use fluent-bit chart from fluent.github.io instead of deprecated charts.helm.sh. Previous fluent-bit values are not compatible with the new chart, the documentation for the new chart can be found [here](https://github.com/fluent/helm-charts/tree/main/charts/fluent-bit) (#1952)
* Use kibana chart from helm.elastic.co instead of deprecated charts.helm.sh. Previous kibana values are not compatible with the new chart, the documentation for the new chart can be found [here](https://github.com/elastic/helm-charts/tree/main/kibana). This also upgrades kibana to version 6.8.18. (#1952)
* Use kube-prometheus-stack instead of prometheus-operator and update grafana dashboards for compatibility and add federation endpoints to relevant queries. (#1915)
* Add log format called 'StructuredJSON' for easier log aggregation (#1951)

## Bug fixes and other updates

* elasticsearch-ephemeral: Disable automatic creation of indices (#1949)

## Documentation

* Document the wire-server PR process better. (#1934)
* Remove documentation of unsupported scim end-point use case. (#1941)
* Document servant setup and combinators (#1933)

## Internal changes

* Add in-memory interpreters for most Spar effects (#1920)
* Use minio helm chart in fake-aws-s3 from charts.min.io instead of helm.min.io, the latter seems to be down (#1944)
* Upgrade to polysemy-1.7.0.0
   (#1932)
* Replace Galley monad with polysemy's Sem throughout Galley (#1917)
* Separate VerdictFormatStore effect from AReqIdStore effect (#1925)

## Federation changes

* The server-to-server API now uses HTTP2 directly instead of gRPC (#1930)
* Errors when leaving a conversation are now correctly handled instead of resulting in a generic federation error. (#1928)


# [2021-11-15] (Chart Release 2.118.0)

## Release notes

* In case you use a multi-datacentre cassandra setup (most likely you do not), be aware that now [LOCAL_QUORUM](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/dml/dmlConfigConsistency.html) is in use as a default. (#1884)
* Deploy galley before brig. (#1857)
* Upgrade webapp version to 2021-11-01-production.0-v0.28.29-0-d919633 (#1856)

## API changes

* Remove locale from publicly facing user profiles (but not from the self profile) (#1888)

## Features

* End-points for configuring self-deleting messages. (#1857)

## Bug fixes and other updates

* Ensure that all endpoints have a correct handler in prometheus metrics (#1919)
* Push events when AppLock or SelfDeletingMessages config change. (#1901)

## Documentation

* Federation: Document how to deploy local builds (#1880)

## Internal changes

* Add a 'filterNodesByDatacentre' config option useful during cassandra DC migration (#1886)
* Add ormolu to the direnv, add a GH Action to ensure formatting (#1908)
* Turn placeholder access effects into actual Polysemy effects. (#1904)
* Fix a bug in the IdP.Mem interpreter, and added law tests for IdP (#1863)
* Introduce fine-grained error types and polysemy error effects in Galley. (#1907)
* Add polysemy store effects and split off Cassandra specific functionality from the Galley.Data module hierarchy (#1890, #1906)
* Make golden-tests in wire-api package a separate test suite (for faster feedback loop during development). (#1926)
* Separate IdPRawMetadataStore effect from IdP effect (#1924)
* Test sending message to multiple remote domains (#1899)
* Use cabal to build wire-server (opt-in) (#1853)

## Federation changes

* Close GRPC client after making a request to a federator. (#1865)
* Do not fail user deletion when a remote notification fails (#1912)
* Add a one-to-one conversation test in getting conversations in the federation API (#1899)
* Notify remote participants when a user leaves a conversation because they were deleted (#1891)

# [2021-10-29] (Chart Release 2.117.0)

## Release notes

* Upgrade SFT to 2.1.15 (#1849)
* Upgrade team settings to Release: [v4.3.0](https://github.com/wireapp/wire-team-settings/releases/tag/v4.3.0) and image tag: 4.3.0-v0.28.28-a2f11cf (#1950)
* Upgrade Webapp to image tag: 20021-10-28-federation-m1 (#1856)

## API changes

* Remove `POST /list-conversations` endpoint. (#1840)
* The member.self ID in conversation endpoints is qualified and available as
  "qualified_id". The old unqualified "id" is still available. (#1866)

## Features

* Allow configuring nginz so it serve the deeplink for apps to discover the backend (#1889)
* SFT: allow using TURN discovery using 'turnDiscoveryEnabled' (#1519)

## Bug fixes and other updates

* Fix an issue related to installing the SFT helm chart as a sub chart to the wire-server chart. (#1677)
* SAML columns (Issuer, NameID) in CSV files with team members. (#1828)

## Internal changes

* Add a 'make flake-PATTERN' target to run a subset of tests multiple times to trigger a failure case in flaky tests (#1875)
* Avoid a flaky test to fail related to phone updates and improve failure output. (#1874)
* Brig: Delete deprecated `GET /i/users/connections-status` endpoint. (#1842)
* Replace shell.nix with direnv + nixpkgs.buildEnv based setup (#1876)
* Make connection DB functions work with Qualified IDs (#1819)
* Fix more Swagger validation errors. (#1841)
* Turn `Galley` into a polysemy monad stack. (#1881)
* Internal CI tooling improvement: decrease integration setup time by using helmfile. (#1805)
* Depend on hs-certificate master instead of our fork (#1822)
* Add internal endpoint to insert or update a 1-1 conversation. This is to be used by brig when updating the status of a connection. (#1825)
* Update helm to 3.6.3 in developer tooling (nix-shell) (#1862)
* Improve the `Qualified` abstraction and make local/remote tagging safer (#1839)
* Add some new Spar effects, completely isolating us from saml2-web-sso interface (#1827)
* Convert legacy POST conversations/:cnv/members endpoint to Servant (#1838)
* Simplify mock federator interface by removing unnecessary arguments. (#1870)
* Replace the `Spar` newtype, instead using `Sem` directly. (#1833)

## Federation changes

* Remove remote guests as well as local ones when "Guests and services" is disabled in a group conversation, and propagate removal to remote members. (#1854)
* Check connections when adding remote users to a local conversation and local users to remote conversations. (#1842)
* Check connections when creating group and team conversations with remote members. (#1870)
* Server certificates without the "serverAuth" extended usage flag are now rejected when connecting to a remote federator. (#1855)
* Close GRPC client after making a request to a remote federator. (#1865)
* Support deleting conversations with federated users (#1861)
* Ensure that the conversation creator is included only once in notifications sent to remote users (#1879)
* Allow connecting to remote users. One to one conversations are not created yet. (#1824)
* Make federator's default log level Info (#1882)
* The creator of a conversation now appears as a member when the conversation is fetched from a remote backend (#1842)
* Include remote connections in the response to `POST /list-connections` (#1826)
* When a user gets deleted, notify remotes about conversations and connections in chunks of 1000 (#1872, #1883)
* Make federated requests to multiple backends in parallel. (#1860)
* Make conversation ID of `RemoteConversation` unqualified and move it out of the metadata record. (#1839)
* Make the conversation creator field in the `on-conversation-created` RPC unqualified. (#1858)
* Update One2One conversation when connection status changes (#1850)

# [2021-10-01] (Chart Release 2.116.0)

## Release notes

* Deploy brig before galley (#1811, #1818)
* The conference call initiation feature can now be configured for personal accounts in `brig.yaml`.  `enabled` is the default and the previous behavior.  If you want to change that, read [/docs/reference/config-options.md#conference-calling-1](https://github.com/wireapp/wire-server/blob/develop/docs/reference/config-options.md#conference-calling-1) (#1811, #1818)
* Only if you are an early adopter of multi-team IdP issuers on release [2021-09-14](https://github.com/wireapp/wire-server/releases/tag/v2021-09-14): note that the [query parameter for IdP creation has changed](https://github.com/wireapp/wire-server/pull/1763/files#diff-bd66bf2f3a2445e08650535a431fc33cc1f6a9e0763c7afd9c9d3f2d67fac196).  This only affects future calls to this one end-point. (#1763)
* For wire.com cloud operators: reminder to also deploy nginz. (No special action needed for on-premise operators) (#1773)

## API changes

* Add endpoint `POST /connections/:domain/:userId` to create a connection (#1773)
* Deprecate `PUT /conversations/:cnv/access` endpoint (#1807)
* Deprecate `PUT /conversations/:cnv/message-timer` endpoint (#1780)
* Deprecate `PUT /conversations/:cnv/members/:usr` endpoint (#1784)
* Deprecate `PUT /conversations/:cnv/receipt-mode` endpoint (#1797)
* Add endpoint `GET /connections/:domain/:userId` to get a single connection (#1773)
* Add `POST /list-connections` endpoint to get connections (#1773)
* Add qualified endpoint for updating conversation access (#1807)
* Add qualified endpoint for updating message timer (#1780)
* Add qualified endpoint for updating conversation members (#1784)
* Add qualified endpoint for updating receipt mode (#1797)
* Add endpoint `PUT /connections/:domain/:userId` to update a connection (#1773)

## Features

* Helm charts to deploy [ldap-scim-bridge](https://github.com/wireapp/ldap-scim-bridge) (#1709)
* Per-account configuration of conference call initiation (details: /docs/reference/config-options.md#conference-calling-1) (#1811, #1818)

## Bug fixes and other updates

* An attempt to create a 3rd IdP with the same issuer was triggering an exception. (#1763)
* When a user was auto-provisioned into two teams under the same pair of `Issuer` and `NameID`, they where directed into the wrong team, and not rejected. (#1763)

## Documentation

* Expand documentation of `conversations/list-ids` endpoint (#1779)
* Add documentation of the multi-table paging abstraction (#1803)
* Document how to use IdP issuers for multiple teams (#1763)
* All named Swagger schemas are now displayed in the Swagger UI (#1802)

## Internal changes

* Abstract out multi-table-pagination used in list conversation-ids endpoint (#1788)
* Testing: rewrite monadic to applicative style generators (#1782)
* Add a test checking that creating conversations of exactly the size limit is allowed (#1820)
* Rewrite the DELETE /self endpoint to Servant (#1771)
* Fix conversation generator in mapping test (#1778)
* Polysemize spar (#1806, #1787, #1793, #1814, #1792, #1781, #1786, #1810, #1816, #1815)
* Refactored a few functions dealing with conversation updates, in an attempt to
  make the conversation update code paths more uniform, and also reduce special
  cases for local and remote objects. (#1801)
* Merged http2-client fixes as mentioned in the comments of #1703 (#1809)
* Some executables now have a runtime dependency on ncurses (#1791)
* Minor changes around SAML and multi-team Issuers.
  - Change query param to not contain `-`, but `_`.  (This is considered an internal change because the feature has been release in the last release, but only been documented in this one.)
  - Haddocks.
  - Simplify code.
  - Remove unnecessary calls to cassandra. (#1763)
* Clean up JSON Golden Tests (Part 6) (#1769)
* Remove explicit instantiations of ErrorDescription (#1794)
* Remove one flaky integration test about ordering of search results (#1798)
* Report all failures in JSON golden tests in a group at once (#1746)
* Convert the `PUT /conversations/:cnv/access` endpoint to Servant (#1807)
* Move /connections/* endpoints to Servant (#1770)
* Servantify Galley's DELETE /i/user endpoint (#1772)
* Convert the `PUT /conversations/:cnv/message-timer` endpoint to Servant (#1780)
* Convert the `PUT /conversations/:cnv/members/:usr` endpoint to Servant (#1796)
* Convert the `PUT /conversations/:cnv/receipt-mode` endpoint to Servant (#1797)
* Expose wire.com internal EJDP process to backoffice/stern. (#1831)
* Update configurable boolean team feature list in backoffice/stern. (#1829)
* Handle upper/lower case more consistently in scim and rich-info data. (#1754)

## Federation changes

* Add value for verification depth of client certificates in federator ingress (#1812)
* Document federation API conventions and align already existing APIs (#1765)
* Notify remote users when a conversation access settings are updated (#1808)
* Notify remote users when a conversation member role is updated (#1785)
* Notify remote users when a conversation message timer is updated (#1783)
* Notify remote users when a conversation is renamed (#1767)
* Make sure that only users that are actually part of a conversation get notified about updates in the conversation metadata (#1767)
* Notify remote users when a conversation receipt mode is updated (#1801)
* Implement updates to remote members (#1785)
* Make conversation ID of the on-conversation-created RPC unqualified (#1766)
* 4 endpoints for create/update/get/list connections designed for remote users in mind. So far, the implementation only works for local users (actual implementation will come as a follow-up) (#1773)
* The returned `connection` object now has a `qualified_to` field with the domain of the (potentially remote) user. (#1773)
* Add migration for remote connection table (#1789)
* Remove a user from remote conversations upon deleting their account (#1790)
* Remove elasticsearch specific details from the search endpoint (#1768)
* Added support for updating self member status of remote conversations (#1753)


# [2021-09-14] (Chart Release 2.115.0)

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
* Support using a single IDP with a single EntityID (aka issuer ID) to set up two teams. Sets up a migration, and makes teamID + EntityID unique, rather than relying on EntityID to be unique. Required to support multiple teams in environments where the IDP software cannot present anything but one EntityID (E.G.: DualShield). (#1755)

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


# [2021-09-08] (Chart Release 2.114.0)

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

# [2021-08-27] (Chart Release 2.113.0)

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


# [2021-08-16] (Chart Release 2.112.0)

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

# [2021-08-02] (Chart Release 2.111.0)

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

# [2021-07-09] (Chart Release 2.110.0)

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


# [2021-06-23] (Chart Release 2.109.0)

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


# [2021-06-08] (Chart Release 2.108.0)

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


# [2021-05-26] (Chart Release 2.107.0)

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


# [2021-05-04] (Chart Release 2.105.0)

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


# [2021-03-23] (Chart Release 2.104.0)

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

# [2021-03-02] (Chart Release 2.102.0)

## Bug fixes and other updates

* Return PubClient instead of Client from /users/list-clients (#1391)

## Internal changes

* Federation: Add qualified endpoints for prekey management (#1372)


# [2021-02-25] (Chart Release 2.101.0)

## Bug fixes and other updates

* Pin kubectl image in sftd chart (#1383)
* Remove imagePullPolicy: Always for reaper chart (#1387)


## Internal changes

* Use mu-haskell to implement one initial federation request across backends (#1319)
* Add migrate-external-ids tool (#1384)


# [2021-02-16] (Chart Release 2.99.12)

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


# [2021-01-15] (Chart Release 3.30.6)

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


# [2021-01-12] (Chart Release 2.97.0)

## Release Notes

This release contains bugfixes and internal changes.

## Bug fixes and other updates

* [SCIM] Fix bug: Deleting a user retains their externalId (#1323)
* [SCIM] Fix bug: Provisioned users can update update to email, handle, name (#1320)

## Internal changes

* [SCIM] Add logging to SCIM ops, invitation ops, createUser (#1322) (#1318)
* Upgrade nixpkgs and add HLS to shell.nix (#1314)
* create_test_team_scim.sh script: fix arg parsing and invite (#1321)


# [2021-01-06] (Chart Release 2.95.18)

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


# [2020-12-21] (Chart Release 2.95.0)

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
