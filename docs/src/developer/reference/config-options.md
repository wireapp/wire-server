# Config Options

Reference: {#RefConfigOptions}

Fragment.

This page is about the yaml files that determine the configuration of
the Wire backend services.

## MLS private key paths

Note: This developer documentation. Documentation for site operators can be found here: [Messaging Layer Security (MLS)](../../understand/mls.md#mls-message-layer-security)

The `mlsPrivateKeyPaths` field should contain a mapping from *purposes* and
signature schemes to file paths of corresponding x509 private keys in PEM
format.

At the moment, the only purpose is `removal`, meaning that the key will be used
to sign external remove proposals.

For example:

```default
  mlsPrivateKeyPaths:
    removal:
      ed25519: /etc/secrets/ed25519.pem
      ecdsa_secp256r1_sha256: /etc/secrets/ecdsa_secp256r1_sha256
      ecdsa_secp384r1_sha384: /etc/secrets/ecdsa_secp384r1_sha384
      ecdsa_secp521r1_sha512: /etc/secrets/ecdsa_secp521r1_sha512
```

A simple way to generate an ed25519 private key, discarding the corresponding
certificate, is to run the following command:

```default
openssl genpkey -algorithm ed25519
```

ECDSA private keys can be generated with:

```default
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-256
```

and similar (replace `P-256` with `P-384` or `P-521`).

## Feature flags

> Also see [Wire docs](../../understand/team-feature-settings.md) where some of the feature flags are documented from an operations point of view.

Feature flags can be used to turn features on or off, or determine the
behavior of the features. Example:

```default
# [galley.yaml]
settings:
  featureFlags:
    sso: disabled-by-default
    legalhold: disabled-by-default
    teamSearchVisibility: disabled-by-default
    setEmailVisibility: visible_to_self
```

The `featureFlags` field in the galley settings is mandatory, and all
features must be listed.  Each feature defines its own set of allowed
flag values.  (The reason for that is that as we will see, the
semantics is slightly different (or more specific) than boolean.)

NOTE: some feature flags can also be changed for individual teams via
the backoffice app, or the internal rest API
([example](https://staging-nginz-https.zinfra.io/api-internal/swagger-ui/galley/#/galley/iget_ChannelsConfigB)).

### SSO

This sets the default setting for all teams, and can be overridden by
customer support / backoffice.  [Allowed
values](https://github.com/wireapp/wire-server/blob/46713382a1a6544de3936eb03e987b9f76df3faa/libs/galley-types/src/Galley/Types/Teams.hs#L327-L329):
`disabled-by-default`, `enabled-by-default`.

IMPORTANT: if you change this from ‘enabled-by-default’ to
‘disabled-by-default’ in production, you need to run [this migration
script](https://github.com/wireapp/wire-server/tree/master/tools/db/migrate-sso-feature-flag)
to fix all teams that have registered an idp.  (if you don’t, the idp
will keep working, but the admin won’t be able to register new idps.)

### LegalHold

Optionally block customer support / backoffice from enabling legal
hold for individual teams.  [Allowed
values](https://github.com/wireapp/wire-server/blob/46713382a1a6544de3936eb03e987b9f76df3faa/libs/galley-types/src/Galley/Types/Teams.hs#L332-L334):
‘disabled-permanently’, ‘disabled-by-default’.

IMPORTANT: If you switch this back to `disabled-permanently` from
`disabled-by-default`, LegalHold devices may still be active in teams
that have created them while it was allowed.  This may change in the
future.

### Expose invitation URLs to team admin

For further processing (e.g. sending custom emails or rendering the URLs as QR
codes), team invitation URLs can be made part of the result of
`GET /teams/{tid}/invitations`.

```json
{
    "has_more": false,
    "invitations": [
        {
            "created_at": "2022-09-15T15:47:28.577Z",
            "created_by": "375f56fe-7f12-4c0c-aed8-d48c0326d1fb",
            "email": "foo@example.com",
            "id": "4decf7f8-bdd4-43b3-aaf2-e912e2c0c46f",
            "name": null,
            "phone": null,
            "role": "member",
            "team": "51612209-3b61-49b0-8c55-d21ae65efc1a",
            "url": "http://127.0.0.1:8080/register?team=51612209-3b61-49b0-8c55-d21ae65efc1a&team_code=RpxGkK_yjw8ZBegJuFQO0hha-2Tneajp"
        }
    ]
}
```

This can be a privacy issue as it allows the team admin to impersonate as
another team member. The feature is disabled by default.

To activate this feature two steps are needed. First, the team id (tid) has to
be added to the list of teams for which this feature *can* be enabled
(`exposeInvitationURLsTeamAllowlist`). This is done in `galley`’s `values.yaml`:

```yaml
settings:
  exposeInvitationURLsTeamAllowlist: ["51612209-3b61-49b0-8c55-d21ae65efc1a", ...]
```

Then, the feature can be set for the team by enabling the
`exposeInvitationURLsToTeamAdmin` flag. This is done by making a `PUT` request
to `/teams/{tid}/features/exposeInvitationURLsToTeamAdmin` with the body:

```json
{
    "status": "enabled"
}
```

### Team searchVisibility

The team flag `searchVisibility` affects the outbound search of user
searches. If it is set to `no-name-outside-team` for a team then all users of
that team will no longer be able to find users that are not part of their team
when searching. This also includes finding other users by by providing their
exact handle. By default it is set to `standard`, which doesn’t put any
additional restrictions to outbound searches.

The setting can be changed via endpoint:

```default
GET /teams/{tid}/search-visibility
  -- Shows the current TeamSearchVisibility value for the given team

PUT /teams/{tid}/search-visibility
  -- Set specific search visibility for the team

pull-down-menu "body":
  "standard"
  "no-name-outside-team"
```

The team feature flag `teamSearchVisibility` determines whether it is allowed to change the `searchVisibility` setting or not.
The default is `disabled-by-default`. Note that whenever this feature setting is disabled the `searchVisibility` will be reset to `standard`.

The default setting that applies to all teams on the instance can be defined at configuration

```yaml
settings:
  featureFlags:
    teamSearchVisibility: disabled-by-default # or enabled-by-default
```

### TeamFeature searchVisibilityInbound

The team feature flag `searchVisibilityInbound` affects if the team’s users are
searchable by users from *other* teams. The default setting is
`searchable-by-own-team` which hides users from search results by users from
other teams. If it is set to `searchable-by-all-teams` then users of this team
may be included in the results of search queries by other users.

Note: The configuration of this flag does *not* affect search results when the
search query matches the handle exactly. If the handle is provdided then any user on the instance can find users.

This team feature flag can only by toggled by site-administrators with direct access to the galley instance:

```default
PUT /i/teams/{tid}/features/search-visibility-inbound
with JSON body {"status": "enabled"} or body {"status": disabled}
```

where `enabled` is equivalent to `searchable-by-all-teams` and disabled is equivalent to `searchable-by-own-team`.

The default setting that applies to all teams on the instance can be defined at configuration.

```yaml
searchVisibilityInbound:
  defaults:
    status: enabled # OR disabled
```

Individual teams can overwrite the default setting.

### Email Visibility

[Allowd values](https://github.com/wireapp/wire-server/blob/0126651a25aabc0c5589edc2b1988bb06550a03a/services/brig/src/Brig/Options.hs#L304-L306) and their [description](https://github.com/wireapp/wire-server/blob/0126651a25aabc0c5589edc2b1988bb06550a03a/services/brig/src/Brig/Options.hs#L290-L299).

### Classified domains

To enable classified domains, see the documentation on classified domains: classified-domains

### Conference Calling

The `conferenceCalling` feature flag controls whether a user can initiate a conference call. The flag can be toggled between its states `enabled` and `disabled` per team via an internal endpoint.

The `conferenceCalling` section in `featureFlags` defines the state of the `conferenceCalling` feature flag for all personal users (users that don’t belong to a team). For personal users there is no way to toggle the flag, so the setting of the config section wholly defines the state of `conferenceCalling` flag for all personal users.

The `conferenceCalling` section in `featureFlags` also defines the *initial* state of the `conferenceCalling` flag for all teams. After the flag is set for the first time for a team via the internal endpoint the value from the config section will be ignored.

Example value for the config section:

```yaml
conferenceCalling:
  defaults:
    status: enabled
```

The `conferenceCalling` section is optional in `featureFlags`. If it is omitted then it is assumed to be `enabled`.

See also: conference falling for personal accounts (below).

### File Sharing

File sharing is enabled and unlocked by default. If you want a different configuration, use the following syntax:

```yaml
fileSharing:
  defaults:
    status: disabled|enabled
    lockStatus: locked|unlocked
```

These are all the possible combinations of `status` and `lockStatus`:

| `status`   | `lockStatus` |                                                   |
| ---------- | ------------ | ------------------------------------------------- |
| `enabled`  | `locked`     | Feature enabled, cannot be disabled by team admin |
| `enabled`  | `unlocked`   | Feature enabled, can be disabled by team admin    |
| `disabled` | `locked`     | Feature disabled, cannot be enabled by team admin |
| `disabled` | `unlocked`   | Feature disabled, can be enabled by team admin    |

The lock status for individual teams can be changed via the internal API (`PUT /i/teams/:tid/features/fileSharing/(un)?locked`).

The feature status for individual teams can be changed via the public API (if the feature is unlocked).

### Validate SAML Emails

The feature only affects email address changes originating from SCIM or SAML.  Personal users and team users provisioned through the team management app will *always* be validated.

`enabled` means "user has authority over email address": if a new user account with an email address is created, the user behind the account will receive a validation email.  If they follow the validation procedure, they will be able to receive emails about their account, eg., if a new device is associated with the account.  If the user does not validate their email address, they can still use it to login.

`disabled` means "team admin has authority over email address, and by extension over all member accounts": if a user account with an email address is created, the address is considered valid immediately, without any emails being sent out, and without confirmation from the recipient.

Validate SAML emails is enabled by default.  To disable, use the following syntax:

```yaml
# galley.yaml
validateSAMLEmails:
  defaults:
    status: disabled
```

### 2nd Factor Password Challenge

By default Wire enforces a 2nd factor authentication for certain user operations like e.g. activating an account, changing email or password, or deleting an account.
If this feature is enabled, a 2nd factor password challenge will be performed for a set of additional user operations like e.g. for generating SCIM tokens, login, or adding a client.

Usually the default is what you want. If you explicitly want to enable the feature, use the following syntax:

```yaml
# galley.yaml
sndFactorPasswordChallenge:
  defaults:
    status: disabled|enabled
    lockStatus: locked|unlocked
```

### MLS

If this feature is enabled then clients that support the MLS feature will allow its user to switch between Proteus and the MLS protocol provided the user is listed in `protocolToggleUsers`. The default protocol that clients will create new conversations with is specified in `defaultProtocol`. The `supportedProtocols` array is an ordered list of protocols which may be used by the client. It is used to determine the protocol to use for 1:1 conversations. It must contain the `defaultProtocol`.

The `defaultCipherSuite` and `allowedCipherSuites` contain the default ciphersuite and the allowed ciphersuites that clients should be using. The numerical values should correspond to the indices (starting at 1) specified [here](https://www.rfc-editor.org/rfc/rfc9420.html#table-6).

If the MLS feature is disabled then clients will use the Proteus protocol with this backend.

The default configuration that applies to all teams that didn’t explicitly change their feature configuration can be given in galley’s `featureFlags` section in the config file:

```yaml
# galley.yaml
mls:
  defaults:
    status: enabled
    config:
      protocolToggleUsers: []
      defaultProtocol: mls
      supportedProtocols: [proteus, mls] # must contain defaultProtocol
      allowedCipherSuites: [2]
      defaultCipherSuite: 2
    lockStatus: locked
```

This default configuration can be overriden on a per-team basis through the [feature config API](../developer/features.md)

### MLS End-to-End Identity

The MLS end-to-end identity team feature adds an extra level of security and practicability. If turned on, automatic device authentication ensures that team members know they are communicating with people using authenticated devices. Team members get a certificate on all their devices.

When a client first tries to fetch or renew a certificate, they may need to login to an identity provider (IdP) depending on their IdP domain authentication policy. The user may have a grace period during which they can “snooze” this login. The duration of this grace period (in seconds) is set in the `verificationDuration` parameter, which is enforced separately by each client. After the grace period has expired, the client will not allow the user to use the application until they have logged to refresh the certificate. The default value is 1 day (86400s).

The client enrolls using the Automatic Certificate Management Environment (ACME) protocol [RFC 8555](https://www.rfc-editor.org/rfc/rfc8555.html). The `acmeDiscoveryUrl` parameter must be set to the HTTPS URL of the ACME server discovery endpoint for this team. It is of the form “https://acme.{backendDomain}/acme/{provisionerName}/discovery”. For example: `https://acme.example.com/acme/provisioner1/discovery`.

`useProxyOnMobile` is an optional field. If `true`, mobile clients should use the CRL proxy. If missing, null or false, mobile clients should not use the CRL proxy.

`crlProxy` contains the URL to the CRL proxy. (Not that this field is optional in the server config, but mandatory when the team feature is updated via the team feature API.)

```yaml
# galley.yaml
mlsE2EId:
  defaults:
    status: disabled
    config:
      verificationExpiration: 86400
      acmeDiscoveryUrl: null
      useProxyOnMobile: true
      crlProxy: https://example.com
    lockStatus: unlocked
```

#### Key for DPoP access token signing

The key for signing DPoP access tokens has to be configured at path `brig.secrets.dpopSigKeyBundle` e.g. as follows:

```yaml
brig:
  secrets:
    dpopSigKeyBundle: |
      -----BEGIN PRIVATE KEY-----
      MIGHAgEAMBMGByqGSM49....
      -----END PRIVATE KEY-----
```

The corresponding public key has to be known by the ACME server.

The key must be an ECDSA P-256 key and can be created with the following `openssl` command:

```shell
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-256 --out private.pem
```

To get the public key run:

```shell
openssl ec -in private.pem -pubout --out public.pem
```

### Channels

Channels are configured per team (as "team feature".) The configuration defines
if the channels feature is activated and who can create or open channels.

The configuration has these fields:

- `lockStatus`: `locked` or `unlocked`
- `status`: `enabled` or `disabled`
- `config.allowed_to_create_channels`: _roles_
- `config.allowed_to_open_channels`: _roles_

Possible _roles_ are:

| Value          | Who is that?                                    | UX description                    |
| -------------- | ----------------------------------------------- | --------------------------------- |
| `everyone`     | Partner (a.k.a. external), Member, Admin, Owner | Everyone in the team              |
| `team-members` | Member, Admin, Owner                            | Team members (excluding external) |
| `admins`       | Admin, Owner                                    | Only admins                       |

Configuration example (in Galley's config):

```yaml
settings:
---
featureFlags:
---
channels:
  defaults:
    status: enabled
    lockStatus: unlocked
    config:
      allowed_to_create_channels: everyone
      allowed_to_open_channels: team-members
```

Default values:

- `lockStatus`: `locked`
- `status`: `disabled`
- `config.allowed_to_create_channels`: `team-members`
- `config.allowed_to_open_channels`: `team-members`

### Federation Domain

Regardless of whether a backend wants to enable federation or not, the operator
must decide what its domain is going to be. This helps in keeping things
simpler across all components of Wire and also enables to turn on federation in
the future if required.

For production uses, it is highly recommended that this domain be configured as
something that is controlled by the operator(s). The backend or frontend do not
need to be available on this domain. As per our current federation design, you
must be able to set an SRV record for `_wire-server-federator._tcp.<domain>`.
This record should have entries which lead to the federator.

**IMPORTANT** Once this option is set, it cannot be changed without breaking
experience for all the users which are already using the backend.

This configuration needs to be made in brig, cargohold and galley (note the
slighly different spelling of the config options).

```yaml
# brig.yaml
optSettings:
  setFederationDomain: example.com
```

```yaml
# cargohold.yaml
settings:
  federationDomain: example.com
```

```yaml
# galley.yaml
settings:
  federationDomain: example.com
```

### Federation allow list

See [Configure federation strategy (whom to federate with) in brig](../../understand/configure-federation.md#configure-federation-strategy-in-brig) (since [PR#3260](https://github.com/wireapp/wire-server/pull/3260)).

### Federation TLS Config

When a federator connects with another federator, it does so over HTTPS. There
are a few options to configure the CA for this:

1. `useSystemCAStore`: Boolean. If set to `True` it will use the system CA.
2. `remoteCAStore`: Maybe Filepath. This config option can be used to specify
   multiple certificates from either a single file (multiple PEM formatted
   certificates concatenated) or directory (one certificate per file, file names
   are hashes from certificate).
3. `clientCertificate`: Maybe Filepath. A client certificate to use when
   connecting to remote federators. If this option is omitted, no client
   certificate is used. If it is provided, then the `clientPrivateKey` option
   (see below) must be provided as well.
4. `clientPrivateKey`: Maybe Filepath. The private key corresponding to the
   `clientCertificate` option above. It is an error to provide only a private key
   without the corresponding certificate.

Both the `useSystemCAStore` and `remoteCAStore` options can be specified, in
which case the stores are concatenated and used for verifying certificates.
When `useSystemCAStore` is set to `false` and `remoteCAStore` is not provided,
all outbound connections will fail with a TLS error as there will be no CA for
verifying the server certificate.

#### Examples

Federate with anyone, no client certificates, use system CA store to verify
server certificates:

```yaml
federator:
  optSettings:
    federationStrategy:
      allowAll:
    useSystemCAStore: true
```

Federate only with `server2.example.com`, use a client certificate and a
specific CA:

```yaml
federator:
  optSettings:
    federationStrategy:
      allowedDomains:
        - server2.example.com
    useSystemCAStore: false
    clientCertificate: client.pem
    clientPrivateKey: client-key.pem
```

### Federation protocols

A backend can restrict creation of new federated conversations according to the protocol used (Proteus or MLS). This is controlled by the `federationProtocols` setting. For example:

```yaml
galley:
  settings:
    federationProtocols: ["mls"]
```

will prevent the creation of a Proteus conversation containing federated users, and will prevent federated users from joining a Proteus conversation on this backend. However, existing Proteus conversations with federated users will continue to function, and users of this backend will be allowed to join new and existing Proteus conversations on federated backends.

### Outlook calendar integration

This feature setting only applies to the Outlook Calendar extension for Wire. As it is an external service, it should only be configured through this feature flag and otherwise ignored by the backend.

Example default configuration:

```yaml
# galley.yaml
outlookCalIntegration:
  defaults:
    status: disabled
    lockStatus: locked
```

### Guest Link Lifetime

To set the validity duration of conversation guest links set `guestLinkTTLSeconds` to the desired number of seconds, maximum 1 year, a value ∈ (0, 31536000]. E.g.

```yaml
# galley.yaml
config:
  settings:
    GuestLinkTTLSeconds: 604800
```

### Limited Event Fanout

To maintain compatibility with clients and their versions that do not implement
the limited event fanout when a team member is deleted, the limited event fanout
flag is used. Its default value `disabled` means that the old-style full event
fanout will take place when a team member is deleted. Set the flag to `enabled`
to send team events only to team owners and administrators.

Example configuration:

```yaml
# galley.yaml
limitedEventFanout:
  defaults:
    status: disabled
```

### Domain Registration

This feature flag is per default disabled and locked. It can be set by `ibis` via the internal API and it is not meant to be configured via team management.

```
# galley.yaml
domainRegistration:
  defaults:
    status: disabled
    lockStatus: locked
```

### Cells

This feature flag enables cells integration in wire-server. By default, it is disabled and locked. When enabled, it allows changing the cells state of a conversation.

```yaml
# galley.yaml
cells:
  defaults:
    status: disabled
    lockStatus: locked
```

### Allowed Global Operations

`allowedGlobalOperations` currently supports a single value `mlsConversationReset` which determines if it is allowed to reset MLS conversations by the client.

This setting cannot be updated in the DB and therefore does not apply to any specific team but instead to the entire backend.

To configure this setting set or update it in the `values.yaml` of galley as follows:

```yaml
# galley.yaml
config:
  settings:
    featureFlags:
      allowedGlobalOperations:
        status: enabled
        config:
          mlsConversationReset: true
```

## Settings in brig

Some features (as of the time of writing this: only
`conferenceCalling`) allow to set defaults for personal accounts in
brig. Those are taken into account in galley’s end-points `GET /feature-configs*`.

To be specific:

### Conference Calling

Two values can be configured for personal accounts: a default for when
the user record contains `null` as feature config, and default that
should be inserted into the user record when creating new users:

```default
# [brig.yaml]
settings:
  setFeatureFlags:
    conferenceCalling:
      defaultForNew:
        status: disabled
      defaultForNull:
        status: enabled
```

You can omit the entire `conferenceCalling` block, but not parts of
it.  Built-in defaults: `defaultForNew: null` (user record attribute
is left empty); `defaultForNull: enabled`.  This maintains behavior
prior to the introduction of this change, while allowing site owners
to postpone the decision about the default setting.

When new users are created, their config will be initialized with
what’s in `defaultForNew`.

When a `null` value is encountered, it is assumed to be
`defaultForNull`.

(Introduced in https://github.com/wireapp/wire-server/pull/1811.)

### SFT configuration

Configuring SFT load balancing can be done in two (mutually exclusive) settings:

1. Configuring a SRV DNS record based load balancing setting

```default
# [brig.yaml]
sft:
  sftBaseDomain: sft.wire.example.com
  sftSRVServiceName: sft
  sftDiscoveryIntervalSeconds: 10
  sftListLength: 20
```

or

1. Configuring a HTTP-based load balancing setting

```default
# [brig.yaml]
settings:
  setSftStaticUrl: https://sft.wire.example.com
```

This setting assumes that the sft load balancer has been deployed with the `sftd` helm chart.

Additionally if `setSftListAllServers` is set to `enabled` (disabled by default) then the `/calls/config/v2` endpoint will include a list of all servers that are load balanced by `setSftStaticUrl` at field `sft_servers_all`. This is required to enable calls between federated instances of Wire.

Calls between federated SFT servers can be enabled using the optional boolean `multiSFT.enabled`. If provided, the field `is_federating` in the response of `/calls/config/v2` will reflect `multiSFT.enabled`’s value.

```default
# [brig.yaml]
multiSFT:
  enabled: true
```

Also, the optional object `sftToken` with its fields `ttl` and `secret` define whether an SFT credential would be rendered in the response of `/calls/config/v2`. The field `ttl` determines the seconds for the credential to be valid and `secret` is the path to the secret shared with SFT to create credentials.

Example:

```default
# [brig.yaml]
sft:
  sftBaseDomain: sft.wire.example.com
  sftSRVServiceName: sft
  sftDiscoveryIntervalSeconds: 10
  sftListLength: 20
  sftToken:
    ttl: 120
    secret: /path/to/secret
```

### Locale

#### setDefaultLocale (deprecated / ignored)

The brig server config option `setDefaultLocale` has been replaced by `setDefaultUserLocale` and `setDefaultTemplateLocale`. Both settings are optional and `setDefaultTemplateLocale` defaults to `EN` and `setDefaultLocale` defaults to `setDefaultTemplateLocale`. If `setDefaultLocale` was not set or set to `EN` before this change, nothing needs to be done. If `setDefaultLocale` was set to any other language other than `EN` the name of the setting should be changed to `setDefaultTemplateLocale`.

#### `setDefaultTemplateLocale`

This option determines the default locale for email templates. The language of the email communication is determined by the user locale (see above). Only if templates of the the locale of the user do not exist or if user locale is not set the `setDefaultTemplateLocale` is used as a fallback. If not set the default is `EN`. This setting should not be changed unless a complete set of templates is available for the given language.

```default
# [brig.yaml]
optSettings:
  setDefaultTemplateLocale: en
```

#### `setDefaultUserLocale`

This option determines which language to use for email communication. It is the default value if none is given in the user profile, or if no user profile exists (eg., if user is being provisioned via SCIM or manual team invitation via the team management app). If not set, `setDefaultTemplateLocale` is used instead.

```default
# [brig.yaml]
optSettings:
  setDefaultUserLocale: en
```

### MLS settings

#### `setEnableMLS`

This option determines whether MLS is supported on this backend. When set to false (or absent), MLS endpoints will fail without performing any action.

#### `setKeyPackageMaximumLifetime`

This option specifies the maximum accepted lifetime of a key package from the moment it is uploaded, in seconds. For example, when brig is configured as follows:

```default
# [brig.yaml]
optSettings:
  setKeyPackageMaximumLifetime: 1296000 # 15 days
```

any key package whose expiry date is set further than 15 days after upload time will be rejected.

### Federated domain specific configuration settings

#### Restrict user search

See [Configure federation strategy (whom to federate with) in brig](../../understand/configure-federation.md#configure-federation-strategy-in-brig) (since [PR#3260](https://github.com/wireapp/wire-server/pull/3260)).

### API Versioning

### OAuth

For more information on OAuth please refer to [oauth](./oauth.md).

En-/Disable OAuth as follows (if not set the default is disabled):

```yaml
# [brig.yaml]
optSettings:
  # ...
  setOAuthEnabled: [true|false]
```

#### JWK

The JSON Web Keys in `test/resources/oauth/` are used to sign and verify OAuth access tokens in the local integration tests.
The path to the JWK can be configured in `brig.integration.yaml` as follows:

```yaml
# [brig.yaml]
optSettings:
  # ...
  setOAuthJwkKeyPair: test/resources/oauth/ed25519.jwk
```

A JWK can be generated with `didkit` e.g. Run `cargo install didkit-cli` to install and `didkit generate-ed25519-key` to generate a JWK.

#### Expiration time

Optionally, configure the OAuth authorization code, access token, and refresh token expiration time in seconds with the following settings:

```yaml
# [brig.yaml]
optSettings:
  # ...
  setOAuthAuthCodeExpirationTimeSecs: 300 # 5 minutes
  setOAuthAccessTokenExpirationTimeSecs: 300 # 5 minutes
  setOAuthRefreshTokenExpirationTimeSecs: 14515200 # 24 weeks
```

For more information on what these settings mean in particular, please refer to [oauth](./oauth.md).

#### Max number of active refresh tokens

The maximum number of active OAuth refresh tokens a user is allowed to have.  Built-in default:

```yaml
# [brig.yaml]
optSettings:
  # ...
  setOAuthMaxActiveRefreshTokens: 10
```

#### Password hashing options

Since release 5.6.0, wire-server can hash passwords with
[argon2id](https://datatracker.ietf.org/doc/html/rfc9106) to be stored at rest.
If you do not do anything, the deployment will still use scrypt.

The password hashing options are set for brig and galley:

```yaml
brig:
  optSettings:
    setPasswordHashingOptions:
      algorithm: # argon2id or scrypt
      # These options only apply to argon2id
      iterations: ...
      memory: ... # memory needed in KiB
      parallelism: ...
galley:
  settings:
    passwordHashingOptions:
      algorithm: # argon2id or scrypt
      # These options only apply to argon2id
      iterations: ...
      memory: ... # memory needed in KiB
      parallelism: ...
```

**Performance implications:** argon2id typically takes longer and uses more
memory than scrypt. So when migrating to it brig and galley pods must be
allocated more resouces according to the chosen paramters.

When configured to use argon2id, the DB will be migrated slowly over time as the
users enter their passwords (either to login or to do other operations which
require explicit password entry). This migration is **NOT** done in reverse,
i.e., if a deployment started with argon2id as the algorithm then chose to move
to scrypt, the passwords already stored will not get rehashed automatically,
however the users will still be able to use them to login.

**NOTE** It is highly recommended to move to argon2id as it will be made the
only available choice for the `algorithm` config option in future.

Due to the performance implications, password hashing has to be rate limited
more than other operations. To allow this, the rate limiting happens at a deeper
level than nginx. It can be configured using these options:

```yaml
brig:
  optSettings:
    setPasswordHashingRateLimit:
      ipAddrLimit:
        burst: 5
        inverseRate: 300000000 # 5 mins, makes it 12 reqs/hour
      userLimit:
        burst: 5
        inverseRate: 60000000 # 1 min, makes it 60 req/hour
      internalLimit:
        burst: 10
        inverseRate: 0 # No rate limiting for internal use
      ipv4CidrBlock: 32 # Only block individual IP addresses
      ipv6CidrBlock: 64 # Block /64 range at a time.
      ipAddressExceptions: []
      maxRateLimitedKeys: 100000 # Estimated memory usage: 4 MB
galley:
  settings:
    passwordHashingRateLimit:
      ipAddrLimit:
        burst: 5
        inverseRate: 300000000 # 5 mins, makes it 12 reqs/hour
      userLimit:
        burst: 5
        inverseRate: 60000000 # 1 min, makes it 60 req/hour
      internalLimit:
        burst: 10
        inverseRate: 0 # No rate limiting for internal use
      ipv4CidrBlock: 32 # Only block individual IP addresses
      ipv6CidrBlock: 64 # Block /64 range at a time.
      ipAddressExceptions: []
      maxRateLimitedKeys: 100000 # Estimated memory usage: 4 MB
```

The above are the default values.

The rate limiting happens using the [Token Bucket
Algorithm](https://en.wikipedia.org/wiki/Token_bucket). The parameters can be
separately configured for:
1. IP Addresses to be used in case of unauthenticated requests using
   `ipAddrLimit`.
2. Users and providers using `userLimit`.
3. Internal usages (like calls from backoffice) using `internalLimit`.

The `ipAddressExceptions` have to be CIDR blocks which can be specified like
`"127.0.0.0/8"` to allow any IP address from `127.0.0.0` to `127.255.255.255` to
by pass the rate limits. To limit one particular IP address, it can be specified
as `127.0.0.1/32`.

#### Disabling API versions

It is possible to disable one ore more API versions. When an API version is disabled it won’t be advertised on the `GET /api-version` endpoint, neither in the `supported`, nor in the `development` section. Requests made to any endpoint of a disabled API version will result in the same error response as a request made to an API version that does not exist.

Each of the services brig, cannon, cargohold, galley, gundeck, proxy, spar should to be configured with the same set of disable API versions in each service’s values.yaml config files.

For example to disable API version v3, you need to configure:

```yaml
# brig's values.yaml
config.optSettings.setDisabledAPIVersions: [ v3 ]

# cannon's values.yaml
config.disabledAPIVersions: [ v3 ]

# cargohold's values.yaml
config.settings.disabledAPIVersions: [ v3 ]

# galley's values.yaml
config.settings.disabledAPIVersions: [ v3 ]

# gundecks' values.yaml
config.settings.disabledAPIVersions: [ v3 ]

# proxy's values.yaml
config.disabledAPIVersions: [ v3 ]

# spar's values.yaml
config.disabledAPIVersions: [ v3 ]
```

The development API version(s) can be disabled either explicitly or by adding the `development` keyword to the list of disabled API versions. E.g.:

```yaml
config.disabledAPIVersions: [ v3, development ]
```

This setting is required to be present for all the services (brig, cannon, cargohold, galley, gundeck, proxy, and spar).

The default value (provided under `charts/<service>/values.yaml`) is `[ development ]` and disables the development versions. To enable all versions including the development versions set the value to be empty: `[]`.

### Team invitation URL for personal users

To configure the team invitation URL for personal users that is sent vai email, `emailSMS.team.tExistingUserInvitationUrl` should be set to the desired URL, e.g.:

```yaml
brig:
  config:
    emailSMS:
      team:
        tExistingUserInvitationUrl: '{{ .Values.accountUrl }}/accept-invitation/?team-code=${code}'
```

In some environments the `team` config section does not exist. In this case brig’s configmap constructs the URL from the account pages URL which then must be set under `externalUrls.accountPages` e.g. as follows:

```yaml
brig:
  config:
    externalUrls:
      accountPages: https://account.wire.com
```

### Enterprise Login Audit Log

Audit logs for any update (POST, PUT, DELETE) of a domain registration via the internal API are sent via email to the specified email address. If not specified no audit logs will be sent.

```yaml
brig:
  config:
    optSettings:
      setAuditLogEmailRecipient: security@wire.com
```

### Ephemeral User Creation

An ephemeral user is a temporary user that can be created without an email address to join conversations via a guest link. The ephemeral user account exists only for a limited period of time, usually 24h. Ephemeral user creation is enabled per default. To disable the feature set `setEphemeralUserCreationEnabled` to `false`:

```yaml
brig:
  config:
    optSettings:
      setEphemeralUserCreationEnabled: false
```

## Settings in cargohold

AWS S3 (or an alternative provider / service) is used to upload and download
assets. The Haddock of
[`CargoHold.Options.AWSOpts`](https://github.com/wireapp/wire-server/blob/develop/services/cargohold/src/CargoHold/Options.hs#L64)
provides a lot of useful information.

## Settings in cannon

### Events websocket inactivity

The /events websocket will close a connection when it detects client inactivity. The inactivity behaviour can be controlled by setting the cannon options `wsOpts.activityTimeout` and `wsOpts.pongTimeout`.

After `wsOpts.activityTimeout` microseconds of client inactivity (including no pings), the server sends a ping, and waits for a corresponding pong for `wsOpts.pongTimeout` microseconds. If no pong is received within this time window, the connection is terminated.

If not specified, both options default to 30 seconds.

## Settings in spar

This section describes the common "single-ingress" (one backend is reachable by
only one domain) case. The multi-ingress case is described in [Multi-Ingress
setup](#multi-ingress-setup).

### SAML

In Helm:

```yaml
config:
...
  appUri: <webapp-uri> # E.g. https://webapp.<domain>
  ssoUri: <service-provider-uri> # E.g. https://nginz-https.<domain>/sso
  contacts:
    - type: <contact-type> # One of ContactTechnical, ContactSupport, ContactAdministrative, ContactBilling, ContactOther
      company: <company-name> # Optional
      email: <contact-email-address> # Optional
      givenName: <name> # Optional
      surname: <name> # Optional
      phone: <phone-number-string> # Optional
...
```

`appUri` and `ssoUri` are mapped to `spAppUri` and `spSsoUri` in the `spar`
configuration respectively.

### SCIM

In Helm:
```yaml
config:
...
    maxScimTokens: <number-of-allowed-scim-tokens>
    scimBaseUri: <scim-endpoint-uri>
```

If not defined, `scimBaseUri` is automatically deduced from `ssoUri` (see
[SAML](#saml).) E.g. `appUri: https://nginz-https.example.com/sso` would be
translated to `scimBaseUri: https://nginz-https.example.com/scim/v2`.


## Multi-Ingress setup

In a multi-ingress setup the backend is reachable via several domains, each
handled by a separate Kubernetes ingress. This is useful to obfuscate the
relationship of clients to each other, as an attacker on TCP/IP-level could only
see domains and IPs that do not obviously relate to each other.
Each of these backend domains represents a virtual backend. N.B. these backend
domains are *DNS domains* only, not to be confused of the “backend domain” term
used for federation (see
[Federation](../../understand/configure-federation.md#configure-federation)).
In single-ingress setups the backend DNS domain and federation backend domain
is usually be the same, but this is not true for multi-ingress setups.

For a multi-ingress setup multiple services need to be configured:

### Nginz

nginz sets [CORS
headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS). To generate
them for multiple domains (usually, *nginz* works with only one root domain `nginx_conf.external_env_domain`)
these need to be defined with `nginx_conf.additional_external_env_domains`.

E.g.

```yaml
nginx_conf:
  additional_external_env_domains:
    - red.example.com
    - green.example.org
    - blue.example.net
```

### Note:
- To enable clients other than the web interface, configure the [deeplinks for each additional domain](../../understand/associate/deeplink.md#host-deeplinks-for-a-multi-ingress-setup). The default deep link will not respond to other domains.

### Cannon

*cannon* sets [CORS
headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) for direct API
accesses by clients. To generate them for multiple domains (usually, *cannon*
works with only one root domain) these need to be defined with
`nginx_conf.additional_external_env_domains`.

E.g.

```yaml
nginx_conf:
  additional_external_env_domains:
    - red.example.com
    - green.example.org
    - blue.example.net
```

### Cargohold

The backend domain of a download request is defined by its `Z-Host` header which
is set by `nginz`. Multi-ingress handling only applies to download requests as
these are implemented by redirects to the S3 assets host for local assets.
Uploads are handled by cargohold directly itself.

For a multi-ingress setup `aws.multiIngress` needs to be configured as a map from backend domain (`Z-Host` header value) to a S3 download endpoint. The `Z-Host` header is set by `nginz` to the
value of the incoming requests `Host` header. If there’s no config map entry for
a provided `Z-Host` in a download request for a local asset, then an error is
returned. When configured the configuration of `s3DownloadEndpoint` is ignored.

This example shows a setup with fake backends *red*, *green* and *blue*:

```yaml
aws:
  # S3 endpoint for internal communication (cargohold -> S3)
  s3Endpoint: http://s3.internal.example

  # This option is ignored when multiIngress is configured
  s3DownloadEndpoint: https://assets.default.example.com

  # Other settings can still be used
  # ...

  # Map from backend domain to S3 download domain
  multiIngress:
    - nginz-https.red.example.com: https://assets.red.example.com
    - nginz-https.blue.example.com: https://assets.blue.example.com
    - nginz-https.green.example.com: https://assets.green.example.com
```

This sequence diagram illustrates how users on different virtual backends
(represented by different Kubernetes ingresses) download local assets according
to the configuration example above:

![Sequence Diagram: Alice and Bob download an asset](multi-ingress-example-sequence.svg)

<!--
Unfortunately, kroki currently doesn't work on our CI: SQPIT-1810
Link to diagram:
https://mermaid.live/edit#pako:eNrdVbFu2zAQ_ZUDJ7ewDdhtUkBDgBRB0CHIYCNL4eVEnmWiMk8lKbttkH8vJbsW5dCOUXSqBkHiPT6-e3yinoVkRSITEC5H32syku40FhbXCwP7C6VnC1hqSQNL6l1XeWRPwBuKqxk8OXKwpRyrahxGxvQD11VJY8mvSHPOB4UlMknSrtonbcfStBVar6Wu0HjQJgCdGwUNKfaonMGMax8WeH9acIq5FXKOuwVE7BcqN4U2v9IlibbgFZcqXZ5_ABeMxYK6uiXpwRb5YHp1NYTJ9FN7ixw3jW6ri5UHXva28rZ5BsVbUzIqB-gc-WgTD9DRzU3Pz7v9FChZYnk8L4KGiW23Gdyz3aJVQW7IoYvQbT3gDq2_wsIIbpWCr6MvHF5WhIpsL2p6g6HFhHePvdajFR6Yv0Fd7ZTDquF9mj3AMoR2t0zHcZg1CiJj92akdGP-OLBJ9JpDFOa73YGNxnRAFZ3Te9rxey5L3gZHdmueMrsLyBnHDwpScerGQr_9dn1tzfFeR_2k2MioRFIn15MhTD82Sb0-ndT4fPjM-emcdsDItf23eVlSW_D_ltXYv0uzenTknU_rOd_fzOsfy_9xYvtN_21ixVCsya5Rq_D3fG6KC-FXtKaFyMKjoiXWpV-IhXkJUKw9z38aKTJvaxqKulKBff-jFdkSS0cvvwHKl250
-->

### Galley

For conversation invite links to be correct in a multi-ingress setup `settings.multiIngress` needs to be configured as map from `Z-Host` to the conversation URI prefix.  This setting is a `Z-Host` depended version of `settings.conversationCodeURI`. In fact `settings.multiIngress` and `settings.conversationCodeURI` are mutually exclusive.

Example:

```yaml
multiIngress:
   red.example.com: https://accounts.red.example.com/conversation-join/
   green.example.com: https://accounts.green.example.net/conversation-join/
```

### Spar

To enable SSO via SAML spar needs to act as multiple *ServiceProvider*s
(*SP*s); one per multi-ingress domain. The configuration is done with a map of
domains to *SP* settings. It replaces the usual (single-domain) *SP*
configuration.

So, we go from single-domain *SP* configuration:

```yaml
config:
...
  appUri: <webapp-uri> # E.g. https://webapp.<domain>
  ssoUri: <service-provider-uri> # E.g. https://nginz-https.<domain>/sso
  contacts:
    - type: <contact-type> # One of ContactTechnical, ContactSupport, ContactAdministrative, ContactBilling, ContactOther
      company: <company-name> # Optional
      email: <contact-email-address> # Optional
      givenName: <name> # Optional
      surname: <name> # Optional
      phone: <phone-number-string> # Optional
...
```

To the multi-domain *SP* configuration:

```yaml
config:
...
  domainConfigs:
    <domain1>: # The domain of the incoming nginz-https host. E.g. nginz-https.<domain>
      appUri: <webapp-uri> # E.g. https://webapp.<domain>
      ssoUri: <service-provider-uri> # E.g. https://nginz-https.<domain>/sso
      contacts:
        - type: <contact-type> # One of ContactTechnical, ContactSupport, ContactAdministrative, ContactBilling, ContactOther
          company: <company-name> # Optional
          email: <contact-email-address> # Optional
          givenName: <name> # Optional
          surname: <name> # Optional
          phone: <phone-number-string> # Optional
    <domain2>:
      ...
```
(N.B. the structures in Helm values and spar's service YAML file are almost the
same. Only the outer `config` object is added to the spar YAML config in Helm.)

The inner data structure stays the same. The difference is that it's on
`config`'s top level for single-ingress and in each `<domain>` entry in
`domainConfigs` for multi-ingress.

In a single-ingress setup the SCIM base URI can be deduced from `ssoUri` in
Helm (see [SAML](#saml).) In a multi-ingress setup this relationship isn't that
clear as there are multiple `ssoUri`s defined. So, the SCIM base URI needs to
be set explicitly in `scimBaseUri`. In spar's YAML config file `scimBaseUri` is
always required.


### Webapp

The webapp runs its own web server (a NodeJS server) to serve static files and the webapp config (based on environment variables).
In a multi-ingress configuration, a single webapp instance will be deployed and be accessible from multiple domains (say `webapp.red.example.com` and `webapp.green.example.com`).
When the webapp is loaded from one of those domains it first does a request to the web server to get the config (that will give it, for example, the backend endpoint that it should hit).

Because of the single instance nature of the webapp, by default the configuration is static and the root url to the backend API can be set there (say `nginz-https.root.example.com`).
In order to completely hide this root domain to the webapp, an environment variable can be set to allow the webapp hostname to be used to generate the API endpoint, team settings links, account page links and CSP headers.

The “hostname” is the result of the domain name minus the `webapp.` part of it.
So querying the webapp on `webapp.red.example.com` will resolve to `red.example.com`.

To enable dynamic hostname replacement, first set this variable:

```default
ENABLE_DYNAMIC_HOSTNAME="true"
```

Then, any other variable that will contain the string `[[hostname]]` will be replaced by the hostname of the running webapp. (eg. if a webapp is running on `webapp.red.example.com` then any occurrence of `[[hostname]]` in the config will be replaced by `red.example.com`).

You may use the template variable `[[hostname]]` in any environment variable to not provide (reveal) actual domain names.

For example:

```default
APP_BASE:                                         https://[[hostname]]
BACKEND_REST:                                     https://nginz-https.[[hostname]]
BACKEND_WS:                                       wss://nginz-ssl.[[hostname]]
CSP_EXTRA_CONNECT_SRC:                            https://*.[[hostname]], wss://*.[[hostname]]
CSP_EXTRA_DEFAULT_SRC:                            https://*.[[hostname]]
CSP_EXTRA_FONT_SRC:                               https://*.[[hostname]]
CSP_EXTRA_FRAME_SRC:                              https://*.[[hostname]]
CSP_EXTRA_IMG_SRC:                                https://*.[[hostname]]
CSP_EXTRA_MANIFEST_SRC:                           https://*.[[hostname]]
CSP_EXTRA_MEDIA_SRC:                              https://*.[[hostname]]
CSP_EXTRA_PREFETCH_SRC:                           https://*.[[hostname]]
CSP_EXTRA_SCRIPT_SRC:                             https://*.[[hostname]]
CSP_EXTRA_STYLE_SRC:                              https://*.[[hostname]]
CSP_EXTRA_WORKER_SRC:                             https://*.[[hostname]]
```

## TLS-encrypted Cassandra connections

By default, all connections to Cassandra by the Wire backend are unencrypted. To
configure client-side TLS-encrypted connections (where the Wire backend is the
client), a **C**ertificate **A**uthority in PEM format needs to be configured.

The ways differ regarding the kind of program:

- *Services* expect a `cassandra.tlsCa: <filepath>` attribute in their config file.
-  *\*-schema CLI commands* accept a `--tls-ca-certificate-file <filepath>` parameter.
- *brig-index migrate-data* accepts a `--cassandra-ca-cert <filepath>` parameter.

When a CA PEM file is configured, all Cassandra connections are opened with TLS
encryption i.e. there is no fallback to unencrypted connections. This ensures
that connections that are expected to be secure, would not silently and
unnoticed be insecure.

In Helm charts, the CA PEM is either provided as multiline string in the
`cassandra.tlsCa` attribute or as a reference to a `Secret` in
`cassandra.tlsCaSecretRef.name` and `cassandra.tlsCaSecretRef.key`. The `name`
is the name of the `Secret`, the `key` is the entry in it. Such a `Secret` can
e.g. be created by `cert-manager`.

The CA may be self-signed. It is used to validate the certificate of the
Cassandra server.

How to configure Cassandra to accept TLS-encrypted connections in general is
beyond the scope of this document. The `k8ssandra-test-cluster` Helm chart
provides an example how to do this for the Kubernetes solution *K8ssandra*. In
the example `cert-manager` generates a `Certificate` including Java KeyStores,
then `trust-manager` creates synchronized `Secret`s to make only the CA PEM
accessible to services (and not the private key.)

The corresponding Cassandra options are described in Cassandra’s documentation:
[client_encryption_options](https://cassandra.apache.org/doc/stable/cassandra/configuration/cass_yaml_file.html#client_encryption_options)

## Configure Elasticsearch basic authentication

When the Wire backend is configured to work against a custom Elasticsearch
instance, it may be desired to enable basic authentication for the internal
communication between the Wire backend and the ES instance. To do so the
Elasticsearch credentials can be set in wire-server’s secrets for `brig` and
`elasticsearch-index` as follows:

```yaml
brig:
  secrets:
    elasticsearch:
      username: elastic
      password: changeme

elasticsearch-index:
  secrets:
    elasticsearch:
      username: elastic
      password: changeme
```

In some cases an additional Elasticsearch instance is needed (e.g. for index
migrations). To configure credentials for the additional ES instance add the
secret as follows:

```yaml
brig:
  secrets:
    elasticsearchAdditional:
      username: elastic
      password: changeme
```

## Configure TLS for Elasticsearch

If the elasticsearch instance requires TLS, it can be configured like this:

```yaml
brig:
  config:
    elasticsearch:
      scheme: https

elasticsearch-index:
  elasticsearch:
    scheme: https
```

In case a custom CA certificate is required it can be provided like this:

```yaml
brig:
  config:
    elasticsearch:
      tlsCa: <PEM encoded CA certificates>
elasticsearch-index:
  elasticsearch:
    tlsCa: <PEM encoded CA certificates>
```

There is another way to provide this, in case there already exists a kubernetes
secret containing the CA certificate(s):

```yaml
brig:
  config:
    elasticsearch:
      tlsCaSecretRef:
        name: <Name of the secret>
        key: <Key in the secret containing pem encoded CA Cert>
elasticsearch-index:
  elasticsearch:
    tlsCaSecretRef:
      name: <Name of the secret>
      key: <Key in the secret containing pem encoded CA Cert>
```

For configuring `addtionalWriteIndex` in brig (this is required during a
migration from one index to another or one ES instance to another), the settings
need to be like this:

```yaml
brig:
  config:
    elasticsearch:
      additionalWriteScheme: https
      # One or none of these:
      # addtionalTlsCa: <similar to tlsCa>
      # addtionalTlsCaSecretRef: <similar to tlsCaSecretRef>
```

**WARNING:** Please do this only if you know what you’re doing.

In case it is not possible to verify TLS certificate of the elasticsearch
server, it can be turned off without tuning off TLS like this:

```yaml
brig:
  config:
    elasticsearch:
      insecureSkipVerifyTls: true
      addtionalInsecureSkipVerifyTls: true # only required when addtional index is being used.
elasticsearch-index:
  elasticsearch:
    insecureSkipVerifyTls: true
```

## Configure Redis authentication

If the redis used needs authentication with either username and password or just
password (legacy auth), it can be configured like this:

```yaml
gundeck:
  secrets:
    redisUsername: <username>
    redisPassword: <password>
```

**NOTE**: When using redis < 6, the `redisUsername` must not be set at all (not
even set to `null` or empty string, the key must be absent from the config).
When using redis >= 6 and using legacy auth, the `redisUsername` must either be
not set at all or set to `"default"`.

While doing migrations to another redis instance, the credentials for the
addtional redis can be set as follows:

```yaml
gundeck:
  secrets:
    redisAdditionalWriteUsername: <username>  # Do not set this at all when using legacy auth
    redisAdditionalWritePassword: <password>
```

**NOTE**: `redisAddtiionalWriteUsername` follows same restrictions as
`redisUsername` when using legacy auth.

## Configure TLS for Redis

If the redis instance requires TLS, it can be configured like this:

```yaml
gundeck:
  config:
    redis:
      enableTls: true
```

In case a custom CA certificate is required it can be provided like this:

```yaml
gundeck:
  config:
    redis:
      tlsCa: <PEM encoded CA certificates>
```

There is another way to provide this, in case there already exists a kubernetes
secret containing the CA certificate(s):

```yaml
gundeck:
  config:
    redis:
      tlsCaSecretRef:
        name: <Name of the secret>
        key: <Key in the secret containing pem encoded CA Cert>
```

For configuring `redisAdditionalWrite` in gundeck (this is required during a
migration from one redis instance to another), the settings need to be like
this:

```yaml
gundeck:
  config:
    redisAdditionalWrite:
      enableTls: true
      # One or none of these:
      # tlsCa: <similar to tlsCa>
      # tlsCaSecretRef: <similar to tlsCaSecretRef>
```

**WARNING:** Please do this only if you know what you’re doing.

In case it is not possible to verify TLS certificate of the redis
server, it can be turned off without tuning off TLS like this:

```yaml
gundeck:
  config:
    redis:
      insecureSkipVerifyTls: true
    redisAdditionalWrite:
      insecureSkipVerifyTls: true
```

## Configure RabbitMQ

RabbitMQ authentication must be configured on brig, galley and background-worker. For example:

```yaml
rabbitmq:
  host: localhost
  port: 5672
  vHost: /
  adminHost: localhost
  adminPort: 15672 # for background-worker
```

the `adminHost` and `adminPort` settings are only needed by background-worker.

In order to enable TLS when connecting to RabbitMQ, the following settings need to be added:

```yaml
rabbitmq:
  enableTls: true
  caCert: test/resources/rabbitmq-ca.pem
  insecureSkipVerifyTls: false
```

**WARNING:** Please do this only if you know what you’re doing.

In case it is not possible to verify the TLS certificate of the RabbitMQ
server, verification can be turned off by settings `insecureSkipVerifyTls` to
`true`.

## Configure PostgreSQL

`brig` requires a PostgreSQL database. The configured user needs to be able to
write data and change the schema (e.g. create and alter tables.)

`brig`'s internal configuration YAML file format and the Helm chart differ a
bit.

Helm chart `values.yaml`:

```yaml
config:
  postgresql:
    host: postgresql # DNS name without protocol
    port: "5432"
    user: wire-server
    dbname: wire-server
secrets:
  pgPassword: user-password # plain text; i.e. not base64 encoded
```

Internal YAML file:

```yaml
postgresql:
  port: "5432"
  user: wire-server
  dbname: wire-server
  password: db-password # plain text passwords should only be used in test setups
postgresqlPassword: /path/to/pgPassword # refers to a PostgreSQL password file
```

The `brig` Helm chart also offers an option to mount files (e.g. certificates)
into the container by defining `additionalVolumeMounts` and
`additionalVolumes`. This way does not work for password files (parameter
`passfile`), because `libpq-connect` requires access rights (mask `0600`) for
them that we cannot provide for random uids (brig is executed as user `brig`
with a random uid.)

The `host` can be any DNS name pointing to the database server; e.g.
`database.example.com` would be fine.
The `port` needs to be a number provided as string.

Besides the password file (`postgresqlPassword`), the fields correspond to
[libpq-connect
parameters](https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-PARAMKEYWORDS).
`postgresqlPassword` is read by `brig`. Its content is used as `password`
field.

## Configure Cells

If Cells integration is enabled, gundeck must be configured with the name of
the RabbitMQ queue where Cells events can be forwarded. This can be done via
the `cellsEventQueue` setting:

```yaml
gundeck:
  settings:
    cellsEventQueue: "cells_events"
```
