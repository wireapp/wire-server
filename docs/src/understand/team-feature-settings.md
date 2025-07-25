# Server and team feature settings

Features can be enabled or disabled on a team level or server wide. Here we will only cover the server wide configuration.

When a feature’s lock status is `unlocked` it means that its settings can be overridden on a team level by team admins. This can be done via the team management app or via the team feature API and is not covered here.

## 2nd factor password challenge

By default Wire enforces a 2nd factor authentication for certain user operations like e.g. activating an account, changing email or password, or deleting an account.

If the `sndFactorPasswordChallenge` feature is enabled, a 6 digit verification code will be send per email to authenticate for additional user operations like e.g. for login, adding a new client, generating SCIM tokens, or deleting a team.

After 3 attempts, the key is invalidated, and requests for generating new verification codes are rate limited. The default delay between two consecutive requests is 5 minutes.

Usually the default is what you want. If you explicitly want to enable additional password challenges, add the following to your Helm overrides in `values/wire-server/values.yaml`:

```yaml
galley:
  # ...
  config:
    # ...
    settings:
      # ...
      featureFlags:
        # ...
        sndFactorPasswordChallenge:
          defaults:
            status: enabled
            lockStatus: locked
```

Note that the lock status is required but has no effect, as it is currently not supported for team admins to enable or disable `sndFactorPasswordChallenge`. We recommend to set the lock status to `locked`.

Currently the 2nd factor password challenge if enabled has no effect for SSO users.

As there is currently no feasible way for bots to use the 2nd factor password challenge, bots and the service API are blocked when this feature is enabled.

## Rate limiting of code generation requests

The default delay between code generation requests is 5 minutes. This setting can be overridden in the Helm charts:

```yaml
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      set2FACodeGenerationDelaySecs: 300 # 5 minutes
```

## Guest links

The guest link feature is the ability for a Wire users to join a group conversation by tapping on a unique link generated by an admin of that group.

The feature is enabled and unlocked by default and can be disabled on a per-team (via team management) basis or disabled and optionally locked for the entire backend (via Helm overrides). If the feature is locked, it cannot be enabled by team admins via team management.

To change the configuration for the entire server, add the following to your Helm overrides in `values/wire-server/values.yaml`:

```yaml
galley:
  # ...
  config:
    # ...
    settings:
      # ...
      featureFlags:
        # ...
        conversationGuestLinks:
          defaults:
            status: disabled
            lockStatus: locked
```

## Self-deleting messages

Self-deleting messages is a feature that makes all sent messages delete after a given period of time after the message has been read. By default, this feature is unlocked and not enabled for all the teams. The feature can be toggled on/off through Team Settings. Team settings has presets that can set up message expiration up to a month. Other time frames can be set either through a backend-wide global setting, or manually through API.

Example of backend-wide global setting:

```yaml
galley:
  # ...
  config:
    # ...
    settings:
      # ...
      featureFlags:
        # ...
        selfDeletingMessages:
          defaults:
            config:
              enforcedTimeoutSeconds: 0 # expressed in seconds
            lockStatus: unlocked # locked | unlocked
            status: enabled # enabled | disabled
```

Locking a feature will prevent team administrators from changing the setting!
Backend-wide settings are not applied retroactively! For teams made before a global setting has been set, feature will have to be set manually through a combination of API calls.

### Self-deleting messages custom time frame with API calls

As an on-prem backend administrator, if you find yourself in need of a time-frame not provided by the Team Setting app, you can make an API call to an internal `galley` endpoint to set it manually.

First, port-forward the galley service:

```bash
d kubectl port-forward svc/galley 8080:8080
```

And make a curl request like in example below, set the appropriate <teamID> (this can be fetched from cassandra with `cqlsh` if you are not a direct owner of the team or the admin user)

```bash
curl -X PUT 'http://localhost:8080/i/teams/<teamID>/features/selfDeletingMessages' -H 'accept: application/json;charset=utf-8' -H 'Content-Type: application/json' --data-raw '{ "config": { "enforcedTimeoutSeconds": 6000}, "status": "enabled" }'
```

For locking/unlocking features for a team make the following request:

```bash
curl -X PUT 'http://localhost:8080/i/teams/<teamID>/features/selfDeletingMessages/<lockStatus>'
```

<lockStatus> = locked | unlocked

## TTL for nonces

Nonces that can be retrieved e.g. by calling `HEAD /nonce/clients` have a default time-to-live of 5 minutes. To change this setting add the following to your Helm overrides in `values/wire-server/values.yaml`:

```yaml
brig:
  # ...
  config:
    # ...
    optSettings:
      # ...
      setNonceTtlSecs: 360 # 6 minutes
```

## MLS End-to-End Identity

The MLS end-to-end identity team feature adds an extra level of security and practicability. If turned on, automatic device authentication ensures that team members know they are communicating with people using authenticated devices. Team members get a certificate on all their devices.

When a client first tries to fetch or renew a certificate, they may need to login to an identity provider (IdP) depending on their IdP domain authentication policy. The user may have a grace period during which they can “snooze” this login. The duration of this grace period (in seconds) is set in the `verificationDuration` parameter, which is enforced separately by each client. After the grace period has expired, the client will not allow the user to use the application until they have logged to refresh the certificate. The default value is 1 day (86400s).

The client enrolls using the Automatic Certificate Management Environment (ACME) protocol [RFC 8555](https://www.rfc-editor.org/rfc/rfc8555.html). The `acmeDiscoveryUrl` parameter must be set to the HTTPS URL of the ACME server discovery endpoint for this team. It is of the form “https://acme.{backendDomain}/acme/{provisionerName}/discovery”. For example: `https://acme.example.com/acme/provisioner1/discovery`.

`useProxyOnMobile` is an optional field. If `true`, mobile clients should use the CRL proxy. If missing, null or false, mobile clients should not use the CRL proxy.

`crlProxy` contains the URL to the CRL proxy. (Not that this field is optional in the server config, but mandatory when the team feature is updated via the team feature API.)

```yaml
galley:
  # ...
  config:
    # ...
    settings:
      # ...
      featureFlags:
        # ...
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

## MLS Migration

The MLS migration configuration determines client behaviour related to
migration from Proteus to MSL, and defines the criteria enforced by the backend
when a conversation is finally migrated to MLS.

The settings are the following:

- `startTime`: migration start timestamp. Once this time arrives, clients will
  initialise the migration process (no migration-related action will take
  place before that time). If the migration feature is enabled, but
  `startTime` value is not set (or is set to `null`), migration is never
  started.
- `finaliseRegardlessAfter`: timestamp of the date by which the migration must
  be finalised.
- `usersThreshold`: percentage of migrated users needed for migration to
  finalise (0-100).
- `clientsThreshold`: percentage of migrated clients needed for migration to
  finalise (0-100).

All of the migration finalisation values are technically optional, but at least
one of them must be specified for the configuration to be valid. If
`finaliseRegardlessAfter` is not set, `usersThreshold` or `clientsThreshold`
should be specified. In case both `usersThreshold` and `clientsThreshold` are
specified, even if one of them is set to 0, both have to be fulfilled for the
migration to be finalised.

The `finaliseRegardlessAfter` timestamp determines a time after which the
threshold criteria are dropped, and finalisation is allowed in any case.

An example configuration follows:

```default
galley:
  # ...
  config:
    # ...
    settings:
      # ...
      featureFlags:
        # ...
        mlsMigration:
          defaults:
            status: enabled
            config:
              startTime: "2024-05-16T00:00:00.000Z"
              finaliseRegardlessAfter: "2024-10-17T00:00:00.000Z"
              usersThreshold: 100
              clientsThreshold: 50
            lockStatus: locked
```
