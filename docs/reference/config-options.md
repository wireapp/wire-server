# Config Options {#RefConfigOptions}

Fragment.

This page is about the yaml files that determine the configuration of
the Wire backend services.

## Settings

```
# [galley.yaml]
settings:
  enableIndexedBillingTeamMembers: false
```

### Indexed Billing Team Members

Use indexed billing team members for journaling. When `enabled`,
galley would use the `billing_team_member` table to send billing
events with user ids of team owners (who have the `SetBilling`
permission). Before enabling this flag, the `billing_team_member`
table must be backfilled.

Even when the flag is `disabled`, galley will keep writing to the
`biling_team_member` table, this flag only affects the reads and has
been added in order to deploy new code and backfill data in
production.

## Feature flags

Feature flags can be used to turn features on or off, or determine the
behavior of the features. Example:

```
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

### SSO

This sets the default setting for all teams, and can be overridden by
customer support / backoffice.  [Allowed
values](https://github.com/wireapp/wire-server/blob/46713382a1a6544de3936eb03e987b9f76df3faa/libs/galley-types/src/Galley/Types/Teams.hs#L327-L329):
`disabled-by-default`, `enabled-by-default`.

IMPORTANT: if you change this from 'enabled-by-default' to
'disabled-by-default' in production, you need to run [this migration
script](https://github.com/wireapp/wire-server/tree/master/tools/db/migrate-sso-feature-flag)
to fix all teams that have registered an idp.  (if you don't, the idp
will keep working, but the admin won't be able to register new idps.)

### LegalHold

Optionally block customer support / backoffice from enabling legal
hold for individual teams.  [Allowed
values](https://github.com/wireapp/wire-server/blob/46713382a1a6544de3936eb03e987b9f76df3faa/libs/galley-types/src/Galley/Types/Teams.hs#L332-L334):
'disabled-permanently', 'disabled-by-default'.

IMPORTANT: If you switch this back to `disabled-permanently` from
`disabled-by-default`, LegalHold devices may still be active in teams
that have created them while it was allowed.  This may change in the
future.

### Team Search Visibility

Is a team allowed to change its team search visibility settings? If enabled
for the team, it can be configured so that non-team users do not show up in search.

This sets the default setting for all teams, and can be overridden for
individual teams by customer support / backoffice. [Allowed
values](https://github.com/wireapp/wire-server/blob/151afec7b1f5a7630a094cf000875fbf9035866d/libs/galley-types/src/Galley/Types/Teams.hs#L229-L235):
`disabled-by-default`, `enabled-by-default`.

Disabled by default in the wire cloud.

[Backoffice hook](https://github.com/wireapp/wire-server/blob/151afec7b1f5a7630a094cf000875fbf9035866d/tools/stern/src/Stern/API.hs#L615-L618) looks like this:

```
GET /teams/{tid}/search-visibility
  -- Shows the current TeamSearchVisibility value for the given team

PUT /teams/{tid}/search-visibility
  -- Set specific search visibility for the team

pull-down-menu "body":
  "standard"
  "no-name-outside-team"
```

### Email Visibility

[Allowd values](https://github.com/wireapp/wire-server/blob/0126651a25aabc0c5589edc2b1988bb06550a03a/services/brig/src/Brig/Options.hs#L304-L306) and their [description](https://github.com/wireapp/wire-server/blob/0126651a25aabc0c5589edc2b1988bb06550a03a/services/brig/src/Brig/Options.hs#L290-L299).

### Classified domains

To enable classified domains, the following needs to be in galley.yaml or wire-server/values.yaml under `settings` / `featureFlags`:

```yaml
classifiedDomains:
  status: enabled
  config:
    domains: ["example.com", "example2.com"]
```

Note that when enabling this feature, it is important to provide your own domain
too in the list of domains. In the example above, `example.com` or `example2.com` is your domain.

To disable, either omit the entry entirely (it is disabled by default), or provide the following:

```yaml
classifiedDomains:
  status: disabled
  config:
    domains: []
```

### Conference Calling

The `conferenceCalling` feature flag controls whether a user can initiate a conference call. The flag can be toggled between its states `enabled` and `disabled` per team via an internal endpoint. 

The `conferenceCalling` section in `featureFlags` defines the state of the `conferenceCalling` feature flag for all personal users (users that don't belong to a team). For personal users there is no way to toggle the flag, so the setting of the config section wholly defines the state of `conferenceCalling` flag for all personal users.

The `conferenceCalling` section in `featureFlags` also defines the _initial_ state of the `conferenceCalling` flag for all teams. After the flag is set for the first time for a team via the internal endpoint the value from the config section will be ignored.

Example value for the config section:
```yaml
conferenceCalling:
  defaults:
    status: enabled
```

The `conferenceCalling` section is optional in `featureFlags`. If it is omitted then it is assumed to be `enabled`.

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

This configuration needs to be made in brig and in galley. (note the slighly different spelling of the config options)

```yaml
# galley.yaml
settings:
  federationDomain: example.com
```

```yaml
# brig.yaml
optSettings:
  setFederationDomain: example.com
```

### Federation allow list

As of 2021-07, federation (whatever is implemented by the time you read this) is turned off by default by means of having an empty allow list:

```yaml
# federator.yaml
optSettings:
  federationStrategy:
    allowedDomains: []
```

You can choose to federate with a specific list of allowed servers:


```yaml
# federator.yaml
optSettings:
  federationStrategy:
    allowedDomains:
      - server1.example.com
      - server2.example.com
```

or, you can federate with everyone:

```yaml
# federator.yaml
optSettings:
  federationStrategy:
    # note the 'empty' value after 'allowAll'
    allowAll:

# when configuring helm charts, this becomes (note 'true' after 'allowAll')
# inside helm_vars/wire-server:
federator:
  optSettings:
    federationStrategy:
      allowAll: true
```

### Federation TLS Config

When a federator connects with another federator, it does so over HTTPS. There
are two options to configure the CA for this:
1. `useSystemCAStore`: Boolean. If set to `True` it will use the system CA.
1. `remoteCAStore`: Maybe Filepath. This config option can be used to specify
   multiple certificates from either a single file (multiple PEM formatted
   certificates concatenated) or directory (one certificate per file, file names
   are hashes from certificate).

Both of these options can be specified, in this case the stores are concatenated
and used for verifying certificates. When `useSystemCAStore` is `False` and
`remoteCAStore` is not set, then all outbound connections will fail with TLS
error as there will be no CA to verify.
