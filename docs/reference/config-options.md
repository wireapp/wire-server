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
behavior of the features.  Example:

```
# [galley.yaml]
settings:
  featureFlags:
    sso: disabled-by-default
    legalhold: disabled-by-default
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
