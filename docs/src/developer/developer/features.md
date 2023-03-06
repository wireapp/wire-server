# Features

Wire has multiple features (or feature flags) that affect the behaviour of
backend and clients, e.g. `sso`, `searchVisibility`, `digitalSignatures`,
`appLock`, `legalhold`, `fileSharing`.

Features cannot by configured by the users themselves, which distinguishes
features from user properties. Who can configure a feature depends on the
feature itself, e.g:

- Team administrators via Wire Team Settings
- Customer support via the backoffice tool
- Site administrators by changing a configuration file on the server

Depending on the feature the configuration can be defined

- for all users on the server (per-instance)
- per-team
- or per-user

or a combination of all three levels, where configuration on a per-user level
may override configuration defined for the the team, which may override
configuration defined at the instance level.  or instance may override team
(eg. when security settings are enforced so team admins do not have the
power to break security policy).  details depend on the individual feature.

## Feature Configuration API

The Feature configurations API exposes the feature configuration that results
from combining the settings (per-instance, per-team, per-user) for the user that
queries the endpoint:

`GET /feature-configs/:feature-name`

```json
{
  "status": "enabled" /* or "disabled" */
  "config": {
    /* ... */
  }
}
```
where the optional `config` field contains settings that are specific to the feature.

The configurations for all features can be obtained all at once via `GET
/feature-configs`

```json
{
   "sso": {
      "status": "enabled"
   },
   "searchVisibility": {
      "status": "disabled"
   },
   "appLock": {
      "status": "enabled",
      "config": {
         "enforceAppLock": "true",
         "inactivityTimeoutSecs": 300
      }
   }
   /* ... */
}
```

Note: The implemenation of the Feature Configuration API is re-using the types
from `Wire.API.Team.Feature` (Team Features API).

## Team Features API

The Team features API preceedes the notion of the feature configuration API. The
endpoints of the form `GET /teams/:tid/features/:feature-name` and `PUT
/teams/:tid/features/:feature-name` can be used to get and set the feature
configuration on a per-team level. Features that cannot be get and set on a
per-team level may be missing from the Team Features API. See the [Swagger
documentation](../../understand/api-client-perspective/swagger.md) on what
endpoints are available.
