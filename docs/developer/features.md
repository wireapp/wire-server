## Features

Wire has multiple features (or feature flags) that affect the behaviour of
backend and clients, e.g. `sso`, `searchVisibility`, `digitalSignatures`,
`appLock`, `legalhold`, `fileSharing`.

Features cannot by configured by the users themselves, which distinguishes
features from user properties. Who can configure a feature depends on the
feature itself, e.g:

- Team administrators via Wire Team Settings
- Customer support via the backoffice tool
- Site administrators by changing a configuration file on the server

Depending on the feature the configuration is defined for all users on the
server (per-instance), per-team, or per-user.

## Feature configurations API

The Feature configurations API uniformly exposes all features with their
configuration per user:

`GET /feature-configs/:feature-name`

```javascript
{
  "status": "enabled" /* or "disabled" */
  "config": {
    /* this object depends on the feature */
  }
}
```
where the optional `config` field contains settings that are specific to the feature.

The configurations for all features are exposed via `GET /feature-configs`

```javascript
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

Note: The implemenation of these endpoints is re-using the types from
`Wire.API.Team.Feature` (Team Features API).

## Team Features API

The Team features API preceedes the notion of the feature configuration API. The
endpoints of the form `GET /teams/:tid/features/:feature-name` are deprecated in
favor of `GET /feature-config/:feature-name`. Features that are defined per-team
are be configured via `PUT /teams/:tid/features/:feature-name` endpoint. The
motivation for introducing the Feature configurations API was to generalize team
features to features that may be defined per-instance or per-user, which
wouldn't fit the notion of team feature.
