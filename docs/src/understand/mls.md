(mls-message-layer-security)=

# Messaging Layer Security (MLS)

To enable support for [MLS](https://datatracker.ietf.org/wg/mls/documents/)
conversations you need to configure the `wire-server` helm chart with a removal
key. This key is used by the server to sign External Remove Proposals and
enables the server to remove clients from MLS groups, e.g. when users leave
conversations or delete their clients.

The removal key is configured at path
`galley.secrets.mlsPrivateKeys.removal.ed25519` in the wire-server helm chart.
For example:

```yaml
# values.yaml or secrets.yaml
galley:
  secrets:
    mlsPrivateKeys:
      removal:
        ed25519: |
          -----BEGIN PRIVATE KEY-----
          MC4CAQA....Z709c
          -----END PRIVATE KEY-----
```

The key is a private ED25519 key in PEM format. It can be created by openssl
with this command:

```sh
openssl req -nodes -newkey ed25519 -keyout ed25519.pem -out /dev/null -subj /
```

This will create a `ed25519.pem`. Use the contents of this file as the
configuration value.

This is a sensitive configuration value. Consider using Helm/Helmfile's support
for managing secrets instead of putting this value in plaintext in a
`values.yaml` file.

Next, MLS needs to be explictly enabled in brig. This can be configured at
`brig.config.optSettings.setEnableMLS`, for example:

```yaml
# values.yaml
brig:
  config:
    optSettings:
      setEnableMLS: true
```

Finally, the web applications need to be made aware of *MLS*. This is done by
setting the following environment variable for the web application:

```yaml
webapp:
  envVars:
    FEATURE_ENABLE_MLS: "true"
```

and for the team settings web application:

```yaml
# NOTE: Only relevant if you want team-settings
team-settings:
  envVars:
    FEATURE_ENABLE_MLS: "true"
```

As long as *MLS* is still an opt-in feature, please remember that in order to be able
to use the *MLS* protocol when creating conversation on clients, the team administrator
need to have opted in for the *MLS* feature in the team settings.
