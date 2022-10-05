### Messaging Layer Security (MLS)

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

