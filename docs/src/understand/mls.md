(mls-message-layer-security)=

# Messaging Layer Security (MLS)

To enable support for [MLS](https://datatracker.ietf.org/wg/mls/documents/)
conversations you need to configure the `wire-server` helm chart with a removal
key. This key is used by the server to sign External Remove Proposals and
enables the server to remove clients from MLS groups, e.g. when users leave
conversations or delete their clients.

The removal key is configured at path
`galley.secrets.mlsPrivateKeys.removal` in the wire-server helm chart.
You need to provide a variant for each supported ciphersuite:
- `ed25519`
- `ecdsa_secp256r1_sha256`
- `ecdsa_secp384r1_sha384`
- `ecdsa_secp521r1_sha512`


For example:

```yaml
# values.yaml or secrets.yaml
galley:
  secrets:
    mlsPrivateKeys:
      removal:
        ed25519: |
          -----BEGIN PRIVATE KEY-----
          ...
        ecdsa_secp256r1_sha256: |
          -----BEGIN PRIVATE KEY-----
          ...
        ecdsa_secp384r1_sha384: |
          -----BEGIN PRIVATE KEY-----
          ...
        ecdsa_secp521r1_sha512: |
          -----BEGIN PRIVATE KEY-----
          ...
```

These private keys can be created with with these commands:

```sh
openssl genpkey -algorithm ed25519
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-256
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-384
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-521
```

This is a sensitive configuration value. Consider using Helm/Helmfile's support
for managing secrets instead of putting this value in plaintext in a
`values.yaml` file.

In addition to removal keys, MLS needs to be explictly enabled in brig. This can be configured at
`brig.config.optSettings.setEnableMLS`, for example:

```yaml
# values.yaml
brig:
  config:
    optSettings:
      setEnableMLS: true
```

Finally, the webapp needs to enable made aware of *MLS*. This is done by
setting the following environment variable for the web application:

```yaml
envVars:
  FEATURE_ENABLE_MLS: "true"
```

and for the team settings web application:

```yaml
envVars:
  FEATURE_ENABLE_MLS: "true"
```

As long as *MLS* is still an opt-in feature, please remember that in order to be able
to use the *MLS* protocol when creating conversation on clients, the team administrator
need to have opted in for the *MLS* feature in the team settings.
