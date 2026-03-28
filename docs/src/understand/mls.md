# Configuring MLS vs Proteus messaging protocol

## Overview

Wire has two messaging protocols: **[Proteus](https://github.com/wireapp/proteus)** (the original, based on the Signal Double Ratchet) and **[MLS](https://datatracker.ietf.org/wg/mls/documents/)** (Messaging Layer Security, [RFC 9420](https://www.rfc-editor.org/rfc/rfc9420.html)). MLS is newer, scales better for large groups, and uses standardized end-to-end encryption.

MLS configuration is spread across multiple services, and they all need to agree. If galley tells clients "use MLS" but brig doesn't have MLS enabled, clients won't be able to initialize and users will get a "Something went wrong" error on login.

## Settings that need to be consistent

There are four places where MLS config matters. All four need to agree with each other.

| Setting                                     | File                                                                   | What it does                                        |
|---------------------------------------------|------------------------------------------------------------------------|-----------------------------------------------------|
| `brig.config.optSettings.setEnableMLS`      | `values/wire-server/values.yaml`                                       | Makes brig accept MLS key package operations        |
| `galley.config.settings.featureFlags.mls`   | `values/wire-server/values.yaml`                                       | Advertises MLS support and default protocol          |
| `galley.secrets.mlsPrivateKeys.removal`     | `values/wire-server/secrets.yaml`                                      | Private keys for signing MLS removal proposals       |
| `FEATURE_ENABLE_MLS` (webapp/team-settings) | `values/webapp/values.yaml` and `values/team-settings/values.yaml`     | Turns on MLS in the web UI                          |

If you're not familiar with how helm values overrides work, read [Overriding helm configuration settings](helm.md#overriding-helm-configuration-settings) first.

## Enabling MLS

### 1. Generate MLS removal keys

Galley needs private keys to sign [External Remove Proposals](https://www.rfc-editor.org/rfc/rfc9420.html#section-12.1.4) (used when users leave conversations or delete clients). You need one key per supported ciphersuite:

```sh
openssl genpkey -algorithm ed25519
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-256
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-384
openssl genpkey -algorithm ec -pkeyopt ec_paramgen_curve:P-521
```

Put these in `values/wire-server/secrets.yaml`:

```yaml
galley:
  secrets:
    mlsPrivateKeys:
      removal:
        ed25519: |
          -----BEGIN PRIVATE KEY-----
          ...
          -----END PRIVATE KEY-----
        ecdsa_secp256r1_sha256: |
          -----BEGIN PRIVATE KEY-----
          ...
          -----END PRIVATE KEY-----
        ecdsa_secp384r1_sha384: |
          -----BEGIN PRIVATE KEY-----
          ...
          -----END PRIVATE KEY-----
        ecdsa_secp521r1_sha512: |
          -----BEGIN PRIVATE KEY-----
          ...
          -----END PRIVATE KEY-----
```

These are sensitive. Use Helm's secrets management rather than storing them in plaintext. See [crypto libraries and sources of randomness](crypto-libs.md) for more on Wire's cryptographic foundations.

### 2. Enable MLS in brig

In `values/wire-server/values.yaml`, under brig's config:

```yaml
brig:
  config:
    optSettings:
      setEnableMLS: true
```

Without this, clients that try to register MLS [key packages](https://www.rfc-editor.org/rfc/rfc9420.html#section-10) will get `400 Bad Request` with "MLS is not configured on this backend". This is the most commonly missed setting.

### 3. Configure the MLS feature flag in galley

In `values/wire-server/values.yaml`, under galley's [feature flags](team-feature-settings.md):

```yaml
galley:
  config:
    settings:
      featureFlags:
        mls:
          defaults:
            status: enabled
            config:
              protocolToggleUsers: []
              defaultProtocol: mls
              allowedCipherSuites: [2]
              defaultCipherSuite: 2
              supportedProtocols: [proteus, mls]
            lockStatus: unlocked
```

The fields that matter here:

- `defaultProtocol: mls` tells clients to use MLS for new conversations.
- `supportedProtocols: [proteus, mls]` keeps both protocols around (you need this for backwards compatibility with existing conversations).
- `allowedCipherSuites: [2]` is `MLS_128_DHKEMP256_AES128GCM_SHA256_P256`, the standard ciphersuite.

For related settings like MLS end-to-end identity and MLS migration timelines, see [Server and team feature settings](team-feature-settings.md#mls-end-to-end-identity).

### 4. Enable MLS in webapp and team-settings

In `values/webapp/values.yaml`:

```yaml
envVars:
  FEATURE_ENABLE_MLS: "true"
```

And in `values/team-settings/values.yaml`:

```yaml
envVars:
  FEATURE_ENABLE_MLS: "true"
```

### 5. Apply changes

Helm upgrade after editing the values files:

```sh
helm upgrade --install --wait wire-server ./charts/wire-server \
  --values ./values/wire-server/values.yaml \
  --values ./values/wire-server/secrets.yaml
```

Do the same separately for webapp and team-settings if you changed their values. If you're doing a full deployment from scratch, see [Installing wire-server using Helm](../how-to/install/helm-prod.md) for the complete process.

## Disabling MLS (Proteus only)

If you want a [Proteus](https://github.com/wireapp/proteus)-only deployment, you need to make sure galley doesn't advertise MLS as the default and brig doesn't have MLS turned on.

### 1. Disable MLS in brig

In `values/wire-server/values.yaml`:

```yaml
brig:
  config:
    optSettings:
      setEnableMLS: false
```

Or just remove the `setEnableMLS` line, it defaults to false.

### 2. Set default protocol to proteus in galley

```yaml
galley:
  config:
    settings:
      featureFlags:
        mls:
          defaults:
            status: enabled
            config:
              defaultProtocol: proteus
              supportedProtocols: [proteus, mls]
            lockStatus: unlocked
```

Having `status: enabled` and `supportedProtocols` including `mls` is fine here. It just means MLS is *available* if a team admin explicitly switches to it. What matters is that `defaultProtocol` is set to `proteus`, so clients don't try MLS by default.

### 3. Remove MLS from webapp and team-settings

Either remove `FEATURE_ENABLE_MLS` or set it to `"false"`.

### 4. Apply changes

Helm upgrade as above.

## Common misconfiguration

The most common problem: galley has `defaultProtocol: mls` but brig has `setEnableMLS: false` (or the setting is missing). Here's what happens:

1. User logs in via the webapp
2. Webapp asks galley for the team's feature config, sees `defaultProtocol: mls`
3. Webapp tries to register MLS key packages with brig
4. Brig rejects because MLS isn't enabled on its end
5. Webapp shows "Something went wrong", user can't use the app

The fix: if galley says `defaultProtocol: mls`, brig must have `setEnableMLS: true`. They need to match.

This exact mismatch exists in the `wire-server-deploy` bundle's `prod-values.example.yaml` as of version 5.23.0, where `defaultProtocol` is set to `mls` but `setEnableMLS` is `false`. If you based your deployment on that file, you'll hit this. For help debugging, see [Troubleshooting during installation](../how-to/install/troubleshooting.md).

## MLS removal keys

The removal keys in galley's secrets let the server remove clients from MLS groups. Without them, galley can't sign External Remove Proposals, so operations like leaving a conversation or deleting a client will fail for MLS groups.

If you generated these during initial setup ([wire-server-deploy](https://github.com/wireapp/wire-server-deploy)'s secret generation scripts do this automatically), you don't need to regenerate them when toggling MLS. They can stay in the secrets file even when MLS is disabled.

## Federation and MLS

If you're running a [federated deployment](federation/README.md), MLS is the recommended protocol. MLS groups can span federated backends, and the protocol handles cross-domain key distribution natively. Make sure all federated backends agree on the same `allowedCipherSuites` and `supportedProtocols` values, or group creation across backends will fail.

## Verifying the configuration

After applying changes, check that everything is consistent:

```sh
# Check brig
kubectl exec deploy/brig -- grep setEnableMLS /etc/wire/brig/conf/brig.yaml

# Check galley
kubectl get configmap galley -o yaml | grep -A5 'defaultProtocol'

# Check webapp
kubectl describe deploy/webapp | grep FEATURE_ENABLE_MLS
```

All three should agree on whether MLS is the default.
