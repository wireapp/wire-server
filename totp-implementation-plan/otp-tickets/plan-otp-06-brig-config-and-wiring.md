# TOTP 06 — brig config and effect-stack wiring

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 05
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §1 (effect-stack position), §8 (config).

## Scope

Make the TOTP subsystem runnable inside brig. No endpoints yet — this ticket is pure plumbing, and it is separated from `TOTP 07` precisely because effect-stack changes and route changes fail in different ways and are easier to review apart.

### Config

`services/brig/src/Brig/Options.hs`:

```haskell
data TOTPOpts = TOTPOpts
  { enabled              :: Bool
  , secretEncryptionKeys :: NonEmpty TOTPSecretKeyConfig  -- { keyId, key (base64), active :: Bool }
  , issuer               :: Maybe Text      -- default: the backend domain
  , algorithm            :: TOTPHashAlgorithm -- sha1 (default) | sha256 | sha512
  , period               :: Word16          -- default 30
  , digits               :: Word8           -- default 6
  , skew                 :: Word8           -- TOTAL window in steps, odd, default 3
  , maxLearnedStepOffset :: Word8           -- default 2, used by TOTP 10
  , maxClientClockSkew   :: NominalDiffTime -- default 30s, used by TOTP 10
  , maxCredentialsPerUser:: Word8           -- default 5
  , recoveryCodeCount    :: Word8           -- default 10, used by TOTP 13
  , pendingEnrolmentTTL  :: Timeout         -- default 15 min
  }
```

Added as `totp :: Maybe TOTPOpts` under `settings`. Absent ⇒ feature off ⇒ the endpoints added in `TOTP 07` return `TOTPDisabled`.

**Startup validation, not runtime surprise:** if `enabled` is true, brig must fail to start when

- the key list is empty, no key is marked active, more than one is, a key is not 32 bytes after base64-decoding, or two keys share a `keyId`;
- `algorithm` is not one of `sha1`/`sha256`/`sha512` (the set RFC 6238 defines, and the set `crypton`'s `mkTOTPParams` accepts — it rejects any digest shorter than 20 bytes);
- `skew` is even or zero. It is the **total** window size and the window is symmetric around the current step, so only `1, 3, 5, 7, 9` are expressible ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4(a) has the mapping to `crypton`'s `ClockSkew` radius). Reject `4` with a message that says which values are allowed and why; do **not** round it to 3 or 5 behind the admin's back. Convert the valid value to the radius / `ClockSkew` *here*, once, and carry that in `TOTPSubsystemConfig` — `TOTP 09` must not re-derive it;
- `maxLearnedStepOffset` is smaller than the window radius `(skew - 1) / 2` — activation would then be *stricter* than verification, which is never intended;
- `digits` is outside 6–9 (`OTPDigits` goes down to 4, but a 4- or 5-digit second factor is not something we should let a site configure), or `period` is 0 or > 300 (`mkTOTPParams` enforces the upper bound itself).

A misconfigured backend must not come up and then fail on the first user's enrolment.

### Chart and test config

- `charts/brig/values.yaml` + `charts/brig/templates/configmap.yaml`, with the encryption key sourced from a Kubernetes secret (follow the zauth private key pattern — the key must not sit in a ConfigMap).
- `charts/brig/templates/secret.yaml` as appropriate.
- `hack/helm_vars/wire-server/values.yaml.gotmpl` and `services/brig/brig.integration.yaml`, so CI runs **with TOTP enabled** from this ticket onward.

### Effect-stack wiring

`services/brig/src/Brig/CanonicalInterpreter.hs`:

- `TOTP` goes into `NonRecursiveEffects2` / `BrigLowerLevelEffects` — **below** `AuthenticationSubsystem`, which will depend on it from `TOTP 11` onward. Placing it in `RecursiveEffects` works today and breaks confusingly later.
- `TOTPStore` goes next to `interpretUserGroupStoreToPostgres`, using the existing `hasqlPool` from `Brig.App`.
- `Input TOTPSubsystemConfig` is derived from `TOTPOpts` once at startup, not rebuilt per request.
- Mirror the same wiring in `libs/wire-subsystems/test/unit/Wire/MiniBackend.hs` so unit tests can run the full stack.

## Acceptance criteria

- `cabal build brig` clean; brig starts with TOTP enabled and with it absent.
- A deliberately broken config (no active key / wrong key length / duplicate `keyId` / `skew: 4` / `algorithm: md5` / `period: 0`) produces a clear startup error naming the offending field. Test this — it is the kind of validation that is written and then never exercised.
- `skew: 3` maps to `ClockSkew = OneStep`, `skew: 5` to `TwoSteps`; assert the mapping directly, since an off-by-one here silently halves or doubles every code's lifetime and nothing else would notice.
- The integration environment comes up with TOTP enabled and the existing integration suite stays green (nothing is exposed yet, so this should be a no-op; if it is not, something is wrong).
- `changelog.d/` entry for the new config options, including an explicit note that `secretEncryptionKeys` is required when `enabled` is true and that losing the key means losing every enrolled second factor.

## Notes

- Document the operational consequence prominently: **the encryption key is not recoverable from the database.** Losing it locks every TOTP user out, and the only remedy is the admin reset from `TOTP 15`. This belongs in the chart values comment, not only in `docs/`.
- `issuer` appears in users' authenticator apps. Changing it later does not update already-enrolled credentials, so pick the default deliberately and say so in the config comment.
- The `charts/brig/values.yaml` comment on `algorithm` must carry the two warnings from [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §8: **most authenticator apps ignore the algorithm (and `digits`, and `period`) in the `otpauth://` URI and assume SHA1/6/30**, so a non-default value produces credentials that enrol happily and then never verify — a failure that looks exactly like clock skew; and the setting applies to new enrolments only, since each credential stores its own parameters ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §3). A site admin reading only values.yaml must still find this out.
