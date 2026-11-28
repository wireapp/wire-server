# TOTP 01 — wire types and schemas

```
assignedTo: Matthias Fischmann
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [specs §2 (types), §0 (QR / `otpauth://`), §7 (testing rules)](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan)

## Scope

Introduce `libs/wire-api/src/Wire/API/User/Auth/TOTP.hs` with every type the TOTP feature exchanges over the wire, fully schema'd and documented. No behaviour, no storage, no endpoints — this ticket produces types and their serialisation only.

Types per specs §2, plus the error constructors in `Wire.API.Error.Brig` (`BrigError` + `MapError` instances) from specs §4. Having them early means later tickets can `CanThrow` without touching this module.

## Requirements

- All serialisation via `Data.Schema` (`deriving (FromJSON, ToJSON, S.ToSchema) via Schema X`), with `description` and `example` modifiers on **every field**. The OpenAPI output is a client-facing contract and this is where its quality is decided.
- `TOTPSecret` JSON encoding is unpadded Base32 (RFC 4648) via `Data.ByteArray.Encoding.convertToBase Base32` from `memory`. Decoding rejects padding, lowercase-only input should be accepted case-insensitively (authenticator apps are inconsistent) — decide, document, test.
- `TOTPCode` validates digit-only and length 6–9 at parse time. It cannot validate against the *configured* digit count — wire-api has no access to brig's config — so the exact-length check belongs to the verifier (`TOTP 09`).
- `SecondFactorCode = SFTotp TOTPCode | SFRecovery RecoveryCode` (specs §2) parses either shape at the JSON boundary; this is the one place the "the two shapes are disjoint" invariant lives, so test it here: digits-only parses as `SFTotp`, the recovery-code format as `SFRecovery`, and nothing parses as both.
- **Secrets must be hard to leak.** `TOTPSecret` and `RecoveryCode`:
  - constructors are **not** exported — the type is abstract outside this module, so no code elsewhere can pattern-match and print the contents;
  - hand-written redacting `Show` (cf. `Data.Secret.SecretText` in `libs/types-common`);
  - no `ToByteString`/`ToHttpApiData`/`Cql` instance that round-trips the raw value unless a specific caller needs it, and if one does, it is named and justified in a comment.
  - Provide a single, obviously-named escape hatch (`unsafeRevealTOTPSecret`) used only at the Base32-encoding and crypto boundaries.
- `Arbitrary` instances for all of the above.

## Acceptance criteria

- `cabal build wire-api` is clean.
- Every new type appears in `libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs`.
- `TOTPCredentialId` appears in the `HttpApiData` roundtrip test.
- Golden JSON in `libs/wire-api/test/golden/` for `TOTPEnrolment`, `TOTPCredentialInfo`, `TOTPRecoveryCodes`, `ReauthBody`.
- Negative parse tests: malformed Base32, non-digit code, wrong code length, padded Base32.
- A test asserting `show` of a `TOTPSecret` and a `RecoveryCode` contains no part of the secret.

## Notes

- `TOTPHashAlgorithm` covers `sha1`/`sha256`/`sha512` — the set RFC 6238 defines and `crypton` accepts (`mkTOTPParams` rejects digests under 20 bytes). It is site-configurable (`TOTP 06`), appears in the `otpauth://` URI, and is stored per credential, so all three constructors are reachable from day one. Its JSON/`FromHttpApiData` forms are the lowercase strings used in config; the `otpauth://` URI spells them uppercase (`SHA1`), which is a separate rendering, not the JSON encoding.
- `TOTPCredentialInfo` has **no** `lastUsedAt` field — we do not track last use at all (specs D13). Do not add one "because it is easy"; adding it is a product decision with a privacy dimension, not a schema detail.  Leave a comment to that end in the code.
- `NewTOTPCredential.clientTime` and `ActivateTOTPCredential.clientTime` exist for clock-skew detection (`TOTP 10`). Define them now so the request bodies do not change shape later; the server may ignore them until TOTP 10 lands.
