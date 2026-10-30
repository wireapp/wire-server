# TOTP 03 — encryption of TOTP secrets at rest

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 01
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [specs §3 "Secret at rest", §8 (config)](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan)

Runs in parallel with `TOTP 02`; they do not touch the same files.

## Scope

`libs/wire-subsystems/src/Wire/AuthenticationSubsystem/TOTP/Crypto.hs` — a small, self-contained module that encrypts and decrypts TOTP secrets, plus the config type that carries the keys.

TOTP secrets must be recoverable (a hash is useless for verification), so the only defence against a database dump is encryption with a key held elsewhere.  The repo has no symmetric encryption-at-rest facility today; this ticket builds the first one, scoped narrowly to TOTP.

## Design

- **AES-256-GCM** via `crypton` (`Crypto.Cipher.AES.AES256`, `Crypto.Cipher.Types` AEAD). No new dependency.
- Stored blob layout: `nonce ‖ ciphertext ‖ tag` in `totp_credential.secret`, with the key identifier in `totp_credential.key_id`.
- **The user id is bound in as additional authenticated data (AAD)**, so a ciphertext cannot be transplanted from one user's row to another's.
- Nonces come from the `Random` effect, never a counter, and are never reused across encryptions under the same key.
- Hold key material and decrypted secrets in `memory`'s `ScrubbedBytes` rather than `ByteString` wherever the types allow: it zeroes on GC and its `Show` is already redacted; `crypton`'s APIs accept any `ByteArrayAccess`.
- Config carries a *list* of keys, each `{ keyId, key }` (base64), with one marked active. New writes use the active key; reads select by `key_id`. This makes rotation a config change plus an optional backfill, with no schema change.

```haskell
data TOTPSecretKey = TOTPSecretKey { keyId :: Text, key :: ByteString }
data TOTPCryptoConfig = TOTPCryptoConfig
  { activeKey :: TOTPSecretKey
  , retiredKeys :: [TOTPSecretKey]
  }

encryptSecret :: Member Random r => TOTPCryptoConfig -> UserId -> TOTPSecret -> Sem r (Text, ByteString)
decryptSecret :: TOTPCryptoConfig -> UserId -> Text -> ByteString -> Either TOTPCryptoError TOTPSecret
```

Precedent for secrets in brig's config: zauth private keys, the TURN secret.

## Acceptance criteria

- Round trip: `decryptSecret cfg uid =<< encryptSecret cfg uid s` returns `s`.
- A ciphertext produced for user A **fails to decrypt** for user B (the AAD binding) — and fails loudly, not as a silently different secret.
- A ciphertext produced under key `k1` still decrypts after `k1` is moved from `activeKey` to `retiredKeys` and `k2` becomes active.
- Decryption under an unknown `key_id` is a distinguishable, logged error.
- A tampered byte anywhere in the blob fails the AES galois/counter mode tag check.
- Two encryptions of the same secret produce different ciphertexts (nonce freshness).
- Key material is validated at parse time: wrong length → a clear config error at startup, not a runtime crash on first enrolment.
- The config type refuses to produce a `TOTPCryptoConfig` with duplicate `keyId`s.

## Notes

- Brig must **fail at startup** if TOTP is enabled and no key is configured.  Wiring that check belongs to `TOTP 06`; this ticket provides the types that make the check expressible.
- Do not add a "plaintext fallback" mode. An optional-encryption flag becomes the default-off setting that someone ships to production.
- Recovery codes are **not** encrypted — they are hashed with argon2id via `Wire.HashPassword`. Different threat model, different tool. See `TOTP 13`.
- Key rotation *backfill* (re-encrypting existing rows under the new active key) is deliberately out of scope; `key_id` makes it possible later without a migration. Say so in the config documentation so operators do not assume rotation is automatic.
