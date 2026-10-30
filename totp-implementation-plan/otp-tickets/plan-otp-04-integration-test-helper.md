# TOTP 04 — client-side TOTP helper for the integration suite

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 01
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §7.

Runs in parallel with 02/03/05. Start it early: every later integration test depends on it, and it is small.

## Scope

The integration suite has to behave like an authenticator app: parse an `otpauth://` URI, hold a secret, and produce a valid code for an arbitrary point in time. Without this, none of the downstream tickets can write a meaningful integration test.

- Add `crypton` and `memory` to `integration/integration.cabal`.
- New module `integration/test/Test/TOTP/Util.hs`:

```haskell
data AuthenticatorApp = AuthenticatorApp
  { secret    :: ByteString
  , digits    :: Int
  , period    :: Int
  , algorithm :: Text
  }

parseOtpauthUri :: String -> Either String AuthenticatorApp
codeAt          :: AuthenticatorApp -> UTCTime -> String   -- zero-padded
codeNow         :: AuthenticatorApp -> App String
codeAtStepOffset:: AuthenticatorApp -> Int -> App String   -- n steps from now
```

- Endpoint helpers in `integration/test/API/Brig.hs` and `integration/test/API/BrigInternal.hs` for the routes defined in `TOTP 07`, `TOTP 12` and `TOTP 15`. These can be added incrementally as those tickets land, but the module and its conventions are established here.

## Acceptance criteria

- `parseOtpauthUri` handles the exact format the server emits (§0 of [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan)), including percent-encoded account names with `@` and issuer-prefixed labels, and rejects malformed input rather than silently producing a wrong secret.
- `codeAt` reproduces the **RFC 6238 Appendix B** vectors. This is the one test that proves the helper is a real authenticator and not a mirror of our own server bug: if the helper and the server share a mistake, every downstream integration test passes vacuously.
- `codeAtStepOffset` lets a test produce a code for step `n±k`, which `TOTP 09` (skew window) and `TOTP 10` (learned offset) both need.
- Codes are zero-padded to the configured digit count — a code of `012345` must not be sent as `12345`.

## Notes

- Keep the helper independent of `wire-api`'s `TOTPSecret`: the test suite should be able to construct a *wrong* secret or a *badly timed* code deliberately. Reusing the production type's validation would make negative tests awkward.
- The integration suite addresses brig directly for most tests. Once `TOTP 08` ships the nginz routes, add at least one test that goes through nginz to prove the route entries are correct — a chart typo is otherwise invisible to CI.
