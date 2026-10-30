# TOTP 27 — make secret containment real

```
assignedTo: Matthias Fischmann
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §2 (the "do not derive `Show` naively" rule), §4 (constant-time comparison). A copy lives at `plan-otp.md`.

## Why this one exists

`TOTP 00` (`plan-mock-api.md`) introduced `TOTPSecret`, `TOTPCode` and `RecoveryCode` with unexported constructors and a redacting `Show`, and then described the result as if the plaintext were contained. It is not, and the module said so more confidently than the code earned. The header has since been corrected to describe the actual guarantee; this ticket is about closing the gap rather than documenting it.

**What the current design actually guarantees:** you cannot get the plaintext out with `show`. That is worth having — it rules out the common accident, where a secret reaches a log line, a trace, an exception message or a test failure through a *derived* `Show` on some enclosing record, which is how this goes wrong in practice.

**What it does not guarantee:** that the plaintext is hard to reach. It is one function call away by several routes, listed below.

This is not a "the mock is insecure" ticket — the mock reads and writes nothing. It is a "the abstraction will be load-bearing once real secrets flow through it, and right now it is weaker than it reads" ticket. The right time to fix it is before `TOTP 02`/`TOTP 03` put real secrets behind these types and twenty call sites appear.

## The leak paths, concretely

These are the ones found by audit, not a speculative list.

| # | path | needed by | severity |
|---|---|---|---|
| 1 | `toJSON` / `encode` on `TOTPSecret`, `TOTPCode`, `RecoveryCode`, `SecondFactorCode`, and transitively on `TOTPEnrolment`, `ReauthBody`, `TOTPRecoveryCodes` | the wire format itself | structural |
| 2 | `show . toJSON` — a two-step leak that the redacting `Show` does nothing about | — | the real hole |
| 3 | `totpSecretToBase32 :: TOTPSecret -> Text`, exported, not named `unsafe` anything | brig's `otpauth://` builder | real |
| 4 | `unsafeRevealTOTPCode`, `unsafeRevealRecoveryCode` | the `ToSchema` instances; honestly named at least | expected |
| 5 | `Test.Wire.API.Roundtrip.Aeson.testRoundTrip` prints `counterexample (show (toJSON v))` | the test helper, shared by ~200 types | **a secret in CI logs on any roundtrip failure** |
| 6 | derived `Eq`/`Ord` on the code types are not constant-time | `Ord` is used for nothing; `Eq` for tests | see §4 of the spec |
| 7 | whatever brig's request/response logging and OTel tracing middleware captures on `POST /self/totp` | — | unaudited |

(1) is not a bug: serialising is what a wire type is for. (2) is the one that makes the stated guarantee hollow, because the redacting `Show` is advertised as the thing that stops secrets reaching logs, and the most natural way to log a request body goes straight past it.

(5) is the most concrete and the cheapest to hit: it needs no malice and no unusual code, only a roundtrip property failing once in CI.

## Scope

Decide and implement, roughly in this order of value:

1. **Audit (7) first, and separately.** It is the only item that can be leaking *today* on a staging backend with development versions enabled, and it needs no design work — just reading `Brig.Run`'s middleware stack and the OTel instrumentation to find out whether request or response bodies are captured, and on which routes. `plan-otp-07` already carries "confirm no logging or tracing middleware captures response bodies on this route" as a handler note; that confirmation belongs here, done once, for the whole surface. If something does capture bodies, that is a finding to act on immediately and not to bundle with the rest of this ticket.

2. **Fix (5).** Either make `testRoundTrip`'s counterexample opt-out per type, or give these types a `ToJSON` that is only reachable through the schema machinery and not through a bare `toJSON` in a test helper. The former is a two-line change to a shared helper and fixes it for every secret-bearing type anyone adds later; prefer it unless it turns out to be noisy.

3. **Decide what to do about (2).** The honest options, in increasing cost:
   - *Document and move on.* Keep the guard rail, stop implying it is a boundary. (This is the status quo after the header correction. It is a legitimate answer; say so explicitly rather than by default.)
   - *Separate the two renderings.* Keep `ToJSON` for the wire and make the "give me the plaintext" direction a distinctly-named, greppable function, so a reviewer can audit call sites of a handful of names instead of reasoning about where `toJSON` output ends up.
   - *A `Secret a` wrapper in `types-common`* with no `ToJSON`, next to the existing `Data.Secret.SecretText`, so the type system distinguishes "this value may be serialised to a client" from "this value may be inspected". Most robust; also the most disruptive, and it would want to cover `PlainTextPassword6` too, which is a bigger conversation than TOTP.

4. **(6): constant-time comparison.** Spec §4 requires it for code comparison. Note the comparison that matters happens in the verifier (`TOTP 09`), against a freshly computed code, not via `Eq` on these types — so this may be nothing more than "do not add an `Eq`-based shortcut later, and drop the `Ord` instances if nothing needs them". Check whether `Ord` is used at all before keeping it.

5. **(3) and (4): naming.** If (3) stays, it should at minimum be consistent with (4) — either both are `unsafeReveal*` or neither is, and the wire encoding should not be the same function as the plaintext accessor.

**Explicitly out of scope:** encryption of the secret at rest (`TOTP 03`), and hashing of recovery codes (`TOTP 13`). Those protect against a database read; this ticket is about the process's own memory, logs and traces.

## Notes

- `unsafeRevealTOTPSecret` was removed in `TOTP 00` because nothing called it — the crypto that will need the raw bytes does not exist yet. `TOTP 03` will add it back; when it does, it should be the *only* way to the bytes, and this ticket's naming decision applies to it.
- `crockfordAlphabet` and `secondFactorCodeToText` are likewise unexported for now, for the same reason: an export with no caller is a leak path held open for nobody.
- Do not "fix" this by removing the redacting `Show`. It is doing real work; the problem is only that it was described as doing more.
- The test asserting that `show` leaks nothing (`Test.Wire.API.User.Auth.TOTP`, group "secrets do not leak through Show") is correctly scoped and should stay as-is. If this ticket produces a stronger guarantee, add a test for *that* rather than widening the existing one, so the two claims stay distinguishable.
