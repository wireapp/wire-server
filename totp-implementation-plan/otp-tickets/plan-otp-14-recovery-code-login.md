# TOTP 14 — authenticating with a recovery code

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 11
- TOTP 13
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4 (verification algorithm, rate limiting).

## Scope

Make a recovery code work anywhere a TOTP code is expected — at login and at every operation routed through `EnsureReauthenticated`. The user who lost their phone can get back in without an operator.

### Implementation

Extend `VerifySecondFactor` in `Wire/AuthenticationSubsystem/TOTP/Interpreter.hs` with a fallback branch:

1. Try every activated credential (the existing `TOTP 09` path).
2. **Only if that fails**, and only if the input parsed as `SFRecovery` ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §2 — the shape decision is the parser's, made once at the JSON boundary, not a runtime sniff here), `GetRecoveryCodes`, drop the ones with a `used_at` (the store returns all of them — [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D12), and hash-match against the rest.
3. On a match, `MarkRecoveryCodeUsed` — single use, atomically — and return `MatchedRecoveryCode`, which is what lets callers notify on recovery-code use (`TOTP 17`) without this module knowing about email.

The same `totp_code` field carries either kind of code; clients do not need to know which they are sending. Normalisation per `TOTP 13` happens in the parser.

### The ordering is a cost decision, not a style choice

Verifying a recovery code is an argon2id check: ~100 ms and memory-hard, the same cost as a password verification. With ten unused codes, a naive implementation performs ten of them per attempt — a full second of memory-hard work per request, which an attacker can trigger with any six wrong digits. So:

- The recovery branch runs **only after** the TOTP branch has failed.
- It runs only when the input is shaped like a recovery code, not like a six-digit TOTP. This is a cheap pre-filter that discards almost all brute-force traffic before any hashing.
- Rate limiting applies (charged on failure, per `TOTP 09`), and the recovery branch is the reason that matters here.
- Consider short-circuiting on first match rather than hashing all ten.

The dispatch is total over `SecondFactorCode`, and the disjointness of the two shapes (digits-only vs. Crockford-with-dashes) is a parser invariant with its own tests (`TOTP 01`). Keep the test that ties the recovery-code format (`TOTP 13`) to that invariant, so a future alphabet change cannot silently make a recovery code parse as a TOTP code.

## Acceptance criteria

Unit tests:

- A valid recovery code authenticates where a TOTP code is expected.
- The same recovery code a second time → failure (single use).
- A recovery code belonging to another user → failure.
- Consuming one code leaves the other nine usable.
- A six-digit input never reaches the argon2id path — assert via a mock `HashPassword` that records calls, so this stays true under refactoring.
- Rate limiting is charged on a failed recovery-code attempt.
- A recovery code works when the user has **no** working authenticator (the actual use case — the TOTP branch finds nothing to match).

Integration tests:

- Lost-phone flow end to end: enrol, activate, collect codes, then log in using a recovery code with no access to the authenticator.
- The consumed code fails on the next login.
- After recovering, the user can enrol a new credential.

## Notes

- Decide and document whether using a recovery code should **force** enrolment of a new authenticator before the session is fully usable, or merely log the user in. Recommended for milestone 1.1: log in and leave a clear signal; forcing re-enrolment is a UX flow that belongs with `TOTP 17`.
- Notification on recovery-code use is a classic account-takeover signal and is cheap to add here if `TOTP 12` already wired the email path; otherwise it is `TOTP 17`.
- `changelog.d/` entry.
