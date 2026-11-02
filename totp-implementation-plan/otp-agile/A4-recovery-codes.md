# A4 — recovery codes

> **Disclaimer in force:** replay, clock skew and rate limiting are still absent, and secrets are still stored unencrypted. This is the last step before the hardening bucket.

**Demo:** enrol, write down the ten codes the client shows you, delete the account from your authenticator app, then log in with one of the codes. Then try the same code again and watch it fail.

## Scope

- **Generation.** Ten codes at activation, `xxxx-xxxx-xxxx-xxxx` in Crockford Base32 — 16 symbols, 80 bits. The alphabet excludes I, L, O and U precisely so that a human transcribing from paper cannot produce an ambiguous character. `plan-otp-13`.
- **Storage.** The `totp_recovery_code` table and its migration. Codes are **hashed** with argon2id via `Wire.HashPassword` — password-equivalent, never encrypted-reversible. This is a different treatment from the TOTP secret (which must be recoverable in plaintext to compute codes) and the difference is deliberate: §3.
- **Consumption.** `MarkRecoveryCodeUsed` as a single conditional `UPDATE … WHERE used_at IS NULL` returning the affected row count — a compare-and-set, not a read-then-write. Two concurrent logins presenting the same code is a race, and losing it means the code works twice. One of the two deliberate exceptions to the dumb-store rule (D12); the other arrives in A5.
- **Login and reauth accept them.** No new field: `SecondFactorCode` already parses either shape, which is why it was defined as a sum type at the JSON boundary in A0 rather than as a `Text` to be sniffed later. `plan-otp-14`.
- **Ordering.** The recovery-code branch is reached **only after** the TOTP branch has failed. Verifying a recovery code is an argon2id hash check — the same ~100 ms memory-hard cost as a password, two to three orders of magnitude more than a TOTP comparison. Getting this backwards turns every login into a password hash. §4.
- **Config:** `recoveryCodeCount`, default 10.
- **The client contract**, which is as much of this step as the backend work: codes are shown **once**, at activation, and never again. The client must force an acknowledgement before dismissing them. `GET /self/totp` will never return them. Document it on the route, not only in a ticket.

Still fake: everything in A5.

## What this step does not do

**No regeneration, no "how many do I have left", no notification when one is used.** Those are `plan-otp-17`, and they belong in A7 with the rest of the management surface. A user in A4 who burns all ten codes is in the same position as a user who has lost everything: they need an operator. That is acceptable for a staging feature and unacceptable for a shipped one, which is why `plan-otp-17` is a release blocker for general availability even though it is not one for A5.

## The thing to get right

**Burning a code is not idempotent and the user cannot see it happen.** A recovery code consumed by a request that then fails for an unrelated reason is gone, and the user has nine left and no idea why. So: consume the code as the *last* thing that can fail in the login path, after the password, after the TOTP attempt, and never speculatively. §4's ordering rule says the same thing from the other direction.

## Done when

- The demo works, including the second attempt with the same code failing.
- Integration tests: activation returns exactly `recoveryCodeCount` codes, all distinct, all matching the documented format; login with a valid code succeeds; the same code a second time fails; a used code does not count toward the remaining set; `GET /self/totp` never returns codes; enrolling a second credential does not invalidate the first's codes (decide and document if you think it should — but decide).
- A concurrency test for `MarkRecoveryCodeUsed`: two simultaneous requests with the same code, exactly one succeeds. Against the real Postgres interpreter, not only the mock — this is an atomicity claim and the mock cannot prove it.
- The in-memory mock store reproduces the compare-and-set faithfully, or the unit tests pass against a mock that does not have the bug the real store might.
- **Honesty test:** `testA3NoRecoveryPathExists` is deleted.
- Route `Description`s updated. `changelog.d/` entry.
