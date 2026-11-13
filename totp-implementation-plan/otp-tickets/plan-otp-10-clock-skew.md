# TOTP 10 — graceful clock skew handling

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 07
- TOTP 09
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4 "Clock skew handling".

## Why

TOTP fails silently and confusingly when a clock is wrong. The user sees "invalid code" over and over while their authenticator shows a code they are copying correctly, and nothing in the system tells them the real problem. This is one of the top support categories for every TOTP deployment. Two mechanisms, both cheap, make it diagnosable and then make it not happen.

Layer (a) — the symmetric verification window, `skew` steps wide in total, default 3 — already exists from `TOTP 09`. This ticket adds (b) and (c).

## Scope

### (b) Enrolment-time clock check

`NewTOTPCredential` and `ActivateTOTPCredential` already carry `clientTime` (defined in `TOTP 01`, threaded through in `TOTP 05`, ignored until now). Give it meaning:

- The server compares `clientTime` against `Now.get`.
- If `|Δ| > maxClientClockSkew` (config, default 30 s) → `409 totp-client-clock-skew`.
- The error body carries `server_time`, so the client can tell the user "your device clock is 4 minutes fast — fix it and try again" instead of letting them discover the problem later as a mystery login failure. Model it as a `RichError` (cf. `verificationCodeSubsystemErrorToHttpError` in `Wire/VerificationCodeSubsystem.hs`).
- `TOTPEnrolment.serverTime` is already returned, so a client can self-diagnose before even submitting.

**Document the limit honestly** ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4(b)): the clock that actually matters belongs to the device running the authenticator app, not necessarily the one calling this API; this check is a diagnostic for the common same-device case, (c) is the guarantee. Mind the two knobs' default interplay, also §4(b): widening `maxLearnedStepOffset` without widening `maxClientClockSkew` makes (b) reject enrolments (c) was configured to absorb.

### (c) Learned per-credential step offset

The code submitted at activation identifies exactly which time step the authenticator is on.

- During activation, evaluate the submitted code against steps `n-m … n+m`, where `m = maxLearnedStepOffset` (config, default 2 — i.e. ±60 s). This is **deliberately wider than the verification window** (`skew = 3`, i.e. ±1 step): the generous range is used exactly once, on an already-authenticated request, to pin the offset down; the tight window then governs every later login. Do not conflate the two config values.
- If it matches step `n+k` with `k ≠ 0`, store `step_offset = k` on the credential (the column exists from `TOTP 02`).
- Every later verification applies the offset: the corrected current step is `now / period + step_offset`, and the window is taken around that.
- If no step in `[-m, m]` matches, activation fails — with `totp-client-clock-skew` rather than a bare `totp-invalid-code`, since a wildly-off clock is the overwhelmingly likely cause and a different message leads the user to the fix. This replaces `TOTP 05`'s `TOTPInvalidCode` on activation (update that ticket's test), and it knowingly conflates a mistyped code with a skewed clock — the server cannot tell them apart, so the error description and the docs page must have clients present both possibilities ("wrong code, or the device clock is off — server time is …").

A phone that is persistently 45 seconds fast then keeps working indefinitely, **without widening the window for anyone else** — the window stays `skew` steps wide around the *corrected* time ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4(c) has the full rationale).

Bound `|k| ≤ maxLearnedStepOffset` and store it as `smallint`.

### Consistency with replay protection

The claimed step (`totp_step_claim.step`) is recorded in the credential's own (offset-corrected) step numbering. If `TOTP 09` stored uncorrected steps, fix it here. A test with a non-zero offset credential is mandatory, because this is precisely the bug that no existing test would catch.

The claim's `expires_at` is wall-clock and its derivation is offset-independent (`TOTP 09`, "The claim expires"), so there is nothing to adjust there — but check it rather than assume it, since an offset credential is exactly where a mixed-units bug would hide.

## Acceptance criteria

Unit tests with a settable `Now`:

- `clientTime` within tolerance → enrolment and activation succeed.
- `clientTime` off by more than `maxClientClockSkew`, in both directions → `totp-client-clock-skew`, and the error body contains the server time.
- Activation with a code from step `n+1` → credential activated with `step_offset = 1`; from step `n-1` → `step_offset = -1`. Both signs, or the sign convention gets inverted and only one half of the users notice.
- Activation with a code from step `n+2` / `n-2` → `step_offset = ±2` at the default `maxLearnedStepOffset`.
- Activation with a code from step `n+5` → `totp-client-clock-skew`, credential remains pending.
- A credential with `step_offset = 1` verifies codes generated by an authenticator that is one step fast, and **rejects** codes from an on-time authenticator outside the corrected window.
- **Replay protection still holds for an offset credential**: the same skewed code twice → `TOTPCodeReused`.
- Offset is per credential: a user with one skewed and one on-time authenticator can use both.

Integration tests, using `codeAtStepOffset` from `TOTP 04`:

- Enrol and activate with a deliberately skewed clock; then log in with codes from the same skewed clock and succeed.
- Enrol with a wildly wrong `clientTime` and get a usable error message.

## Notes

- Consider logging observed offsets (bucketed, not per user) as a metric. A rise in enrolments with non-zero offsets is a signal worth having, and it costs almost nothing.
- Do **not** re-learn the offset on every successful verification. Drift correction over time sounds helpful and quietly turns into an accept-anything window; an authenticator whose clock moves that much should re-enrol.
