# TOTP 09 — code verification, skew window and replay protection

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 05
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4 (verification algorithm, rate limiting).

Can run in parallel with `TOTP 07`/`TOTP 08`.

## Scope

Implement `VerifySecondFactor :: UserId -> SecondFactorCode -> TOTP m SecondFactorResult` in `Wire/AuthenticationSubsystem/TOTP/Interpreter.hs` ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4 — the result says *what* matched, which `TOTP 14` and `TOTP 17` need; until 14 lands, `SFRecovery` input is simply `NoMatch`). No endpoints — this is the primitive that `TOTP 11` consumes.

### Algorithm

1. `now <- Now.get`; `GetCredentials` and keep the activated ones. The store returns every row — pending/activated filtering is **this** ticket's job, not the store's ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D12).
2. For each activated credential: decrypt the secret, compute the corrected current step `t = now / period + step_offset` (`step_offset` is always 0 until `TOTP 10`), and evaluate `totp params secret` at each step in `[t-r … t+r]`.
3. On a match at step `s`: `ClaimStepIfNewer uid credId s now (now + claimTTL)`.
   - Returns `False` ⇒ this step was already consumed ⇒ `Replayed`.
4. On `NoMatch`, charge the per-user rate-limit bucket.
5. Return the `SecondFactorResult`; the caller maps it to HTTP (`TOTP 11`).

### The window, and why we do not just call `totpVerify`

`crypton`'s `ClockSkew` is a *radius* `r`: `totpVerify` checks `t-r … t+r` (`Crypto/OTP.hs`). Our config knob `skew` is the **total** window in steps — odd, default 3, so `OneStep` is what the default maps to. The mapping table and the reasoning are in [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4(a); config parsing (`TOTP 06`) rejects an even `skew` and carries the radius pre-computed in `TOTPSubsystemConfig`, so nothing here re-derives it.

`totpVerify` returns a `Bool`, and we need to know *which* step matched — both to claim it here and to learn the offset in `TOTP 10`. So evaluate `totp` per candidate step ourselves. Copy the shape of `totpVerify` while doing it: **every candidate is evaluated and every comparison is performed, with no early exit**, folded with a branch-free equality mask. Stopping at the first match leaks which step matched through timing, which is a (weak, but free to avoid) hint about the authenticator's clock.

### Replay protection is the point of this ticket

Any code within the window is accepted, so the same six digits stay valid for the whole window — 90 seconds at the default `skew = 3`, 150 s if a site widens it to 5. Without step consumption, an attacker who observes one code — shoulder-surfing, a screenshot, a leaked log, a malicious client — can replay it. `ClaimStepIfNewer` (the compare-and-set upsert from `TOTP 02`) is what makes a code single-use.

Get the units consistent: the claimed step is stored in the credential's own (offset-corrected) step numbering — the number the authenticator used. Mixing corrected and uncorrected steps silently breaks replay protection once `TOTP 10` sets a non-zero offset, and no test will catch it unless one is written specifically for a skewed credential. The cheap way to make this unbreakable is a type: e.g. `newtype CorrectedStep = CorrectedStep Word64` (what the authenticator computes, what gets claimed), kept distinct from wall-clock step numbers, with `ClaimStepIfNewer` taking `CorrectedStep` — the mix-up then fails to compile instead of being a test obligation.

### The claim expires, and that is not a compromise

A claim is a Unix timestamp in disguise: `step × period` is when the user last authenticated, to 30 seconds. Keeping it forever would be a per-credential login-time history in the database, which is both more than replay protection needs and the thing [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D13 exists to avoid. So each claim carries

```
claimTTL = (skew + 1) × period      -- 120 s at the defaults
```

and an expired claim counts as no claim (the compare-and-set in `TOTP 02` has that in its `WHERE`).

**No security is traded away for this.** The derivation is in [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §3: a claim cannot reject anything later than `(skew - 1) × period` after it was written; the TTL above adds two steps of slack for clock drift between replicas. The learned `step_offset` cancels out of that derivation — both sides are in the credential's own numbering — so a skewed credential needs no special handling.

Derive `claimTTL` from the configured `skew` and `period`, do not hardcode 120 s, and assert the derivation in a test. A site that widens `skew` must get a proportionally longer claim, or it quietly reopens the replay window it thought it was only widening for clock skew.

### Rate limiting

`doRateLimited (RateLimitUser uid)` against brig's existing single `rateLimitEnv`. **Charge only on failure.** Brig has one token bucket per `RateLimitKey`, shared with password hashing (`Brig/App.hs:301`, `settings.passwordHashingRateLimit`, `burst: 5`): if every attempt cost a token, a login that checks password *and* code would spend two, and three mistyped codes would produce a confusing 429 on an otherwise correct password.

The cost analysis and the argument for not introducing a separate bucket are in [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4; the broader question is `TOTP 20`.

### Constant time, and no oracles

- Code comparison uses constant-time primitives.
- Every failure mode returns the same thing to the caller. "No such credential", "wrong code" and "credential not activated" must not be distinguishable from outside. `TOTPCodeReused` is the one deliberate exception — decide explicitly whether to surface it or fold it into `TOTPInvalidCode`, and write down the reasoning. (Surfacing it is better UX and tells the user something useful; folding it leaks less. Default: surface it, because an attacker replaying a code already knows the code, so it tells them nothing they do not have.)

## Acceptance criteria

Unit tests, mock store, settable `Now`, no `threadDelay`:

- A freshly generated code verifies.
- With the default `skew = 3`: codes from steps `n-1`, `n`, `n+1` verify; `n-2` and `n+2` do not. **Both directions, symmetric** — a test that only covers the past would pass against an off-by-one window that silently rejects every authenticator running slightly fast.
- With `skew = 1`: only step `n` verifies. With `skew = 5`: `n-2 … n+2` verify and `n±3` do not. Parameterise the test over the window rather than hardcoding the default.
- **The same code twice: first succeeds, second fails with `TOTPCodeReused`.**
- After consuming step `n`, a still-in-window code for step `n-1` is rejected.
- Replay protection holds for the **whole** window: advance the clock step by step and replay the step-`n` code at every point where it is still acceptable; it must be refused at each one. This is the test that would catch a `claimTTL` set too short — which is otherwise invisible, since the normal path keeps working.
- Past `claimTTL`, the claim row is gone (or ignored) and a *later* code succeeds normally — the expiry must not wedge the credential.
- `claimTTL` tracks config: with `skew = 5` the claim outlives the wider window too. Assert against the formula, not a constant.
- A pending (unactivated) credential never verifies, even with a correct code.
- With two activated credentials, a code from either verifies; consuming a step on one does not affect the other.
- Another user's code does not verify.
- A code that is correct but for a different secret does not verify.
- Failures consume rate-limit tokens; **successes do not**.
- Exhausting the bucket produces the rate-limit error rather than a silent rejection.
- Property: `verify (generate t) == True` for arbitrary secrets and times, modulo replay.
- RFC 6238 Appendix B vectors through our configured parameters.

## Notes

- Recovery codes are not handled here. The fallback branch is added in `TOTP 14`, and it matters that it sits *after* the TOTP branch: verifying a recovery code is an argon2id check (~100 ms, memory-hard), so reaching it on every failed TOTP attempt would be a real cost amplification.
- `HasActivatedCredential` already exists from `TOTP 05`; the login path uses it to decide whether a second factor is required at all.
