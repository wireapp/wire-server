# A2 — real codes

> **Disclaimer in force:** a code stays valid for its whole acceptance window and can be replayed within it. Clock skew is not handled — if your phone's clock is off by more than a minute, enrolment will appear to be broken. Secrets are still stored unencrypted.

**Demo:** scan the QR with Google Authenticator. The six digits it shows are accepted. Six different digits are rejected. This is the first step where the feature is recognisably itself.

## Scope

- **`crypton` wiring.** `Crypto.OTP`: build `TOTPParams` from the credential's stored `algorithm` / `digits` / `period`, evaluate `totp` at the current step, compare. `plan-otp.md` §0.
- **Verification in the subsystem.** `VerifySecondFactor` and the activation check both land here. Compute candidate steps, evaluate **all** of them, fold the comparisons branch-free — no early exit, mirroring what `crypton`'s own `totpVerify` does. We cannot call `totpVerify` directly because we need to know *which* step matched, and A5 needs that number. §4 step 2.
- **Constant-time comparison.** Not optional, not "A5 will fix it". Getting this wrong now means writing the code twice.
- **`totp-invalid-code`** starts being thrown, from activation.
- **The integration-test TOTP generator** (`plan-otp-04`): a client-side implementation in the test suite that computes codes the way an authenticator app would. Everything from here on is untestable without it.
- **The real handler module.** `Brig/API/TOTP/Mock.hs` is deleted (`git rm`) and replaced with handlers that call the subsystem. `plan-otp-07`.

Still fake:

- **No replay protection.** The same six digits work for the whole window, repeatedly. A5.
- **No clock skew tolerance beyond the fixed window**, and no learned `step_offset`. A phone that is persistently a minute fast cannot enrol. A5.
- **No `clientTime` check.** The field is parsed and ignored, so a user with a skewed clock gets "invalid code" with no diagnostic. A5.
- **No rate limiting.** Brute force is bounded by nothing. A5.
- Secrets still plaintext at rest. A5.

## Window size for this step

Use the final default — `skew = 3`, i.e. radius 1, a 90-second code lifetime (§4, RFC 6238 §5.2's recommendation). Resist widening it "so the demo works". A wide window is precisely the wrong fix for skew, it lengthens every code's life for every user, and A5's learned offset is the right fix. If the demo fails because someone's phone clock is wrong, that is the feature working and the diagnostic missing — which is A5's job, and is worth seeing now.

Hardcode the radius rather than adding the `skew` config knob; the knob needs the odd/even validation (§4) and belongs with the rest of the skew work in A5.

## The RFC vectors

Assert RFC 6238 Appendix B's test vectors (T = 59, 1111111109, 1111111111, 1234567890, 2000000000, 20000000000, for SHA1/SHA256/SHA512) against **our own parameter plumbing**, not against `crypton` in isolation. `crypton` is almost certainly correct; what is unproven is that we hand it the right secret, the right step, and the right digit count. §7.

## Done when

- The demo works with at least two independent authenticator apps, on a phone whose clock is correct.
- Integration tests, using the generator from `plan-otp-04`: enrol → parse the `otpauth://` URI → compute a code → activate → the listing shows it activated; a wrong code → 403 and the credential is still pending; a code computed for a *different* secret → 403.
- RFC 6238 Appendix B vectors green for all three hash algorithms.
- **The honesty test rotates:** `testA1ActivationAcceptsAnyCode` is deleted; `testA2CodeIsReplayableWithinWindow` replaces it, asserting that the same code activates and then verifies again within the window. A5 deletes that one and asserts the opposite.
- `Brig/API/TOTP/Mock.hs` no longer exists; `QuickCheck` comes back out of `brig.cabal`'s library stanza.
- Route `Description`s updated: drop "codes are not yet verified", add the replay and clock-skew caveats.
- `changelog.d/` entry.
