# A3 — login challenge

> **Disclaimer in force:** there is no recovery path. If you enrol and lose your authenticator before A4 lands, you are locked out of that account and need an operator to delete the row. Activation returns `{"codes": []}` — the shape is final, the content arrives in A4. Replay, skew and rate limiting are still absent.

**Demo:** log out. Log in with your password. The client says "enter the code from your authenticator". Type it. You are in.

This is the step the whole feature exists for. Everything before it was a settings screen.

## Read first

`../otp-tickets/plan-otp.md` §6 in full, and `plan-otp-11`. §6 now also contains the SSO rule (D15) and the password-reset analysis (D16); the former is in scope here, the latter is A6.

## Scope

- **`EnsureReauthenticated`** in `AuthenticationSubsystem`, with the full `SensitiveOperation` enumeration defined even though only three constructors are wired up. §6, `plan-otp-11`.
- **It returns evidence, not `()`** — `plan-otp-26`, pulled forward to here. See below.
- **`POST /login`** gains `totp_code :: Maybe SecondFactorCode` on the `Login` body. Additive, optional, no version bump — see `plan-otp.md` §5 "Backwards compatibility, and the one place it breaks".
- **Rewire enrolment and delete** to `EnsureReauthenticated … OpEnrolTOTP` / `OpDeleteTOTPCredential`. A1 shipped those as password-only; this closes the gap where a stolen session plus a stolen password could quietly add an attacker's authenticator, and it makes `DELETE /self/totp/:id` require the factor it is deleting.
- **The bootstrap rule:** enrolling your *first* authenticator needs only a password (you have no activated credential); enrolling your *second* needs a code from the first. A pending credential never counts, in either direction.
- **SSO accounts are rejected at enrolment** (D15). An SSO account has no password and no credential, so without this check `EnsureReauthenticated` would pass having verified nothing whatsoever. This is the one genuinely dangerous hole in the design and it closes here, not in A7.

Still fake:

- **No recovery codes.** `TOTPRecoveryCodes { codes = [] }` from activation. A4.
- Everything in A2's "still fake" list. A5.

## Build the evidence type now, not in a later refactor

`plan-otp-11` ships `EnsureReauthenticated :: … -> m ()` and leaves a note that `plan-otp-26` will later make it return proof. `plan-otp-26` is then a fifteen-call-site refactor across brig, galley and spar. **Do it here instead.** Read `plan-otp-26` in full; it is short and its design is right. The only thing wrong with it is its position in the sequence.

The argument is arithmetic. A3 wires three operations (`OpLogin`, `OpEnrolTOTP`, `OpDeleteTOTPCredential`). A6 wires seven more. Getting the signature right with three call sites in front of you is nearly free; converting it afterwards is a cross-service refactor that competes for attention with everything else and will keep being deferred. `plan-otp-26` itself recommends spiking the shape on one operation before converting fifteen — A3 *is* that spike, except nothing needs converting afterwards.

```haskell
-- constructor NOT exported
data Reauthenticated (op :: SensitiveOperation) = UnsafeReauthenticated (Local UserId)

EnsureReauthenticated
  :: Local UserId -> Sing op -> ReauthBody -> AuthenticationSubsystem m (Reauthenticated op)
```

Three properties, each earning one design detail: **unforgeable** (constructor unexported, so the only way to hold one is to have called the check); **operation-specific** (the phantom `op` stops evidence for a cheap operation being reused for an expensive one); **bound to a user** (it carries the `Local UserId` it was issued for, and the operation reads *that* rather than a separately-threaded id — evidence for user A authorising an action on user B is the failure mode worth the most here).

What it is not: a token. Not serialisable, not storable, no expiry, never crosses a process boundary. If it ever grows a `ToJSON` instance it has become a capability token with an unexamined lifetime. Say so in the module header.

The value is proportional to how far the evidence is **threaded**. A `Reauthenticated` produced and discarded two lines later proves nothing; the argument has to reach the function that does the irreversible thing. With three operations that is a small amount of work and it is the entire point.

One risk to spike early: Polysemy effect signatures returning a phantom-indexed type can get awkward around `makeSem` and higher-order interpreters. Try it on `OpLogin` before committing to the shape.

## The two things that must not break

**`totp-required` is unreachable until the password has been verified.** Otherwise the response distinguishes "this account exists and has a second factor" from "bad credentials" before any secret is checked — an enumeration oracle that also discloses who has 2FA on. The test is that "wrong password, no code" and "wrong password, correct code" return exactly what a wrong password returns today, byte for byte. `plan-otp.md` §5.

**A user with no activated credential sees no change anywhere.** This is the regression test that matters more than any positive test in this file. Per-user opt-in by enrolment is the entire safety argument for touching the login path at all.

## Why this is safe to put on the login path

`POST /login` is not and cannot be version-gated. The protection is the README's invariant: the only way to get an activated credential is through a `From 'V19` endpoint, so on a backend with development versions disabled the new branch is unreachable. Re-read that section before implementing this step — it is the one place where an innocuous-looking change (a test fixture, a support tool, an internal endpoint that creates a credential) turns into production users being challenged for a factor they never enrolled.

## Done when

- The demo works, through nginz, on staging, from a real client build.
- The unit-test matrix from `plan-otp-11`: {no credential, activated credential} × {no password, wrong password, right password} × {no code, wrong code, right code}, every cell asserted. "No password" is two distinct cells — the body omits it, versus the account has none (SSO) — keep them apart.
- Integration tests: login unaffected for a user without TOTP, with and without a `totp_code` field present; `totp-required` without a code; 403 with a wrong code; success with a correct one and a cookie issued as normal; a code belonging to another user → 403; enrolling a second authenticator requires a code from the first; a pending credential neither satisfies nor triggers the requirement; **SSO user enrolling → rejected**.
- The documented precedence rule between TOTP and the existing emailed `verification_code`, written down and tested. They coexist until `plan-otp-21`; "TOTP wins if the user has an activated credential" is the straightforward rule but it must be a decision, not an accident.
- A negative compilation test (or a documented, reviewed example) showing that calling a guarded operation without evidence does not compile, and that evidence for the wrong `op` does not either. Without this the invariant rots the first time someone adds a handler.
- **Honesty test:** `testA3NoRecoveryPathExists`, asserting activation returns an empty `codes` array. A4 deletes it.
- `changelog.d/` entry describing the new optional login field.
