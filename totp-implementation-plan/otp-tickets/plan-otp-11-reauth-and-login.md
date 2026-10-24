# TOTP 11 — `EnsureReauthenticated` and TOTP at login

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 09
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6.

## Scope

The second-factor mechanism itself, plus its first consumer: `POST /login`.

### The mechanism

In `AuthenticationSubsystem`:

```haskell
data SensitiveOperation
  = OpLogin | OpDeleteSelf | OpDeleteClient | OpChangePassword | OpChangeEmail
  | OpCreateScimToken | OpDeleteTeam | OpEnrolTOTP | OpDeleteTOTPCredential
  | OpRevokeOAuthAccess
  deriving (Eq, Show, Bounded, Enum)

EnsureReauthenticated :: Local UserId -> SensitiveOperation -> ReauthBody -> AuthenticationSubsystem m ()
```

Semantics, in order:

1. Verify the password if the account has one (existing `reauthenticateEither` logic, unchanged — do not reimplement it). On failure, stop: do **not** go on to the second factor, so a wrong password never burns a TOTP step or a recovery code ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §4, ordering rule).
2. If `TOTP.HasActivatedCredential`: require `totpCode` and verify it via `TOTP.VerifySecondFactor`. Missing → `TOTPRequired`; `NoMatch` → `TOTPInvalidCode`; `Replayed` → `TOTPCodeReused` (`TOTP 09`).
3. Otherwise: no second factor.

Step 2's condition is what makes `OpEnrolTOTP` work without a bootstrap paradox: a user enrolling their *first* authenticator has no activated credential, so only the password is demanded; a user enrolling their *second* must present a code from the first. The second case is the point — without it, anyone with a stolen session and password could register their own authenticator and bypass the second factor from then on. A pending credential never counts, in either direction. See [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6.

This is where the `AuthenticationSubsystem → TOTP` dependency materialises, and why `TOTP 06` put `TOTP` below it in brig's effect stack. If the build complains about effect ordering here, fix the stack, not the dependency direction.

The full enumeration of `SensitiveOperation` is defined now even though only `OpLogin`, `OpEnrolTOTP` and `OpDeleteTOTPCredential` are wired in milestone 1.1. Having the type complete makes `TOTP 18` a wiring exercise rather than a redesign.

`EnsureReauthenticated` returns `()` here. `TOTP 26` (milestone 1.3) makes it return evidence that the caller must hand to the operation it is about to perform, so that forgetting the call becomes a type error. Nothing in this ticket should make that harder — in particular, keep the call adjacent to the operation it guards rather than burying it in a handler prologue three functions away.

### Login

`POST /login`: add an optional `totp_code` field (type `SecondFactorCode` — it must accept recovery codes, `TOTP 14`) to `Wire.API.User.Auth.Login` and route it through `EnsureReauthenticated ... OpLogin`.

### Rewire the enrolment endpoints

`TOTP 07` shipped enrol/activate with password-only verification as a documented interim. Switch those handlers to `EnsureReauthenticated … OpEnrolTOTP` **in this ticket** — otherwise the attack described above (stolen session plus password registers a new authenticator) stays open for exactly as long as the gap between the two tickets.

### What this ticket deliberately does not do

It does **not** touch `verificationCode` / `EnforceVerificationCodeEither`, the emailed second factor. The two mechanisms coexist. Unifying them is `TOTP 21`, which is a much larger piece of work and must not be smuggled in here.

Factual note, because it is easy to misremember: the existing `verificationCode` is not only for SCIM tokens. `VerificationAction = CreateScimToken | Login | DeleteTeam`, and `enforceVerificationCodeEither` is already called from `Brig.User.Auth.verifyLoginCode` (login) and from `Brig.API.Auth.reauthenticate` (the internal endpoint galley and spar use). So login already has an emailed second factor; after this ticket it has two independent ones. Decide and document what happens when a user is subject to both — the straightforward rule is "TOTP takes precedence if the user has an activated credential", and that rule must be written down and tested.

## Backwards compatibility — the thing to protect

Enforcement is **per-user opt-in by enrolment**. A user with no activated credential must see no behaviour change anywhere. Adding an optional `totp_code` field to an existing request body is additive, so no new API version is needed for the body (the `/self/totp` endpoints stay `From 'V19`).

The regression test for this is more important than any of the positive tests.

## Acceptance criteria

- Unit tests for `EnsureReauthenticated` across the matrix: for each of {no TOTP credential, activated credential} × {no password, wrong password, right password} × {no code, wrong code, right code}, the outcome is asserted. No cell left undefined. "No password" is two different cells — the body omits it (a client error for a password account) vs. the account has none (the SSO case) — keep them apart.
- Integration tests:
  - user **without** TOTP: login works exactly as before, with and without a `totp_code` field present in the body;
  - user **with** TOTP: login without a code → `totp-required`;
  - with a wrong code → 403, and the existing login failure counter behaves as it does for the email-code path;
  - with a correct code → success, cookie issued as normal;
  - **replay across the HTTP boundary**: the same code at login twice → the second fails;
  - enrolling a second authenticator requires a code from the first; a pending credential neither satisfies nor triggers the requirement;
  - a code belonging to a different user → 403;
  - a user subject to both the emailed code and TOTP behaves per the documented precedence rule;
  - SSO users: confirm the documented behaviour (SSO users have no password; do they get a TOTP challenge? decide, document, test — `enforceVerificationCode` exempts SAML users today and the same question applies).
- `changelog.d/` entry describing the new optional login field.

## Notes

- `Brig/User/Auth.hs:408-427` (`ssoLogin`) swallows several `reauthenticateEither` error cases as success. Do not extend that pattern. Fixing it is `TOTP 21`; just do not make it worse.
- Keep the HTTP error mapping in one place so `totp-required` and `totp-invalid-code` look identical regardless of which endpoint produced them.
