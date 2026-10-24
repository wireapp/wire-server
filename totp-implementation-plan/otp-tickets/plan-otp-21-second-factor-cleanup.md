# TOTP 21 — unify second-factor and re-authentication handling

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 18
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6.

Milestone 1.3 (debt). Substantially larger than the whole TOTP feature; plan it as its own epic, not as a follow-up commit. Nothing in milestones 1.1–1.2 depends on it.

## Background

"This operation is sensitive, prove it's really you" is implemented several different ways in wire-server, with inconsistent semantics, inconsistent error handling, and one outright blunt-instrument workaround. The TOTP work builds a clean mechanism alongside the existing ones rather than refactoring on the way past. This ticket is that refactor.

### Mechanism 1 — password re-authentication

`AuthenticationSubsystem.reauthenticateEither`, plus `authenticateEither`, `verifyUserPassword`, `verifyUserPasswordError`, `verifyProviderPassword`. Call sites include:

- `Brig/API/Auth.hs:293` — internal `/i/users/:uid/reauthenticate`, called by galley and spar.
- `Brig/API/OAuth.hs:370, 385` — revoke OAuth account access, delete refresh token.
- `Brig/User/Auth.hs:410` — `ssoLogin`.
- `Brig/User/Auth.hs:448` — `legalHoldLogin`.
- `Brig/API/User.hs:1058ff` — account self-deletion, which does **not** use `reauthenticateEither` but hand-rolls `UserStore.lookupHashedPassword` + `HashPassword.verifyPassword`.
- `Brig/API/User.hs:978ff` — password change, likewise hand-rolled.
- `UserSubsystem/Interpreter.hs:1197` — `acceptTeamInvitationImpl`.
- Client deletion (`RmClient`).

### Mechanism 2 — emailed second-factor code

`enforceVerificationCodeEither` (`Wire/AuthenticationSubsystem/Interpreter.hs:423`), gated on the `sndFactorPasswordChallenge` team feature, with

```haskell
data VerificationAction = CreateScimToken | Login | DeleteTeam
```

Call sites: `Brig/User/Auth.hs:127` (`verifyLoginCode`), `Brig/API/Auth.hs:297` (internal reauthenticate), and spar's SCIM token path.

For anyone who remembers this as "the SCIM token thing": it also covers login and delete-team.

### Mechanism 3 — TOTP

`EnsureReauthenticated` / `SensitiveOperation`, added by `TOTP 11` and extended by `TOTP 18`. This is the shape the other two should converge on.

### Mechanism 4 — the blunt instrument

`guardSecondFactorDisabled` (`Brig/Provider/API.hs:1012`, plus siblings in `Galley/API/Public/Bot.hs:44` and `FeaturesConfigSubsystem/Interpreter.hs:258`). Its own comment says what it is:

> If second factor auth is enabled, make sure that end-points that don't support
> it, but should, are blocked completely. (This is a workaround until we have
> 2FA for those end-points as well.)

It appears **roughly 35 times in `Brig/Provider/API.hs` alone**. Turning on 2FA for a team disables that team's entire provider/bot API.

### Known specific defects

- `ssoLogin` (`Brig/User/Auth.hs:408-427`) matches on the error from `reauthenticateEither` and treats `AuthInvalidCredentials`, `ReAuthCodeVerificationRequired`, `ReAuthCodeVerificationNoPendingCode` and `ReAuthCodeVerificationNoEmail` as **success**. The surrounding comment justifies why `ReAuthMissingPassword` must throw; the other four swallow-as-success cases are justified nowhere. Establish whether each is correct and then write it down, because the next reader will have the same question.
- Password verification is hand-rolled in at least two places, so the rate-limit key and the `PasswordStatus` (rehash-needed) handling differ from everywhere else. `TOTP 18` fixes two of these; find the rest.
- `ReAuthError` and `VerificationCodeError` are separate types flattened into HTTP responses differently at each call site.

## What this ticket should produce

1. **A complete inventory** of operations requiring a password and/or a second factor across brig, galley and spar — including the ones not listed above, which certainly exist. Per operation: which mechanism, what it returns in each failure mode, whether that is intentional.

2. **One mechanism.** Converge on `EnsureReauthenticated`: a single error type, a single set of HTTP error labels, and the policy (password required? second factor? which kinds?) decided once per `SensitiveOperation` rather than at each call site.

3. **Retire `guardSecondFactorDisabled`.** Either give the provider/bot endpoints real second-factor support, or make an explicit, documented decision that they are out of scope for 2FA and block them for a stated reason rather than as a "workaround until".

4. **Decide the fate of the emailed code.** With TOTP shipped, is `sndFactorPasswordChallenge` (a) retained as an alternative for users without an authenticator app, (b) retained for teams that prefer it, or (c) deprecated? A product decision with a migration path attached, not just a refactor.

5. **Tests.** The inventory becomes a test matrix: every `SensitiveOperation` × {no credential, wrong password, right password + no second factor, right password + wrong second factor, right password + right second factor} has a deterministic asserted outcome. Today several of those cells are undocumented and at least four are silently "success".

## Constraints

- **Backwards compatibility is the hard part.** Changing when an endpoint demands a second factor is client-visible. TOTP sidesteps this by being per-user opt-in; a unification cannot, because it touches existing emailed-code behaviour. Expect API versioning and a staged rollout.
- `/i/users/:uid/reauthenticate` is consumed by galley and spar; its contract cannot change unilaterally.
- Keep the `AuthenticationSubsystem → TOTP` dependency direction intact ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §1).
