# TOTP 12 — disabling TOTP

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 07
- TOTP 11
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §5, §6.

## Why it blocks on 11

Removing a second factor must itself require the second factor. A delete endpoint protected by a password alone would mean that anyone who obtains the password can strip the 2FA they are supposedly being stopped by — which is the entire threat model, inverted. So this cannot ship with `TOTP 07`, however tempting it is to add one more route there.

## Scope

```
DELETE /self/totp/:id      totp-delete      ReauthBody → 200
```

`From 'V19`, documented to the same standard as `TOTP 07`.

The handler calls `EnsureReauthenticated luid OpDeleteTOTPCredential body` and then `TOTP.DeleteCredential`. Password **and** a valid TOTP code (or recovery code, once `TOTP 14` lands) are both required.

### Decisions this ticket has to make and write down

- **Deleting the last credential.** Does it silently turn TOTP off for the user, or does it require an extra confirmation / a distinct endpoint? Recommended: allow it, but return a response or emit an event that makes it unmistakable, and notify the user by email. Unexplained silent 2FA removal is an account-takeover signal.
- **Recovery codes on last-credential deletion.** When the final activated credential goes away, the user's recovery codes are meaningless. Delete them in the same operation (`TOTPStore.DeleteRecoveryCodes`) rather than leaving valid password-equivalent secrets in the database for an account that no longer has a second factor. This is easy to miss and is a genuine leak.
- **Pending credentials.** Deleting a pending (never-activated) credential should not require a TOTP code — the user cannot produce one for it. Allow password-only in that case, and test it.

### Notification

Emit an email (and/or a user event) on second-factor removal, via `EmailSubsystem` / `Wire.Events`. This is standard practice for a reason: it is the one signal a victim gets when an attacker with their password removes their 2FA.

## Acceptance criteria

Integration tests:

- Delete with password + valid code → 200; `GET /self/totp` no longer lists it; login no longer requires a second factor (if it was the last credential).
- Delete with password but **no** code → `totp-required`.
- Delete with password and a **wrong** code → 403, credential still present.
- Delete with a correct code but no password → 403.
- Delete another user's credential id → 404.
- Delete a **pending** credential with password only → 200.
- Deleting the last activated credential also removes the user's recovery codes — assert this against the store, not just via the API.
- A code already consumed at login cannot be reused to authorise the delete (replay protection holds across endpoints).
- Notification is emitted.

Unit tests for the last-credential and recovery-code-cleanup logic.

## Notes

- Add the nginz path if `TOTP 08` has not already covered it with the `([^/]*)$` pattern. nginz matches on path, not method — verify rather than assume.
- Under team enforcement (`TOTP 19`/`TOTP 24`), deleting the last credential makes the user **immediately overdue** — deliberately no fresh grace period, per `TOTP 24`'s evasion-loop rule. Warn the user in the response and docs before they do that to themselves.
- `changelog.d/` entry.
