# TOTP 18 — second factor on the remaining sensitive operations

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 11
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6 "Endpoints to wire up".

Milestone 1.2.

## Scope

`TOTP 11` built `EnsureReauthenticated` and wired it into login, enrolment and TOTP deletion. This ticket wires it into everything else that asks for a password today, so that "I have your password" stops being enough for any account-altering operation.

The `SensitiveOperation` enumeration already exists in full; this is wiring, not design.

### Endpoints

| endpoint | request type | notes |
|---|---|---|
| `DELETE /self` | `DeleteUser` | account self-deletion; also has an email-code path today (`Brig/API/User.hs:1058ff`) |
| `DELETE /clients/:id` | `RmClient` | |
| `POST /clients` | `NewClient` | add a new client device; uses `newClientPassword` through `ClientSubsystem.upsertClient` (`ClientSubsystem/Interpreter.hs:209`) |
| `PUT /self/password` | `PasswordChange` | password verification is hand-rolled here (`Brig/API/User.hs:978ff`), not routed through the subsystem — fix that on the way |
| email change | | |
| `POST /oauth/applications/:id/revoke-access` | `PasswordReqBody` | `Brig/API/OAuth.hs:369` |
| `DELETE /oauth/.../refresh-tokens/:id` | `PasswordReqBody` | `Brig/API/OAuth.hs:382` |
| `POST /i/users/:uid/reauthenticate` | `ReAuthUser` | internal; galley's delete-team and spar's SCIM-token creation inherit the second factor through it |
| accept team invitation | | `UserSubsystem/Interpreter.hs:1197` uses `verifyUserPasswordError` directly |
| create app | | `AppSubsystem/Interpreter.hs:108` uses `verifyUserPasswordError` directly |
| `POST /password-reset/complete` | `CompletePasswordReset` | password reset via email code; see A6 for the design details |

Each gains an optional `totp_code` field in its existing request body. That is an additive schema change, and enforcement is per-user opt-in by enrolment, so users without TOTP see no change and no new API version is required for the bodies.

Two endpoints have no natural request body. Add one ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §6). Never a query parameter — a code in the URL is written to access logs at every proxy hop, and OTPs must not be logged — and not a custom header: nginz's zauth module rewrites `Z-*` headers, and a non-`Z-` header is easy to drop in a proxy chain.

### Do the internal reauthenticate endpoint carefully

`/i/users/:uid/reauthenticate` is consumed by galley and spar. Changing its contract affects both; a `totp_code` field has to be plumbed from those services' own request bodies to be useful at all. Sequence it so the internal endpoint accepts the field before the callers start sending it.

## Acceptance criteria

- For **every** endpoint in the table, the full matrix is tested: {no TOTP credential, activated credential} × {no code, wrong code, right code}. Cells are asserted, not left to inference.
- The opt-in guarantee holds everywhere: a user without TOTP sees byte-identical behaviour before and after, including when the client sends a `totp_code` field anyway.
- Password verification in `PUT /self/password` and `DELETE /self` goes through the subsystem rather than hand-rolled `lookupHashedPassword` + `verifyPassword`, so rate-limit keys and `PasswordStatus` rehash handling match the rest of the codebase.
- Replay protection holds across endpoints: a code consumed at login cannot authorise a client deletion in the same window.
- Error labels (`totp-required`, `totp-invalid-code`) are identical across all endpoints.
- Full OpenAPI documentation for each changed body, in the same commit.
- `changelog.d/` entry.

## Notes

- This ticket is the natural point to notice how much duplicated reauth logic exists. Resist fixing it here — that is `TOTP 21`, and mixing a behaviour change with a large refactor makes both unreviewable. Note what you find; leave it.
- `guardSecondFactorDisabled` (~35 call sites in `Brig/Provider/API.hs`) is explicitly out of scope; it belongs to 21.
