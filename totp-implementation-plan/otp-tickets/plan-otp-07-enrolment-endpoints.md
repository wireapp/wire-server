# TOTP 07 — enrolment endpoints

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 04
- TOTP 05
- TOTP 06
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §5, §0 (QR / `otpauth://`).

## Why this one matters

This is the ticket that delivers **"the backend can give you a token for adding Wire to your authenticator app"**. Everything before it is scaffolding; everything after it extends a working feature. Prioritise accordingly: `01 → 02/03 → 05 → 06 → 07` is the critical path and should be kept clear of other work.

## Scope

`libs/wire-api/src/Wire/API/Routes/Public/Brig/TOTP.hs`, spliced into `BrigAPI`, plus handlers in `services/brig/src/Brig/API/Public.hs`.

| verb | path | `Named` | body → response |
|---|---|---|---|
| POST | `/self/totp` | `totp-enrol` | `NewTOTPCredential` (embeds `ReauthBody` — one request, one body; [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §2) → 201 `TOTPEnrolment` |
| POST | `/self/totp/:id/activate` | `totp-activate` | `ActivateTOTPCredential` → 200 `TOTPRecoveryCodes` |
| GET | `/self/totp` | `totp-list` | → 200 `[TOTPCredentialInfo]` |
| PUT | `/self/totp/:id` | `totp-rename` | `TOTPCredentialName` → 200 |

All `:> From 'V19` (the current development version). `DELETE /self/totp/:id` is `TOTP 12` — it needs the second factor itself and therefore blocks on login.

Handlers verify the password via `AuthenticationSubsystem.verifyUserPasswordError` **before** calling the TOTP effect (this is what keeps the dependency arrow pointing one way, see `TOTP 05`), then delegate. Add a `totpAPI :: ServerT TOTPAPI (Handler r)` block next to `userGroupAPI`. Password-only is a documented interim: `TOTP 11` replaces it with `EnsureReauthenticated … OpEnrolTOTP`, which additionally demands a code from an existing authenticator once one is activated.

Until `TOTP 13` lands, activation returns `TOTPRecoveryCodes { codes = [] }`. The shape is final; the content arrives later.

Two handler details that exist because the enrolment response carries a secret:

- Set `Cache-Control: no-store` on the enrolment response, and confirm no logging or tracing middleware captures response bodies on this route.
- **Activation sends the user an email** ("a new authenticator was added to your account") via `EmailSubsystem` — OWASP: notify out-of-band on every MFA factor change. Because TOTP is opt-in by enrolment, an attacker holding a stolen password can otherwise enrol *their* authenticator on a victim's account silently, and this mail is the victim's only signal. If the email plumbing is heavier than this ticket wants, it may land with `TOTP 12` — but it must land before `TOTP 08` makes the endpoints public.

## Documentation is part of this ticket

Per [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D11, the OpenAPI documentation lands in the same commit as the routes. Each route gets `Summary` plus a `Description` that actually explains the flow. Between them the descriptions must cover:

- that enrolment is **two steps** (enrol, then activate with a code from the app), and that a credential is useless until activated;
- the exact `otpauth://` URI format, field by field;
- that **the client renders the QR code** — the server returns the URI and the Base32 secret and no image ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §0);
- that the secret is returned **exactly once** and `GET /self/totp` will never return it again;
- the enrolment TTL and what `totp-enrolment-expired` means;
- what `clientTime` is for and what `totp-client-clock-skew` means (the server ignores `clientTime` until `TOTP 10`, but clients should send it from day one — document it as required);
- that recovery codes come back from activation and are shown once.

Every error the handler can produce has a `CanThrow`. A reviewer should be able to implement a client from the generated OpenAPI document alone, without reading Haskell.

## Acceptance criteria

- `cabal build brig` clean; `integration/test/Test/Swagger.hs` green and showing the new endpoints.
- Integration tests in `integration/test/Test/TOTP.hs`, using the helper from `TOTP 04`:
  - happy path: enrol → parse the `otpauth://` URI → compute a code → activate → `GET /self/totp` shows one activated credential;
  - the URI the server emits is parseable by an independent authenticator implementation and yields codes the server accepts;
  - enrol without a password → 403;
  - enrol with a wrong password → 403;
  - activate with a wrong code → 403, credential still pending in the listing;
  - activate twice → 409;
  - activate someone else's credential id → 404, not 403 (no existence oracle);
  - `GET /self/totp` never contains the secret or the URI;
  - rename works and shows in the listing;
  - `maxCredentialsPerUser` exceeded → 409;
  - the enrolment response carries `Cache-Control: no-store`;
  - activation emits the new-authenticator notification;
  - TOTP disabled in config → `totp-disabled` on every endpoint;
  - requesting the endpoints at `V18` → 404.
- Golden/OpenAPI snapshot updated.
- `changelog.d/` entry.

## Notes

- Not reachable through nginz yet — that is `TOTP 08`, intentionally a separate, tiny ticket so a chart typo cannot block this one.
- Resist adding `DELETE` here "since it is trivial". Deleting a second factor must itself require the second factor, which does not exist until `TOTP 11`. Shipping a delete endpoint that only checks a password would be a real security regression.
