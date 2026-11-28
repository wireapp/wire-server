# TOTP 17 — recovery code management

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 13
- TOTP 14
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §5, and the open questions left at the end of `TOTP 13` and `TOTP 14`.

Milestone 1.2.

## Scope

Milestone 1.1 ships recovery codes that are generated once and can be used once each. That is enough to not be locked out, and not enough to live with: a user who has used eight of ten codes has no way to find out, and no way to get a fresh set.

### Endpoints

| verb | path | body → response |
|---|---|---|
| GET | `/self/totp/recovery-codes` | → 200 `TOTPRecoveryCodesStatus` (counts only, never the codes) |
| POST | `/self/totp/recovery-codes` | `ReauthBody` → 200 `TOTPRecoveryCodes` (regenerate) |

```haskell
data TOTPRecoveryCodesStatus = TOTPRecoveryCodesStatus { remaining :: Int, total :: Int }
```

Regeneration invalidates the entire previous set atomically — delete-then-insert in one transaction, or a dedicated `ReplaceRecoveryCodes` store operation; a crash between two separate store calls must not silently leave the user with zero codes — and requires full re-authentication (password + second factor) via `EnsureReauthenticated`.

`GET` returns counts only. There is no endpoint, ever, that re-reads issued codes — that is what makes "write these down now" meaningful.

nginz entries for both paths (`/self/totp/recovery-codes$`), no `oauth_scope`.

### Notifications

- Email the user when a recovery code is **used** (the `MatchedRecoveryCode` result from `VerifySecondFactor` is the trigger, `TOTP 14`). This is the strongest single account-takeover signal in the whole feature.
- Email the user when codes are **regenerated**.
- Warn when the remaining count drops below a threshold (e.g. 3), either in the login response or out of band.
- Consider notifying on repeated **failed** second-factor attempts too (OWASP recommends surfacing failed MFA logins, with time and origin): a correct password followed by a stream of wrong codes is the classic signature of a compromised password, and the user should be told to rotate it.

### Open questions to resolve here

- Should using a recovery code force enrolment of a new authenticator before the session is fully usable, or just log in and nag? `TOTP 14` deferred this.
- Should a user be allowed to regenerate codes without having any activated credential (i.e. after disabling TOTP entirely)? Probably not — codes with no second factor to recover are just a weaker password.
- Interaction with team-level enforcement (`TOTP 19`): can a team disable recovery codes, on the grounds that a printable bypass undermines an enforced second factor?

## Acceptance criteria

- `remaining`/`total` track consumption accurately, including across regeneration.
- Regeneration invalidates every code from the previous set — verify that an old code fails afterwards.
- Regeneration requires password **and** second factor; missing either → 403.
- `GET` never returns code material.
- Using the last remaining code: documented behaviour, tested.
- Notifications are emitted for use and for regeneration.
- Full OpenAPI documentation in the same commit, per [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D11.
- `changelog.d/` entry.

## Notes

- Keep the argon2id cost discipline from `TOTP 14`: `GET` must answer from row counts, not by hashing anything.
