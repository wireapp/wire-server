# TOTP 05 — `AuthenticationSubsystem.TOTP` effect: enrolment

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 01
- TOTP 02
- TOTP 03
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §1 (module layout, dependency direction), §4.

## Scope

The subsystem effect and the enrolment half of its interpreter. Verification (`VerifySecondFactor`) is deliberately **not** in this ticket — see `TOTP 09`. Splitting them keeps each reviewable and gets the enrolment endpoint shipped sooner.

### Modules

```
Wire/AuthenticationSubsystem/TOTP.hs             -- the GADT + makeSem
Wire/AuthenticationSubsystem/TOTP/Config.hs      -- TOTPSubsystemConfig (named to avoid
                                                 -- colliding with the galley team-feature
                                                 -- TOTPConfig, TOTP 19; brig imports both)
Wire/AuthenticationSubsystem/TOTP/Error.hs       -- TOTPError + HTTP mapping
Wire/AuthenticationSubsystem/TOTP/Interpreter.hs -- enrolment logic
```

### Operations implemented here

```haskell
EnrolCredential        :: UserId -> Text -> UTCTime -> TOTP m TOTPEnrolment
ActivateCredential     :: UserId -> TOTPCredentialId -> TOTPCode -> UTCTime -> TOTP m [RecoveryCode]
ListCredentials        :: UserId -> TOTP m [TOTPCredentialInfo]
RenameCredential       :: UserId -> TOTPCredentialId -> Text -> TOTP m ()
DeleteCredential       :: UserId -> TOTPCredentialId -> TOTP m ()
DeleteAllCredentials   :: UserId -> TOTP m ()
HasActivatedCredential :: UserId -> TOTP m Bool
```

Declare `VerifySecondFactor` in the GADT now (so the effect's shape is settled) and leave the interpreter branch as an explicit `error "see TOTP 09"` that no caller reaches yet — or omit it and add it in 09, whichever the reviewer prefers. Do not half-implement it.

`ActivateCredential` returns `[RecoveryCode]`; until `TOTP 13` lands it returns `[]`. The *type* is settled here so the endpoint response shape does not change later.

### Enrolment logic

1. `GetCredentials` and reject if the user is already at `maxCredentialsPerUser` credentials, counting activated **and** unexpired pending ones (`TOTPTooManyCredentials`) — counting only activated ones would let a client mint unbounded pending rows inside the TTL.
2. Generate a 20-byte secret from the `Random` effect (160 bits, per RFC 4226 §4 R6).
3. Encrypt it (`TOTP 03`), insert a pending row with `activated_at = NULL`, stamping the credential with the *configured* algorithm, digits and period (`TOTP 06`) so it keeps them for life.
4. Build the `otpauth://` URI and return `TOTPEnrolment` with `serverTime` and `expiresAt`.

### Activation logic

1. Load the credential; if absent → `TOTPNotFound`; if already activated → `TOTPAlreadyActivated`; if pending and `created_at < Now.get - pendingEnrolmentTTL` → `TOTPEnrolmentExpired`.
2. Verify the submitted code against the secret. On success, `SetCredentialActivated`.
3. On failure → `TOTPInvalidCode`, leaving the credential pending so the user can retry.

### All the filtering and policy is here, not in the store

`TOTPStore` is a dumb wrapper ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D12, `TOTP 02`). So this interpreter is where:

- the TTL comparison happens, against `Now.get` — which is why it is unit-testable with a settable clock, and why an expired pending credential is invisible *without* the sweep job having run (`TOTP 16`). Factor it into one predicate (`isExpiredPending :: TOTPSubsystemConfig -> UTCTime -> StoredTOTPCredential -> Bool`) and use that everywhere pending-expiry is decided — activation here, the cap count above, the status derivation in `TOTP 22` — so the semantics cannot drift between call sites;
- activated/pending filtering happens — `GetCredentials` returns everything;
- `maxCredentialsPerUser` is counted and enforced;
- a missing row is turned into `TOTPNotFound` (the store's writes against a missing id are silent no-ops).

The `clientTime` argument is accepted and ignored in this ticket; `TOTP 10` gives it meaning. Thread it through now.

### `otpauth://` URI

```
otpauth://totp/{issuer}:{account}?secret={base32}&issuer={issuer}&algorithm={SHA1|SHA256|SHA512}&digits={6..9}&period={seconds}
```

`issuer` from config (default: the backend domain), `account` the user's email or handle. Both percent-encoded. `algorithm`, `digits` and `period` come from the credential's own stored parameters, not from the config read at URI-build time — they are the same value today, and will not be after someone changes the config ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §8). The algorithm is spelled uppercase here, per the `otpauth://` de-facto spec, even though config and JSON use lowercase.

Document the exact format — client teams consume it and it is what the QR encodes ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §0).

## Dependency direction — do not get this wrong

This effect must **not** depend on `AuthenticationSubsystem`. Password re-authentication before enrolment happens one level up, in the handler (`TOTP 07`) or in `EnsureReauthenticated` (`TOTP 11`). Keeping the arrow pointing one way is what lets `TOTP` be interpreted below `AuthenticationSubsystem` in brig's effect stack, which `TOTP 06` relies on.

Interpreter effects: `TOTPStore`, `Random`, `Wire.Sem.Now`, `Input TOTPSubsystemConfig`, `Error TOTPError`, `TinyLog`. Note the absence of `RateLimit` — nothing here is guessable (an activation code can only confirm a secret the caller was handed at enrolment, so brute-forcing it gains nothing); it arrives with verification in `TOTP 09`.

## Acceptance criteria

- Unit tests in `libs/wire-subsystems/test/unit/Wire/AuthenticationSubsystem/TOTPSpec.hs` against the mock store and a **settable** `Now` (no `threadDelay`):
  - enrol produces a 20-byte secret; two enrolments produce different secrets;
  - the `otpauth://` URI parses back to the issued secret (round-trip against the helper from `TOTP 04`, not against our own parser);
  - account names containing `@` and `:` are percent-encoded correctly;
  - activation with the correct code flips `activated_at`;
  - activation with a wrong code leaves the credential pending and returns `TOTPInvalidCode`;
  - activating twice → `TOTPAlreadyActivated`;
  - activating past the TTL → `TOTPEnrolmentExpired`, **even though no sweep job ran**;
  - `maxCredentialsPerUser` is enforced; unexpired pending credentials count toward it, expired ones do not;
  - `ListCredentials` never returns a secret in any form;
  - `DeleteAllCredentials` clears everything for that user and nothing for anyone else.
- RFC 6238 Appendix B vectors asserted through *our* parameter construction (`mkTOTPParams` with our configured digits/period/algorithm), not just against `defaultTOTPParams`.

## Notes

- `TOTPError` → HTTP mapping lives in `TOTP/Error.hs` and reuses the `Wire.API.Error.Brig` constructors added in `TOTP 01`.
- `TOTPClientClockSkew` needs an error body carrying `server_time`; model it as a `RichError` (cf. `verificationCodeSubsystemErrorToHttpError` in `Wire/VerificationCodeSubsystem.hs`, which attaches headers the same way). The constructor can exist here unused; `TOTP 10` throws it.
