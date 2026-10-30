# TOTP 13 — recovery code generation

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 02
- TOTP 05
- TOTP 07
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §3 (storage), §4 (rate limiting).

## Scope

Generate recovery codes at activation and return them exactly once. *Using* a recovery code to authenticate is `TOTP 14`; this ticket only produces and stores them.

The `totp_recovery_code` table already exists from `TOTP 02`, and `ActivateCredential` already returns `[RecoveryCode]` (currently `[]`) from `TOTP 05`. This ticket fills both in, so no API shape changes.

### Generation

- `recoveryCodeCount` codes (config, default 10) on activation of a user's **first** credential.
- Each code is 16 symbols of Crockford Base32 drawn from the `Random` effect — exactly 80 bits — rendered lowercase and dash-grouped (`xxxx-xxxx-xxxx-xxxx`). Crockford's decode-time normalisation is the input treatment: strip dashes, case-fold, map `o→0` and `i`/`l`→`1`, so a user typing from paper is not punished for formatting or for visually ambiguous characters. (Mind the arithmetic when touching this: 12 symbols of a 31-letter "no ambiguous characters" alphabet is only ~59 bits — a format example can quietly undershoot an entropy requirement.)
- Stored **hashed** with argon2id via `Wire.HashPassword`, never in the clear and never encrypted-reversible. They are password-equivalent: a recovery code alone plus a password gets you into the account.
- Returned exactly once, in the activation response. There is no endpoint that re-reads them.

### Activating a second credential

Activating an *additional* credential must not silently reissue codes and invalidate the ones the user already wrote down. Decide and document: the straightforward rule is "codes are generated once, on first activation; subsequent activations return an empty list", with regeneration available later via `TOTP 17`.

### Interaction with deletion

`TOTP 12` deletes recovery codes when the last activated credential is removed. If 12 has already landed, verify that path still holds now that codes actually exist.

## Acceptance criteria

- Exactly `recoveryCodeCount` codes are generated, all distinct.
- Codes are stored hashed — assert against the store that no row contains the plaintext.
- `show` on a `RecoveryCode` does not reveal it (the abstract-type requirement from `TOTP 01`).
- The activation response contains the codes; no other endpoint ever does.
- Activating a second credential follows the documented rule.
- Input normalisation: the same code with and without dashes, in any casing, and with `o`/`i`/`l` typed for `0`/`1`, all hash to the same thing.
- Entropy: a statistical smoke test that codes are not drawn from a tiny space (e.g. 1000 generated codes are all distinct).
- Deleting the user cascades to `totp_recovery_code`.

## Notes

- **Cost warning for the next ticket.** Verifying a recovery code is an argon2id check — ~100 ms, memory-hard, the same cost as a password verification, and with 10 unused codes a naive implementation does *ten* of them per attempt. `TOTP 14` must handle this; flag it there rather than discovering it in production.
- Shipping recovery codes in milestone 1.1 means the first release has a user-initiated recovery path and does not depend on operators for every lost phone. Management of those codes (regenerate, count remaining, notify on use) is `TOTP 17`.
- `changelog.d/` entry: activation now returns recovery codes.
