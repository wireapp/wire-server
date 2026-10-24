# A1 — it remembers

> **Disclaimer in force:** codes are not checked. Activation accepts **any** well-formed 6-digit string, including `000000`. TOTP secrets are stored **unencrypted**. Do not enrol an authenticator entry you rely on.

**Demo:** enrol an authenticator in the client on your phone, open the client on your laptop, and see it in the list. Rename it; the new name is on both. Delete it; it is gone from both. Enrol six and the sixth is refused.

This is the step where the feature stops being a screenshot and starts being a thing. Everything a settings screen does, except the one bit that needs cryptography.

## Why storage comes before crypto

Because "plausible" is a storage property, not a crypto property. A client team cannot test a settings screen against randomness — they cannot test the empty state, the one-credential state, the at-the-limit state, the pending-then-activated transition, or any error path, because the server's answer does not depend on anything they did. Every one of those becomes testable the moment the data is real, and none of them needs a single HMAC.

Faking the crypto for one more step is nearly free. Faking the storage is what was costing us.

## Scope

Real, as specified in `otp-tickets/plan-otp.md`:

- **The Postgres tables and the migration** — `totp_credential` only. Not `totp_step_claim` (that is replay protection, A5) and not `totp_recovery_code` (A4). `plan-otp-02` and §3.
- **`Wire.TOTPStore` + the Postgres interpreter**, to the dumb-store rule in §3/D12: storage only, no policy, no TTL arithmetic, no activated/pending filtering. Omit `ClaimStepIfNewer` and the recovery-code operations; nothing calls them yet.
- **`Wire.AuthenticationSubsystem.TOTP`**, the effect and interpreter, with the real logic for: enrol (generate a secret, store pending, build the `otpauth://` URI), activate (pending → activated), list, rename, delete, and the `maxCredentialsPerUser` cap. `plan-otp-05`.
- **Pending-enrolment expiry**, subsystem-side: a pending credential older than the TTL reads as absent and yields `totp-enrolment-expired` (§3, layer 1). The *sweeper* is A5; this layer is the one that is authoritative for correctness, and it is four lines.
- **Config** — the `totp` section in `Brig.Options` arrives here, because now there is something to configure: `issuer`, `algorithm`, `period`, `digits`, `maxCredentialsPerUser`, `pendingEnrolmentTTL`. Not `secretEncryptionKeys` (A5), not `skew` / `maxLearnedStepOffset` / `maxClientClockSkew` (A5), not `recoveryCodeCount` (A4). Mirror into `charts/brig/values.yaml`, the configmap template, `hack/helm_vars/`, and `brig.integration.yaml`. `plan-otp-06`, partial.
- **Events** — `user.totp-add` / `-update` / `-remove` to self, per `plan-otp.md` §5 "Events" and D17. They belong here and not later: the multi-device demo above *is* the event working. `Added` fires on activation, not enrolment.
- **Real errors.** The constructors A0 declared start being thrown: `totp-not-found`, `totp-already-activated`, `totp-enrolment-expired`, `totp-too-many-credentials`.
- **`DELETE /self/totp/:id`** becomes real. It checks the password only — see below.

Still fake:

- **Activation accepts any 6-digit code.** No `crypton`, no HMAC, no `step_offset`. The code is parsed, validated for shape, and discarded.
- **Secrets are stored in plaintext** in the `secret` column. Keep the `key_id` column in the migration from day one with a sentinel value (`"plaintext"`), so A5 is a data migration over existing rows rather than a schema change.
- **`DELETE` requires only a password**, not a second factor. Deleting a second factor ought to require one (`plan-otp-12`), but there is nothing to require yet; A3 fixes it.
- No rate limiting, no replay protection, no clock-skew handling.

## The thing to get right

**`activatedAt` is the only state that matters, and it is a one-way door.** Pending → activated, never back, never twice. `totp-already-activated` on the second attempt. Clients build their flow around this transition and will discover any sloppiness in it immediately — which is the point of doing it now.

The second thing: **the secret is returned exactly once**, at enrolment. `GET /self/totp` returns `TOTPCredentialInfo`, which has no secret field at all. That is enforced by the type, which is why `plan-mock-api.md` insisted on separate types for the two responses. Do not add a "convenience" endpoint to fetch it again.

## Done when

- The multi-device demo above works, through nginz, against staging.
- Integration tests: enrol → list shows it pending → activate → list shows it activated; activate twice → 409; activate someone else's credential id → **404, not 403** (no existence oracle); enrol past the cap → 409; pending credential past its TTL → 410; rename and delete reflected in the list; the enrolment secret never appears in any listing response.
- Unit tests for expiry against a settable `Wire.Sem.Now` mock. No `threadDelay`.
- A test asserting the events fire to self, with `Added` on activation and not on enrolment.
- **The honesty test** (see README): an integration test named so that it is obviously a weakness — `testA1ActivationAcceptsAnyCode` — asserting that `000000` activates a credential. A2 deletes it and replaces it with the opposite assertion.
- `postgres-schema.sql` regenerated. `changelog.d/` entry.
- The route `Description`s updated: drop "returns random data", add "codes are not yet verified".
