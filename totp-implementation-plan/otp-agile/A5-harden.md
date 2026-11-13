# A5 — harden

> **Disclaimer at the end of this step: none.** This is where the "staging only" language comes out of the route descriptions and the feature becomes defensible in front of a security reviewer.

**Demo:** nothing visible. That is the definition of this step — five changes, none of which alters a single response shape, status code or field name. The demo is a checklist going green.

## This is a bucket, not a sequence

The five items below are independent. They touch different code, can land in any order, in parallel, by different people, and none of them blocks another. Treat this file as a backlog, not a ticket.

What they have in common is that **no client has to do anything**. If any item here requires a client change, it has been designed wrong — stop and re-read the relevant section of `plan-otp.md`.

---

### 1. Secrets encrypted at rest — `plan-otp-03`, §3

AES-256-GCM, key from brig config, stored as `nonce ‖ ciphertext ‖ tag`, with the user id bound in as additional authenticated data so a ciphertext cannot be transplanted between rows. `key_id` identifies the key so rotation needs no schema change later.

A1 deliberately shipped the `key_id` column with the sentinel `"plaintext"`, so this is a **data migration over existing rows**, not a schema change: read every row, encrypt, write back, flip `key_id`. Do it in bounded batches.

This is the difference between "a database read is a full 2FA bypass" and "a database read plus a config read". Brig must fail at startup if TOTP is enabled without a key — a backend that silently falls back to plaintext is worse than one that does not start. And write the operational warning down where operators will see it: **the key is unrecoverable**; lose it and every enrolled credential is dead.

### 2. Replay protection — `plan-otp-09`, §3, §4 step 3

The `totp_step_claim` table, and `ClaimStepIfNewer` as a single `INSERT … ON CONFLICT DO UPDATE … WHERE step < EXCLUDED.step OR expires_at <= $5`. The second of the two deliberate exceptions to the dumb-store rule (D12): it is atomicity, not policy, and it must not be implemented as a read-then-write — that race *is* the replay attack.

The claim carries `expires_at = now + (skew + 1) × period` and is swept, so the database never holds a record of when a user last authenticated that is older than a couple of minutes. That is a privacy property, derived in §3, and it is the reason the claim lives in its own table rather than as a column on `totp_credential`. D13.

Make the in-memory mock interpreter reproduce both the compare-and-set **and** the expiry faithfully, or the replay tests pass against the mock and the bug ships.

### 3. Clock skew — `plan-otp-10`, §4

Three layers, and they are not alternatives:

- the symmetric verification window, with `skew` as a **total odd step count** in config (default 3); an even value is a config error and brig refuses to start rather than silently rounding;
- the `clientTime` check at enrolment and activation, rejecting with `409 totp-client-clock-skew` and a body carrying the server time, so a client can say "your device clock is four minutes fast" instead of the user discovering it later as a mystery "invalid code";
- the **learned per-credential `step_offset`**, pinned at activation over the wider `maxLearnedStepOffset` range and applied on every subsequent verification. This is the layer that actually fixes a persistently-skewed phone, without widening the window for anybody else.

The claimed step must be recorded in the credential's own offset-corrected numbering, or replay protection silently breaks for exactly the credentials that needed the offset — and no test catches it unless one is written for a skewed credential. Write that test.

### 4. Rate limiting — `plan-otp-20`, §4

Reuse the existing per-user bucket (`doRateLimited (RateLimitUser uid)`); no new bucket. **Charge only on failure.** A login that verifies a password and a TOTP code would otherwise consume two tokens of a burst-5 bucket, and a user who fat-fingers their code three times gets a confusing 429 on an otherwise correct password.

### 5. The pending-enrolment sweep — `plan-otp-16`, §3

A periodic bounded-batch `DELETE FROM totp_credential WHERE activated_at IS NULL AND created_at < $1`, plus the expired step claims from item 2, on the existing job machinery. Note that nothing depends on this for *security* — A1 already made expiry authoritative in the subsystem — only for disk. Which is exactly why it comes last and why it must not grow into the authoritative check.

---

## Done when

- All five items landed, each with the tests named in its ticket.
- **Honesty test:** `testA2CodeIsReplayableWithinWindow` is deleted and replaced by its opposite — the same code presented twice is rejected the second time, across the HTTP boundary, not only in a unit test.
- A test asserting that an existing plaintext row is migrated and still verifies afterwards. The migration is the part of item 1 most likely to be wrong and least likely to be exercised.
- Every route `Description` has had its caveat line removed. Grep for the words "mock", "not yet" and "staging" under `Wire/API/Routes/Public/Brig/TOTP.hs` and confirm there are no hits.
- A security review, with this file as the agenda. The disclaimer in the README — all three lines — stops being said.
- `changelog.d/` entries. These are the ones worth writing carefully; they are what an operator reads to decide whether to enable the feature.

## What is still missing for general availability

A5 makes the feature *defensible*. It does not make it *complete*. Before general availability you still need, from A7: recovery-code regeneration and status (`plan-otp-17`) so a user who burns their codes is not dependent on an operator, the admin reset path (`plan-otp-15`) for users who have lost everything, and the `docs/` page (`plan-otp-08`) including the ops runbook. Those are product and support obligations rather than security ones, which is why they are not here — but shipping without them means every lockout becomes a manual escalation.
