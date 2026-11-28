# TOTP 16 — sweeping expired pending enrolments and step claims

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 02
- TOTP 06
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) §3 "Pending-enrolment expiry", "No durable last-use record".

## Scope

A background job with two statements, both reclaiming rows that are already being ignored by the code that reads them.

```
-- enrolments the user never completed
DELETE FROM totp_credential
 WHERE activated_at IS NULL AND created_at < $1

-- replay claims that can no longer reject anything
DELETE FROM totp_step_claim WHERE expires_at <= $1
```

via `TOTPStore.DeletePendingCreatedBefore` and `TOTPStore.DeleteExpiredStepClaims`, scheduled through the existing job machinery (`Wire.JobSubsystem` / brig's `Wire.BackgroundJobsRunner`). Cutoffs **and the batch bound** are computed by the job (from `Now.get` and, for enrolments, `pendingEnrolmentTTL`, default 15 minutes) and passed in — the store knows nothing about TTLs or safe batch sizes ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D12, `TOTP 02`); the job loops on the returned row count until a run deletes fewer rows than the bound.

The two have different reasons to exist and different cadences. Pending enrolments are about **disk**: sweep every few minutes, or hourly, it does not matter. Step claims are about **privacy** ([spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D13): each one is a last-authenticated timestamp, and the interval between a claim expiring and the sweep removing it is the only window in which the database holds a fact we said it would not. Run that one often — a minute, not an hour — so the effective retention stays close to the ~2 minute `claimTTL` rather than being set by the job schedule.

## Neither statement is load-bearing for security

`TOTP 05` made expiry authoritative in the **subsystem**: activation compares `created_at` against `Now.get - pendingEnrolmentTTL` and refuses an older row, so an expired enrolment cannot be activated whether or not this job ever runs. That ordering is deliberate and must stay that way. A sweeper that is down, lagging, or misconfigured then costs disk and nothing else.

The same holds for step claims: `TOTP 02`'s compare-and-set treats an expired claim as absent (`expires_at <= $now` is in the `WHERE`), so a claim stops protecting — and stops blocking — at exactly its expiry, whether or not this job has deleted the row. The sweep removes data we have already stopped reading.

Do not "simplify" by removing either check once this job exists. The whole point is that correctness does not depend on a cron. Do not "simplify" in the other direction either, by dropping the sweep because the reads ignore the rows: for step claims the deletion is the privacy property, not housekeeping.

## Acceptance criteria

- The job runs on a schedule, deletes only expired *pending* rows, and never touches an activated credential — including one created long ago.
- Deleting in batches with a bound, so a large backlog cannot produce one enormous statement or a long lock.
- The claim sweep deletes only rows past `expires_at`, never a live claim — deleting a live one would reopen a replay window, which is the one way this job can actually cause harm.
- Metrics: a counter of rows reclaimed per run and per statement, so the enrolment-abandonment rate is observable and so a claim table that stops shrinking is visible.
- Logs at most a summary line per run, not per row — and **never** the user ids or the step values, which would move the data this job deletes into the log retention system instead.
- A unit test with a settable `Now`: rows at `ttl - 1s` survive, rows at `ttl + 1s` are removed, for both statements.
- An integration or unit test asserting the invariant directly: an expired pending credential **cannot be activated even before the sweeper runs**, and an expired claim **does not block a later code even before the sweeper runs**. These are the tests that protect the design above.
- A test that no `totp_step_claim` row older than `claimTTL` plus one sweep interval survives a run — the retention claim in [spec](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan) D13 should be asserted somewhere rather than only asserted in prose.

## Notes

- Single-tenant-per-backend, so no team scoping is needed.
- Multiple brig replicas will run this concurrently. `DELETE` is idempotent so this is harmless, but confirm the job framework does not log errors when two replicas race.
- If the job framework makes scheduling awkward, an acceptable fallback is to sweep opportunistically on enrolment (delete the caller's own expired pending rows). Less tidy, but it keeps the table bounded without new infrastructure — and the subsystem-side check means neither approach is load-bearing for correctness. The claim sweep has the same fallback: each verification can delete that credential's own expired claim as it goes, which bounds retention for anyone who is actually logging in.
- `totp_step_claim` is a small, hot, high-churn table: a row per recently-authenticating user, each one deleted within minutes. Check that autovacuum keeps up with it, since the dead-tuple rate is high relative to the live row count, and a table that is one row wide but bloated to gigabytes is a classic outcome.
- If the claim store moves to Redis (`TOTP 02`), the second statement and everything said about it disappear: the TTL is native and nothing has to be swept.
