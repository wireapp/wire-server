# TOTP 02 — Postgres schema and `TOTPStore`

```
assignedTo: Matthias Fischmann
blockedBy:
- TOTP 01-api-types.md
links:
- https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan
```

**Context:** [specs §3](https://wearezeta.atlassian.net/wiki/spaces/AIS/pages/3364651049/TOTP+RFC+6238+support+in+wire-server+Implementation+plan)

## Scope

The persistence layer: migration, store effect, Postgres interpreter, in-memory mock interpreter, store-level tests.  No business logic — the store is a dumb wrapper around the database.

### Migration

`libs/wire-subsystems/postgres-migrations/<timestamp>-create-totp-tables.sql`, creating `totp_credential`, its partial index `totp_credential_pending_idx`, `totp_step_claim` with its expiry index, and `totp_recovery_code`. Exact DDL in spec §3. Note `step_offset smallint NOT NULL DEFAULT 0` — it is unused until `TOTP 10`, but adding the column now avoids a second migration.

`totp_credential` and `totp_recovery_code` reference `wire_user(id) ON DELETE CASCADE`; `totp_step_claim` references `totp_credential (user_id, id) ON DELETE CASCADE`, so deleting a credential takes its replay state with it and deleting a user takes everything.

**Replay state is a separate table on purpose** (spec §3, D13). A claimed step is a Unix timestamp in disguise — `step × period` is when the user last authenticated — so it is short-lived data that must be able to *disappear* rather than sit in a nullable column on the credential row forever. Keeping it in its own table makes the deletion a row vanishing, and keeps `totp_credential` structurally free of behavioural data.

### Store effect

`libs/wire-subsystems/src/Wire/TOTPStore.hs` — the GADT from spec §3, with `makeSem`. `StoredTOTPCredential` (the row type, with the secret still encrypted) lives here or in a sibling module, as do `StoredRecoveryCode` and `RecoveryCodeId` (a new `Id` phantom tag, store-internal); none of them are wire-api types.

### Postgres interpreter

`libs/wire-subsystems/src/Wire/TOTPStore/Postgres.hs`, following `Wire/UserGroupStore/Postgres.hs` for style and for the hasql plumbing.

Two operations need care — the two compare-and-set writes. They return `Bool` and look like logic; they are not. They are **atomicity**, which is the one thing the subsystem cannot do for itself: a read-then-write up there loses the race between two concurrent requests presenting the same code, and that race *is* the replay attack. Each must be a single statement with the condition in the `WHERE` clause, returning the affected row count, no explicit transaction; exact statements in spec §3. Two caveats there worth repeating:

- Check the row count of the `INSERT … ON CONFLICT`: when the `WHERE` fails it updates nothing and reports zero, which is the replay signal.
- Both timestamps are **parameters**, never `now()` in SQL — the store does not own the clock (and the replay tests need a settable one).

The *policy* (what a non-increasing step means, how long a claim is kept) stays in the subsystem.

Everything else is plain: `GetCredentials` returns **all** rows for the user, activated or not, and the subsystem filters (a user has at most `maxCredentialsPerUser`, default 5, so there is nothing to optimise).  `DeletePendingCreatedBefore` and `DeleteExpiredStepClaims` take a cutoff `UTCTime` **and a batch bound**, both computed by their caller (`TOTP 16` deletes in bounded batches) — the store knows neither TTLs nor safe batch sizes. In Postgres a bounded `DELETE` needs the `WHERE ctid IN (SELECT ctid … LIMIT $n)` shape; the row count returned is what lets the caller loop until done.

### Mock interpreter

`libs/wire-subsystems/test/unit/Wire/MockInterpreters/TOTPStore.hs`, in-memory over `Polysemy.State`, following the existing `MockInterpreters` pattern. It must reproduce both compare-and-set semantics faithfully, or the replay tests in later tickets will pass against the mock and fail in production.

## Acceptance criteria

- `cabal build wire-subsystems` clean; migration applies to a fresh DB and to one with existing data.
- `postgres-schema.sql` regenerated.
- Store tests (against the real Postgres interpreter where the harness allows, otherwise against the mock with a separate smoke test):
  - insert → get roundtrip preserves every column including `key_id`;
  - `GetCredentials` returns pending **and** activated rows — the store does not filter;
  - `ClaimStepIfNewer` returns `True` for a strictly greater step, `False` for an equal or smaller one, and `True` when no claim row exists yet;
  - `ClaimStepIfNewer` returns `True` for a *smaller* step when the existing claim has expired — the expiry branch is the one that is easy to get wrong and impossible to notice, since the only symptom is replay protection quietly weakening;
  - two concurrent `ClaimStepIfNewer` calls for the same step: exactly one wins;
  - `MarkRecoveryCodeUsed` is single-shot, and single-shot under concurrency;
  - `DeleteExpiredStepClaims` removes claims at or past the cutoff and leaves live ones; deleting a credential cascades its claim row away;
  - `DeletePendingCreatedBefore` removes pending rows older than the given cutoff and leaves activated ones untouched regardless of age;
  - both sweep operations respect their batch bound and report the number of rows actually deleted;
  - `SetCredentialActivated` / `SetCredentialName` against a non-existent id are no-ops and do not throw (the subsystem checked existence already);
  - deleting the `wire_user` row cascades to both tables.
- Mock and Postgres interpreters agree on every one of the above (a shared test body parameterised over the interpreter is the cheapest way to guarantee it).
- Cassanrda migration compatibility tests: check if all affected tables are already fully migrated, and if not make sure the cassandra case also works.

## Notes

- Recovery-code rows store an argon2id `Password`, not the code. This ticket only stores what it is given; generation and hashing belong to `TOTP 13`.
- `DeleteAllCredentials` is needed by account deletion and by admin reset; wire the account-deletion call site in `TOTP 15`, not here.
- There is no `last_used_at` column anywhere (spec D13), and the replay claim is not exposed through any API, internal or public. If a reviewer asks why the claim is not simply a column on `totp_credential`, the answer is in spec §3: it must expire, and a table it can be deleted from says that in the schema.
- If anything in this ticket starts to look like a decision rather than a write, it belongs in `TOTP 05` or `TOTP 09` instead. Move it there rather than implementing it here.
