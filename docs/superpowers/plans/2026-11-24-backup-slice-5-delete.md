# Backup API Slice 5: Client Deletes Own Backups Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A client can delete all its backups (`DELETE /backups/clients/<cid>` → 204). Backups it deletes, and orphans created by full replacement or stale-epoch removal, are removed from S3 right away (best effort, unversioned path).

**Architecture:** Vertical slice: wire-api route → `BackupStore` (`OrphanClient`, `DeleteOrphanRows`, `MarkDeletionTried`, `CommitFull` returning orphaned ids) + `BackupObjectStore.DeleteObjects` → `BackupSubsystem.DeleteClientBackups` + shared `deleteOrphansNow` → cargohold handler + S3 `DeleteObjects` by key → unit + integration tests. One commit.

**Tech Stack:** Haskell, servant, polysemy, hasql / hasql-th / hasql-transaction, amazonka-s3 2.0, hspec (wire-subsystems), tasty (cargohold-tests), `integration/` testsuite.

**Spec:** `docs/superpowers/specs/2026-11-24-backup-api-design.md` (Cleanup, Concurrency, Errors; RFC `2026-11-16 RFC Backup API(1).md`: `DELETE /backups/clients/<ClientId>`, Orphan files, Deletion in batches)

**Depends on:** Slice 2 (`2026-11-24-backup-slice-2-full-round-trip.md`): `Wire.API.Backup`, `Wire.BackupStore`, `Wire.BackupObjectStore`, `Wire.BackupSubsystem(.Interpreter)`, mocks, `CargoHold.Backup.ObjectStore`, `CargoHold.API.Backup.runBackupEffects`, `integration/test/Test/Backups.hs`. Slice 3: `backup_incrementals` table, `CommitIncremental`. Slice 4: listings (not used here).

## Global Constraints

- Route `From 'V20`, `ZLocalUser :> ZClient`.
- Every mutating store transaction starts with `SELECT pg_advisory_xact_lock(hashtextextended($user::text || ':' || $client, 0))`, `ReadCommitted`, `Write`. Single-statement row deletes/updates by id (`DeleteOrphanRows`, `MarkDeletionTried`) take no lock.
- Orphans created by deletion carry `file_created_at` = the file's creation time (never `NULL`; `NULL` = upload reservation).
- `OrphanClient` touches only committed files (epochs + incrementals). In-flight reservations stay; their commits then fail the epoch re-check (`max` is gone) and cleanup (slice 6) collects them.
- Incrementals never orphan anything, so only `CommitFull`'s success result changes; `CommitIncremental` stays as is.
- `deleteOrphansNow` never throws on S3 failure: the cargohold `DeleteObjects` interpreter catches all synchronous exceptions per batch, logs them, and reports those ids as not gone. Postgres errors in `DeleteOrphanRows`/`MarkDeletionTried` propagate (500; Postgres is assumed reliable, so no extra handling). State stays consistent: rows stay, the cron retries, a retried DELETE returns 204.
- S3: `DeleteObjects` with `quiet = True`, ≤ 1000 keys per call, key `backups/<uuid>`. A missing key counts as gone (S3 reports success).
- Error bodies generic. Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`. Never run integration tests (compile only).
- Commit: type-checks, no warnings, unit tests of changed packages green, `make format`, `make regen-local-nix-derivations` if deps changed.

## Review Focus

1. **S3 delete fails during the request:** the request still succeeds, rows stay and get `deletion_last_tried`, other ids are cleaned up. → unit test `deleteObjectFailureMarksTried` (Task 2).
2. **Deleting one client's backups must not touch other clients of the same user:** → unit test `deleteKeepsOtherClients` (Task 2), integration test `testBackupDeleteOtherClientsUntouched` (Task 4).
3. **GDPR: deletion works with the feature disabled or without a team:** → unit test `deleteAllowedWhenFeatureOff` (Task 2), integration test `testBackupDeleteWhenDisabled` (Task 4).
4. **Upload racing a delete:** a commit after `OrphanClient` must re-check the epoch under the lock and fail, not recreate the epoch with a stale number. → unit test `commitAfterOrphanClientFails` (Task 2).
5. **Object really gone from S3 (garage), not just unreferenced:** a presigned URL obtained before deletion/replacement returns 404 afterwards. → integration tests `testBackupDeleteRemovesObject`, `testBackupReplaceRemovesOldObject` (Task 4).

---

## Commit: client deletes own backups + inline S3 deletion

### Task 1: wire-api route

**Files:**
- Modify: `libs/wire-api/src/Wire/API/Routes/Public/Cargohold.hs` (append to `BackupAPI`)

**Interfaces:**
- Produces: `Named "backup-delete-client"` (`From 'V20 :> ZLocalUser :> ZClient :> "backups" :> "clients" :> Capture "cid" ClientId :> MultiVerb 'DELETE '[JSON] '[RespondEmpty 204 "Backups deleted"] ()`), `CanThrow 'BackupForbidden`. Description: path client must equal `Z-Client`; feature flag not checked; succeeds if nothing to delete.

- [ ] **Step 1: Implement** the route.
- [ ] **Step 2: Run** `make c package=wire-api test=1 | grep -vE '...'`. Expected: green. cargohold fails to compile until Task 3 (missing handler).

### Task 2: wire-subsystems: store, object store, subsystem

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/BackupStore.hs`, `.../BackupStore/Postgres.hs`
- Modify: `libs/wire-subsystems/src/Wire/BackupObjectStore.hs`
- Modify: `libs/wire-subsystems/src/Wire/BackupSubsystem.hs`, `.../BackupSubsystem/Interpreter.hs`
- Modify: `libs/wire-subsystems/test/unit/Wire/MockInterpreters/BackupStore.hs`, `.../MockInterpreters/BackupObjectStore.hs`
- Test: `libs/wire-subsystems/test/unit/Wire/BackupSubsystem/InterpreterSpec.hs`

**Interfaces:**
- Produces (`Wire.BackupStore`):
  ```haskell
  OrphanClient :: UserId -> ClientId -> BackupStore m [BackupFileId]
  DeleteOrphanRows :: [BackupFileId] -> BackupStore m ()
  MarkDeletionTried :: [BackupFileId] -> BackupStore m ()
  -- changed (was Either CommitFullError BackupFile); list = ids orphaned by replacement / stale-epoch removal
  CommitFull :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> Word64 -> BackupStore m (Either CommitFullError (BackupFile, [BackupFileId]))
  ```
  Postgres:
  - `OrphanClient`: one transaction. Lock, then (a) move incrementals: `WITH moved AS (DELETE FROM backup_incrementals i USING backup_epochs e WHERE i.full_id = e.full_id AND e.user_id = $1 AND e.client_id = $2 RETURNING i.id, e.epoch_no, i.created_at) INSERT INTO backup_orphans (id, user_id, client_id, epoch_no, file_created_at) SELECT id, $1, $2, epoch_no, created_at FROM moved RETURNING id`. Then (b) move epochs the same way from `DELETE FROM backup_epochs WHERE user_id = $1 AND client_id = $2 RETURNING full_id, epoch_no, full_created_at`. Use two statements in this order: the FK is `ON DELETE RESTRICT`, so one statement with both CTEs can fail. Returns (a) ++ (b).
  - `DeleteOrphanRows`: `DELETE FROM backup_orphans WHERE id = ANY($1 :: uuid[])`. `MarkDeletionTried`: `UPDATE backup_orphans SET deletion_last_tried = now() WHERE id = ANY($1 :: uuid[])`. Both via `runStatement`. Empty list: no DB call.
  - `CommitFull`: add `RETURNING id` to the existing orphan-producing inserts (old full, its incrementals, stale epochs' fulls + incrementals) and return them.
- Produces (`Wire.BackupObjectStore`): `DeleteObjects :: [BackupFileId] -> BackupObjectStore m [BackupFileId]`. Returns the ids whose objects are gone; a missing object counts as gone. The interpreter never throws.
- Produces (`Wire.BackupSubsystem`): `DeleteClientBackups :: Local UserId -> ClientId -> ClientId -> BackupSubsystem m ()` (caller, Z-Client, path client).
- Produces (`Wire.BackupSubsystem.Interpreter`, exported):
  ```haskell
  deleteOrphansNow :: (Member BackupStore r, Member BackupObjectStore r, Member TinyLog r) => [BackupFileId] -> Sem r ()
  -- [] → no-op; gone <- DeleteObjects ids; DeleteOrphanRows gone; MarkDeletionTried (ids \\ gone); log counts (warn if any not gone)
  ```
  `interpretBackupSubsystem` gains `Member TinyLog r` (**interface change vs slice 2**).
  `DeleteClientBackups`: checks (1) path ≠ Z-Client → `Forbidden`; (2) Z-Client ∉ `LookupClients [uid]` → `Forbidden`; no team or flag check. Then `OrphanClient` → `deleteOrphansNow`.
  `UploadFull`: after a successful `CommitFull`, `deleteOrphansNow orphaned`, then return the file.
- Produces (mocks, **change vs slice 2**):
  - `BackupState.orphans :: Map BackupFileId MockOrphan` with `data MockOrphan = MockOrphan { fileCreatedAt :: Maybe UTCTime, orphanedAt :: UTCTime, deletionLastTried :: Maybe UTCTime }`. This replaces `Maybe UTCTime`; rewrite the existing slice 2/3 assertions on `orphans` in this commit. Slice 6 relies on all three fields.
  - `inMemoryBackupObjectStore` gets `DeleteBehaviour` as a second argument: `data DeleteBehaviour = DeleteSucceeds | DeleteFailsFor (Set BackupFileId)`. Failing ids stay in the map and are not returned. Existing call sites pass `DeleteSucceeds`.
  - `MockOrphan.orphanedAt` and `deletionLastTried` come from `Now`.
  - Test stacks discard logs (`Wire.Sem.Logger.TinyLog.discardTinyLogs`).

- [ ] **Step 1: Write failing tests** in `InterpreterSpec.hs`:
  - `deleteClientBackupsOrphansAndDeletes`: upload full e1, incremental e1, full e2; delete → `getEpochFiles` 1 and 2 → `Left BackupErrorNotFound`; object store empty; `orphans` empty.
  - `deleteForbiddenForOtherClient`: path ≠ Z-Client → `Left BackupErrorForbidden`; epochs unchanged.
  - `deleteForbiddenForRemovedClient`: Z-Client not in brig stub → `Left BackupErrorForbidden`.
  - `deleteAllowedWhenFeatureOff`: upload, then flag off, then delete → `Right ()` and objects gone; `deleteAllowedWithoutTeam`: teamless user, no backups → `Right ()`.
  - `deleteKeepsOtherClients`: clients A, B upload e1; A deletes → B's `getEpochFiles 1` ok, B's object present.
  - `deleteKeepsReservations`: `reserveUpload` for A (no commit), delete A → reservation still in `orphans` with `fileCreatedAt == Nothing`.
  - `deleteObjectFailureMarksTried`: `DeleteFailsFor {fullE1}` → `Right ()`; `orphans` = only `fullE1`, `deletionLastTried == Just now`; other objects gone.
  - `commitAfterOrphanClientFails`: e1 committed; `reserveUpload` e2; `orphanClient`; `commitFull` e2 → `Left CommitEpochInvalid`.
  - `replaceFullDeletesOldObjectInline`: upload e1 twice → first id not in object store, `orphans` empty.
  - `staleEpochDeletedInline`: e1, e2, e3 → e1's full + incrementals gone from store, `orphans` empty.
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m Backup' | grep -vE '...'`. Expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** the same command. Expected: all `Backup` specs pass. Then `make c package=wire-subsystems test=1 | grep -vE '...'` green.

### Task 3: cargohold: handler + S3 `DeleteObjects`

**Files:**
- Create: `services/cargohold/src/CargoHold/Backup/Deletion.hs` (pure, no amazonka types; slice 7 extends it)
- Modify: `services/cargohold/src/CargoHold/Backup/ObjectStore.hs` (interpret `DeleteObjects`; use `backupKey` from `Deletion`)
- Modify: `services/cargohold/src/CargoHold/API/Backup.hs` (handler `deleteClientBackups`), `services/cargohold/src/CargoHold/API/Public.hs` (route), `runBackupEffects` (`TinyLog` already in the stack; nothing else new)
- Modify: `services/cargohold/cargohold.cabal` (library module; test `other-modules`; `cargohold-tests` deps `containers`, `uuid`, `tasty-hunit` as used, because of `-Wunused-packages`)
- Test: create `services/cargohold/test/unit/Test/CargoHold/Backup/DeletionTest.hs`; register in `test/unit/Main.hs` and `cargohold-tests` `other-modules`

**Interfaces:**
- Produces (`CargoHold.Backup.Deletion`):
  ```haskell
  data ObjectTarget = ObjectTarget { key :: Text, versionId :: Maybe Text }  -- Eq, Ord, Show
  data DeletionPlan = DeletionPlan { targets :: Map BackupFileId [ObjectTarget], unlisted :: Set BackupFileId }
  backupKey :: BackupFileId -> Text                    -- "backups/<uuid>"
  maxDeleteBatch :: Int                                -- 1000
  keysOnlyPlan :: [BackupFileId] -> DeletionPlan       -- one versionless target per id, unlisted = ∅
  deleteBatches :: DeletionPlan -> [[ObjectTarget]]    -- all targets, chunks of ≤ maxDeleteBatch, none empty
  goneIds :: DeletionPlan -> Set ObjectTarget -> [BackupFileId]
  -- failed targets in; id gone iff none of its targets failed (empty target list = gone); unlisted never gone
  ```
- Produces (`CargoHold.Backup.ObjectStore`): `deleteBackupObjects :: AWS.Env -> [BackupFileId] -> IO [BackupFileId]`. Per batch: `newDeleteObjects bucket (newDelete & delete_objects .~ targets & delete_quiet ?~ True)` via `AWS.sendCatch` inside `tryAny`. Failed targets = `deleteObjectsResponse_errors` (`s3ServiceError_key`/`_versionId`), or the whole batch on exception/`Left`. Log failures (count + first error code) with `AWS.Env`'s logger. amazonka sets `Content-MD5` for `DeleteObjects`.
- Produces handler: `deleteClientBackups :: Local UserId -> ClientId -> ClientId -> Handler ()`.

- [ ] **Step 1: Write failing tests** (tasty, `DeletionTest.tests` group `"CargoHold.Backup.Deletion"`):
  - `backupKey` of `BackupFileId nil` == `"backups/00000000-0000-0000-0000-000000000000"`.
  - QuickCheck `batchesBounded`: for `n ∈ [0..2500]` ids, `deleteBatches (keysOnlyPlan ids)`: every batch length in `[1..1000]`, `concat` == the targets in order, `n == 2001` → 3 batches.
  - `goneIdsExcludesFailed`: ids `[a, b]`, failed `{target a}` → `[b]`; failed ∅ → `[a, b]`.
- [ ] **Step 2: Run** `make c package=cargohold test=1 | grep -vE '...'`. Expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** the same command. Expected: cargohold builds without warnings, `cargohold-tests` green.

### Task 4: integration tests + commit

**Files:**
- Modify: `integration/test/API/Cargohold.hs`: `deleteClientBackups :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response` (user, Z-Client, path client)
- Modify: `integration/test/Test/Backups.hs`

- [ ] **Step 1: Write tests** (slice 2 helpers `setupBackupUser`, `uploadBackupFull`, `getBackupEpoch`, `downloadBackupFull`; slice 3's incremental upload helper where noted):
  - `testBackupDeleteClient`: full e1 + one incremental, full e2; DELETE → 204; `getBackupEpoch` 1 and 2 → 404 `backup-not-found`; DELETE again → 204.
  - `testBackupDeleteRemovesObject`: upload e1, `downloadBackupFull` → 302 `Location`; DELETE → 204; GET `Location` without auth → 404.
  - `testBackupReplaceRemovesOldObject`: upload e1 → `Location` of first id; upload e1 again → 200; GET old `Location` → 404.
  - `testBackupDeleteForbiddenOtherClient`: path = second client of same user → 403 `backup-forbidden`.
  - `testBackupDeleteRemovedClient`: delete the client, then DELETE with its id as Z-Client and path → 403 `backup-forbidden`.
  - `testBackupDeleteWhenDisabled`: upload, disable `automaticBackups`, DELETE → 204; re-enable; `getBackupEpoch` 1 → 404.
  - `testBackupDeleteWithoutTeam`: personal user (`randomUser`) with a client → 204.
  - `testBackupDeleteOtherClientsUntouched`: clients A, B upload e1; A deletes own → B `getBackupEpoch` 1 → 200.
- [ ] **Step 2: Run** `make c package=integration | grep -vE '...'`. Expected: compiles. Do not run; list the test names for the human.
- [ ] **Step 3: Quality + commit**: append to `changelog.d/2-features/backup-api` ("Clients can delete their backups; deleted, replaced and stale backup files are removed from S3 immediately."). `make format`; `make c package=wire-api test=1`, `package=wire-subsystems test=1`, `package=cargohold test=1` (filtered) green; then
  ```bash
  git add libs/wire-api libs/wire-subsystems services/cargohold integration changelog.d
  git commit -m "Backups: client deletes own backups, inline S3 deletion"
  ```

## Unresolved questions

