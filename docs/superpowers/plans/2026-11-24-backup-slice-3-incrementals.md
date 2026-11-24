# Backup API Slice 3: Incrementals Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A client can upload incrementals under its epoch's current full, list them with the epoch and download them via presigned redirect; `maxIncrementalBytes` / `maxFilesPerEpoch` are enforced; replacing a full or removing stale epochs orphans their incrementals.

**Architecture:** One vertical commit: wire-api errors/routes → migration `backup_incrementals` + `BackupStore` ops → `BackupSubsystem` ops → cargohold handlers/config → unit + integration tests. Builds on slice 2's effects, mocks and `runBackupEffects`.

**Tech Stack:** Haskell, servant, polysemy, hasql / hasql-th / hasql-transaction, conduit, hspec, `integration/` testsuite.

**Spec:** `docs/superpowers/specs/2026-11-24-backup-api-design.md` (RFC: `2026-11-16 RFC Backup API(1).md`)

**Depends on:** Slice 2 plan (`2026-11-24-backup-slice-2-full-round-trip.md`), both commits: `Wire.API.Backup`, `BackupStore` (`ReserveUpload`, `CommitFull`, `LookupFull`, `LookupEpochFiles`), `BackupObjectStore` (`PutObject`, `PresignGet`), `BackupSubsystem`, mocks, `CargoHold.API.Backup.runBackupEffects`, `BackupRedirect`, integration helpers `uploadBackupFull`, `getBackupEpoch`, `downloadBackupFull`, `setupBackupUser`.

## Global Constraints

- Routes `From 'V20`, prefix `/backups/clients/<cid>/epochs/<epoch>/full/<fullId>`, `ZLocalUser :> ZClient`.
- Every mutating store transaction: first statement `SELECT pg_advisory_xact_lock(hashtextextended($user::text || ':' || $client, 0))`; `ReadCommitted`, `Write`. Reads take no lock.
- FK `backup_incrementals.full_id → backup_epochs.full_id` is `RESTRICT` and not deferrable: within a transaction, move incrementals to orphans **before** any `UPDATE`/`DELETE` of their epoch row.
- `maxFilesPerEpoch` counts the full: an epoch holds the full + at most `maxFilesPerEpoch - 1` incrementals.
- Error labels new: `backup-full-missing` 409, `backup-epoch-full` 409. Bodies generic (no ids, no S3 keys).
- No nginz change (slice 2 location `^(/v[0-9]+)?/backups` covers the new paths).
- Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`. Integration tests: compile only.
- Commit: type-checks, no warnings, unit tests of changed packages green, `make format`, `make regen-local-nix-derivations` if deps changed.

## Review Focus

1. **Two concurrent incrementals at the limit** both pass the pre-check: only the commit-time count under the advisory lock stops the second. → integration test `testBackupConcurrentIncrementalsAtLimit` (Task 4; only test exercising the Postgres lock).
2. **Full replaced while an incremental uploads**: commit must return `CommitFullMissing` (409), never attach to the orphaned full. → unit test `commitIncrementalAfterReplacement` (Task 2).
3. **Replacement / stale-epoch removal must orphan incrementals** (else S3 leak; wrong statement order → FK violation → 500 on every replace). → unit tests `replaceFullOrphansIncrementals`, `staleEpochOrphansIncrementals` (Task 2); integration `testBackupReplaceFullWithIncrementals` (Task 4, real FK).
4. **Incremental download keyed by `(user, client, epoch, full, id)`**: wrong full or other user → 404. → integration tests `testBackupDownloadIncrementalWrongFull`, `testBackupDownloadIncrementalOtherUser` (Task 4).
5. **Limit boundaries**: full + `max - 1` incrementals ok, next rejected; `maxIncrementalBytes + 1` → 413. → unit tests `maxFilesPerEpochBoundary`, `incrementalTooLarge` (Task 2).

---

## Commit: incrementals

### Task 1: wire-api errors, routes

**Files:**
- Modify: `libs/wire-api/src/Wire/API/Error/Cargohold.hs` (constructors + `MapError`)
- Modify: `libs/wire-api/src/Wire/API/Routes/Public/Cargohold.hs` (`BackupAPI`)

**Interfaces:**
- Produces (`Wire.API.Error.Cargohold`): `BackupFullMissing` (409 `backup-full-missing` "Backup full not found or replaced"), `BackupEpochFull` (409 `backup-epoch-full` "Backup epoch has too many files").
- Produces (routes, slice 2 prefix `"backups" :> "clients" :> Capture "cid" ClientId :> "epochs" :> Capture "epoch" BackupEpoch`):
  - `Named "backup-upload-incremental"`: `:> "full" :> Capture "fullId" BackupFileId :> "incremental" :> Header' '[Required, Strict] "Content-Length" Word64 :> BackupBody :> Post '[JSON] BackupFile`; `CanThrow` forbidden, disabled, full-missing, epoch-full, too-large, `'InvalidLength`, `'IncompleteBody`.
  - `Named "backup-download-incremental"`: `:> "full" :> Capture "fullId" BackupFileId :> "incremental" :> Capture "incrementalId" BackupFileId :> ZHostOpt :> MultiVerb 'GET '[JSON] '[ErrorResponse 'BackupNotFound, BackupRedirect] URI` (`ZHostOpt` placed as in `backup-download-full`); `CanThrow` disabled, forbidden, not-found.
  - Both appended to `BackupAPI`.

- [ ] **Step 1: Implement** per Interfaces (no new types; existing roundtrip tests cover `BackupFile`).
- [ ] **Step 2: Run** `make c package=wire-api test=1 | grep -vE '...'` — expected: green; cargohold fails (missing handlers) until Task 3.

### Task 2: wire-subsystems — migration, store, subsystem, mocks

**Files:**
- Create: `libs/wire-subsystems/postgres-migrations/<ts>-backup-incrementals.sql` (via `hack/bin/new-migration.sh backup-incrementals`)
- Modify (from slice 2): `libs/wire-subsystems/src/Wire/BackupStore.hs`, `.../BackupStore/Postgres.hs`, `.../BackupSubsystem.hs`, `.../BackupSubsystem/Interpreter.hs`, `test/unit/Wire/MockInterpreters/BackupStore.hs`
- Test (from slice 2): `libs/wire-subsystems/test/unit/Wire/BackupSubsystem/InterpreterSpec.hs`

**Migration:** `backup_incrementals` exactly as RFC SQL, plus `CREATE INDEX backup_incrementals_full_id ON backup_incrementals (full_id);`.

**Interfaces:**
- Produces (`Wire.BackupStore`):
  ```haskell
  data CommitIncrementalError = CommitFullMissing | CommitEpochFull | CommitIncrementalReservationGone

  -- new constructors
  CommitIncremental :: Word32 {- maxFilesPerEpoch -} -> UserId -> ClientId -> BackupEpoch -> BackupFileId {- full -} -> BackupFileId {- new -} -> Word64 -> BackupStore m (Either CommitIncrementalError BackupFile)
  LookupIncremental :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupFileId -> BackupStore m Bool
  CountEpochFiles :: UserId -> ClientId -> BackupEpoch -> BackupStore m (Maybe Word32)  -- Nothing: no epoch; count incl. full
  ```
  - `CommitIncremental` tx order: lock → `SELECT full_id` of epoch; `Nothing` or `≠ full` → `CommitFullMissing` → `1 + count(incrementals of full) >= maxFilesPerEpoch` → `CommitEpochFull` → claim reservation (1 row, else `CommitIncrementalReservationGone`) → `INSERT backup_incrementals (id, full_id, size, created_at = now())`. Every `Left` → `Transaction.condemn`.
  - `CommitFull` (changed): replacement = lock → claim → move incrementals of old full to orphans (RFC CTE, `file_created_at = created_at`) → orphan old full → `UPDATE`. Stale removal (`e >= 3`): move incrementals of fulls of epochs `<= staleEpochs e` to orphans → `DELETE ... RETURNING` those epochs into orphans.
  - `LookupEpochFiles` (changed): incrementals of the current full, `ORDER BY created_at, id`.
  - `LookupIncremental`: `EXISTS` over `backup_incrementals JOIN backup_epochs USING (full_id)` filtered by all five keys.
- Store interpreters stay config-free (brig also runs `interpretBackupStoreToPostgres`, slice 8): the limit is passed per call as `CommitIncremental`'s first argument (plain `Word32`, so the store doesn't import `Wire.BackupSubsystem`).
- **Changed (mock state):** `BackupState` gains `incrementals :: Map BackupFileId (BackupFileId {- full -}, BackupFile)`.
- Produces (`Wire.BackupSubsystem`):
  ```haskell
  data BackupConfig = BackupConfig { maxFullBytes :: Word64, maxIncrementalBytes :: Word64, maxFilesPerEpoch :: Word32, uploadTimeout :: NominalDiffTime }
  data BackupError = ... | BackupErrorFullMissing | BackupErrorEpochFull
  -- args: caller, Z-Client, path client, epoch, full id, Content-Length, body
  UploadIncremental :: Local UserId -> ClientId -> ClientId -> BackupEpoch -> BackupFileId -> Word64 -> ConduitT () ByteString (ResourceT IO) () -> BackupSubsystem m BackupFile
  -- args: caller, Z-Client, host, path client, epoch, full id, incremental id
  DownloadIncremental :: Local UserId -> ClientId -> Maybe Domain -> ClientId -> BackupEpoch -> BackupFileId -> BackupFileId -> BackupSubsystem m URI
  ```
  - `UploadIncremental` order: slice 2 checks (1)–(3); length `0` → `InvalidLength`, `> maxIncrementalBytes` → `TooLarge`; `LookupFull` `False` → `FullMissing`; `CountEpochFiles` `Just n | n >= maxFilesPerEpoch` → `EpochFull`; then reserve, put, deadline, `CommitIncremental` (`CommitFullMissing` → `FullMissing`, `CommitEpochFull` → `EpochFull`, `CommitIncrementalReservationGone` → `UploadFailed`).
  - `DownloadIncremental`: checks (2), (3); `LookupIncremental` `False` → `NotFound`; else `PresignGet host incId`.

- [ ] **Step 1: Write failing tests** (unit config: `maxIncrementalBytes = 10`, `maxFilesPerEpoch = 3`; `passTime 1` between uploads):
  - `uploadIncrementalListed`: full + 2 incrementals → `getEpochFiles` `incrementals` ids in upload order, sizes match; object store holds bytes.
  - `incrementalToReplacedFull`: full A, full B (same epoch), incremental under A → `Left BackupErrorFullMissing`; no new orphan with `Nothing` (pre-check, no reservation).
  - `incrementalToStaleEpochFull`: epochs 1, 2, 3; incremental under epoch 1's full → `Left BackupErrorFullMissing`.
  - `incrementalUnknownFull`: random full id → `Left BackupErrorFullMissing`.
  - `commitIncrementalAfterReplacement` (store-level): `ReserveUpload inc`, `ReserveUpload B`, `CommitFull … B` (replaces A), `CommitIncremental … A inc` → `Left CommitFullMissing`; `inc` still in `orphans` with `Nothing`.
  - `maxFilesPerEpochBoundary`: full + 2 incrementals ok; 3rd → `Left BackupErrorEpochFull`; store-level: `ReserveUpload inc`, `CommitIncremental` at count 3 → `Left CommitEpochFull`, `inc` still reserved.
  - `incrementalTooLarge`: length 11 → `Left BackupErrorTooLarge`; 10 ok; 0 → `Left BackupErrorInvalidLength`.
  - `incrementalForbiddenOtherClient`: path client ≠ Z-Client → `Left BackupErrorForbidden`.
  - `replaceFullOrphansIncrementals`: full A + 2 incs, full B → `orphans` contain A and both inc ids, each `Just createdAt`; `getEpochFiles` `incrementals == []`; `incrementals` state empty.
  - `staleEpochOrphansIncrementals`: epoch 1 full + inc, epochs 2, 3 → epoch 1's full and inc in `orphans`.
  - `downloadIncremental`: URI ends with inc id; wrong full id → `Left BackupErrorNotFound`; other user → `Left BackupErrorNotFound`.
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m Backup' | grep -vE '...'` — expected: compile failure.
- [ ] **Step 3: Implement** migration, store (Postgres + mock), subsystem per Interfaces. Update slice 2 call sites: test `BackupConfig` gets the two new fields; `UploadIncremental` passes `cfg.maxFilesPerEpoch` to `CommitIncremental`.
- [ ] **Step 4: Run** same — expected: pass; `make c package=wire-subsystems test=1 | grep -vE '...'` green.

### Task 3: cargohold wiring, config

**Files:**
- Modify (from slice 2): `services/cargohold/src/CargoHold/API/Backup.hs` (handlers `uploadIncremental`, `downloadIncremental`; map `BackupErrorFullMissing`/`BackupErrorEpochFull` via `errorToWai`)
- Modify: `services/cargohold/src/CargoHold/API/Public.hs` (handlers into `backupAPI`)
- Modify: `services/cargohold/cargohold.integration.yaml` (`backups.maxIncrementalBytes: 65536`, `backups.maxFilesPerEpoch: 3`)
- Modify: `charts/wire-server/values.yaml` (`maxIncrementalBytes: 104857600`, `maxFilesPerEpoch: 1000`), `charts/wire-server/templates/cargohold/configmap.yaml` (if fields are rendered individually)
- Modify: `docs/src/developer/reference/config-options.md`, `changelog.d/2-features/backup-api`

- [ ] **Step 1: Implement** per Files.
- [ ] **Step 2: Run** `make c package=cargohold test=1 | grep -vE '...'` — expected: builds, no warnings, unit tests green.

### Task 4: integration tests + commit

**Files:**
- Modify: `integration/test/API/Cargohold.hs`:
  - `uploadBackupIncremental :: (HasCallStack, MakesValue user) => user -> String -> String -> Int -> String -> ByteString -> App Response` (user, Z-Client, path client, epoch, full id, bytes; headers as `uploadBackupFull`)
  - `downloadBackupIncremental :: (HasCallStack, MakesValue user) => user -> String -> String -> Int -> String -> String -> App Response` (… full id, inc id; `noRedirect`)
- Modify (from slice 2): `integration/test/Test/Backups.hs`

- [ ] **Step 1: Write tests:**
  - `testBackupUploadIncrementalAndList`: full 1024, incs 100 and 200 → 200 each; `getBackupEpoch` `incrementals` = `[id1, id2]` in order, sizes `[100, 200]`.
  - `testBackupIncrementalStaleFull`: replace full, inc under old full → 409 `backup-full-missing`; random full id → 409 `backup-full-missing`.
  - `testBackupEpochFull`: full + 2 incs → 200; 3rd → 409 `backup-epoch-full`.
  - `testBackupConcurrentIncrementalsAtLimit`: full + 1 inc; 2 incs via `mapConcurrently` (`UnliftIO.Async`) → statuses sorted `[200, 409]`, the 409 labelled `backup-epoch-full`; `getBackupEpoch` has 2 incrementals.
  - `testBackupIncrementalTooLarge`: 65537 bytes → 413 `backup-too-large`.
  - `testBackupIncrementalForbiddenOtherClient`: second client as path client → 403 `backup-forbidden`.
  - `testBackupReplaceFullWithIncrementals`: full + 2 incs, new full → 200; `getBackupEpoch` `incrementals == []`; old inc download → 404 `backup-not-found`.
  - `testBackupDownloadIncremental`: 302, `Location` GET without auth → 200, body equals uploaded bytes.
  - `testBackupDownloadIncrementalWrongFull`: inc id with epoch 2's full id → 404 `backup-not-found`.
  - `testBackupDownloadIncrementalOtherUser`: user B → 404 `backup-not-found`.
- [ ] **Step 2: Run** `make c package=integration | grep -vE '...'` — expected: compiles. Do not run; list new test names for the human.
- [ ] **Step 3: Quality + commit**: `make format`; `make c package=wire-api test=1`, `package=wire-subsystems test=1`, `package=cargohold test=1` (filtered) green;
  ```bash
  git add libs/wire-api libs/wire-subsystems services/cargohold charts docs changelog.d integration
  git commit -m "Backups: upload and download incrementals"
  ```

## Unresolved questions

- None. Decided: reject `maxFilesPerEpoch < 1` at parse; missing epoch on incremental → `backup-full-missing` (RFC).
