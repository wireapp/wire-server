# Backup API Slice 6: Orphan Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expired upload reservations and orphans whose inline S3 deletion failed are eventually deleted. Cargohold exposes `POST /i/backups/cleanup` (one batch); a background-worker cron job calls it repeatedly.

**Architecture:** Vertical slice: wire-api internal route + `BackupCleanupResult` + Arbiter job payload → `BackupStore.SelectCleanupBatch` + `BackupSubsystem.CleanupOrphans` + `BackupConfig` fields → cargohold internal handler + config/helm → new `CargoholdAPIAccess` effect (Rpc) → background-worker options, env, effect stack, `backups` Arbiter queue with cron job `backups-cleanup` → unit + integration tests. One commit.

**Tech Stack:** Haskell, servant, polysemy, hasql / hasql-th, Arbiter (`Arbiter.Worker.Cron`), Bilge/`Wire.Rpc`, hspec, wire-api golden tests, `integration/` testsuite.

**Spec:** `docs/superpowers/specs/2026-11-24-backup-api-design.md` (Cleanup, background-worker; RFC `2026-11-16 RFC Backup API(1).md`: Orphan files, background-worker job)

**Depends on:** Slice 2 (effects, mocks, `runBackupEffects`, `BackupConfig`, `Test/Backups.hs` helpers), slice 3 (`backup_incrementals`, `CommitIncremental`, `BackupConfig` fields), slice 5 (`DeleteObjects`, `DeleteOrphanRows`, `MarkDeletionTried`, `deleteOrphansNow`, `MockOrphan` with `orphanedAt`/`deletionLastTried`, `DeleteBehaviour`).

## Global Constraints

- Eligible orphans: `kind = 'deletion' OR orphaned_at < now() - grace` (`kind` is the generated column from slice 2). Grace is measured with Postgres `now()`.
- `SelectCleanupBatch` is one short transaction: select ≤ batch size `ORDER BY deletion_last_tried NULLS FIRST, orphaned_at FOR UPDATE SKIP LOCKED`, set `deletion_last_tried = now()`, return ids. Concurrent runs may still pick the same rows after commit; that's harmless because S3 and row deletes are idempotent. No advisory lock: these rows belong to no epoch.
- Missing S3 objects count as gone (slice 5 `DeleteObjects` contract).
- `BackupConfig.orphanGracePeriod` (seconds, default 3600) must be `> uploadTimeout`, else the config parse fails, which fails cargohold startup. `cleanupBatchSize` default 100, must be ≥ 1.
- Reservation claim in `CommitFull` (and slice 3's `CommitIncremental`) becomes `DELETE FROM backup_orphans WHERE id = $new AND deletion_last_tried IS NULL`. Only `SelectCleanupBatch` sets that column on reservations, so a claim blocked on cleanup's row lock re-evaluates to 0 rows (`CommitReservationGone`) under ReadCommitted. This is clock-independent.
- Cargohold stays the only S3 client; background-worker only calls `POST /i/backups/cleanup`.
- HTTP client background-worker → cargohold: new `Wire.CargoholdAPIAccess` effect with a Bilge/`Wire.Rpc` interpreter (`rpcWithRetries "cargohold"`), the same pattern as `Wire.SparAPIAccess.Rpc`. It's added to `BackgroundWorkerEffects`. Slice 8 extends this effect with its delete calls; it doesn't add a second client.
- Background-worker option key is exactly `cargohold` (`Endpoint`). `integration/test/Testlib/ModService.hs` `updateServiceMapInConfig` rewrites keys by `serviceName`, so dynamic backends call their own cargohold. A different key would silently call the default backend.
- Cron job: per run, call cleanup while `deleted > 0`, at most `backupsCleanupMaxCalls = 20` calls. An Rpc error ends the run (logged); the next tick retries.
- Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`. Never run integration tests (compile only).
- Commit: type-checks, no warnings, unit tests of changed packages green, `make format`, `make regen-local-nix-derivations` if deps changed.

## Review Focus

1. **In-flight uploads must never be cleaned up** (fresh reservation, `file_created_at NULL`, younger than grace). → unit test `cleanupSkipsFreshReservation` (Task 2), integration test `testBackupCleanupExpiredReservation` (Task 5, `deleted == 0` before grace).
2. **Upload commit racing cleanup** (cleanup deletes the object, commit still claims the reservation → dangling row). Guarded by `grace > uploadTimeout` and by the claim requiring `deletion_last_tried IS NULL`. → unit tests `commitFailsAfterCleanupSelected`, `backupConfigGraceMustExceedTimeout` (Task 2).
3. **Permanently failing objects must not starve others:** never-tried rows are selected first. → unit test `cleanupPrefersNeverTried` (Task 2).
4. **Failed S3 deletes keep their row** and are retried later. → unit test `cleanupKeepsFailedRows` (Task 2).
5. **Cron run is bounded and stops when idle.** → unit test `cleanupLoopStops` (Task 4).

---

## Commit: cleanup endpoint + background-worker cron job

### Task 1: wire-api: result type, internal route, job payload

**Files:**
- Modify: `libs/wire-api/src/Wire/API/Backup.hs`
- Modify: `libs/wire-api/src/Wire/API/Routes/Internal/Cargohold.hs`
- Modify: `libs/wire-api/src/Wire/API/Jobs.hs`
- Test: `libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs`, `libs/wire-api/test/golden/Test/Wire/API/Golden/Manual.hs`, `.../Golden/Manual/AdminlessJobs.hs` (next to `testObject_MeetingsJobPayload_MeetingsCleanup_1`), new `libs/wire-api/test/golden/testObject_BackupsJobPayload_BackupsCleanup_1.json`

**Interfaces:**
- Produces (`Wire.API.Backup`): `newtype BackupCleanupResult = BackupCleanupResult { deleted :: Word32 }`. JSON `{"deleted": n}` via `schema-profunctor`; `Arbitrary`.
- Produces (`Wire.API.Routes.Internal.Cargohold`, inside the `"i"` prefix): `Named "i-backups-cleanup" ("backups" :> "cleanup" :> Post '[JSON] BackupCleanupResult)` (200).
- Produces (`Wire.API.Jobs`) (**new Arbiter queue, i.e. a new Arbiter table via `runJobMigrations`**):
  - `type BackupsQueueName = "backups"`, `backupsQueueName :: Text`
  - `data BackupsCleanupJob = BackupsCleanupJob` (schema `object (pure BackupsCleanupJob)`)
  - `data BackupsJobPayload = BackupsCleanup BackupsCleanupJob`, tag enum `BackupsJobPayloadTag = BackupsCleanupTag` (`"backups_cleanup"`), envelope via `taggedJobPayloadObjectSchema`, `makePrisms`, `Arbitrary`
  - `JobRegistry` gets `Queue BackupsQueueName BackupsJobPayload`

- [ ] **Step 1: Write failing tests:**
  - Roundtrip: `testRoundTrip @BackupCleanupResult`, `@Jobs.BackupsCleanupJob`, `@Jobs.BackupsJobPayload`.
  - Golden `testObject_BackupsJobPayload_BackupsCleanup_1 = BackupsCleanup BackupsCleanupJob` ↔ `{"data": {}, "type": "backups_cleanup"}`.
  - `toJSON (BackupCleanupResult 3) == object ["deleted" .= 3]`.
- [ ] **Step 2: Run** `make c package=wire-api test=1 | grep -vE '...'`. Expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** the same command. Expected: green. cargohold fails to compile until Task 3 (missing handler).

### Task 2: wire-subsystems: config, store, subsystem

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/BackupSubsystem.hs` (`BackupConfig`, new op), `.../BackupSubsystem/Interpreter.hs`
- Modify: `libs/wire-subsystems/src/Wire/BackupStore.hs`, `.../BackupStore/Postgres.hs`
- Modify: `libs/wire-subsystems/test/unit/Wire/MockInterpreters/BackupStore.hs`
- Test: `libs/wire-subsystems/test/unit/Wire/BackupSubsystem/InterpreterSpec.hs`

**Interfaces:**
- Produces (`BackupConfig`): `orphanGracePeriod :: NominalDiffTime` (JSON seconds, `.:? .!= 3600`), `cleanupBatchSize :: Word32` (`.:? .!= 100`). `FromJSON` is hand-written (replace a generic one if slice 2/3 used it) and fails with `parserThrowError [Key "orphanGracePeriod"] "orphanGracePeriod must be greater than uploadTimeout"` unless `orphanGracePeriod > uploadTimeout`, and fails if `cleanupBatchSize == 0`.
- Produces (`Wire.BackupStore`): `SelectCleanupBatch :: NominalDiffTime -> Word32 -> BackupStore m [BackupFileId]`. Postgres, one `runTransaction ReadCommitted Write`:
  ```sql
  UPDATE backup_orphans SET deletion_last_tried = now()
   WHERE id IN (SELECT id FROM backup_orphans
                 WHERE kind = 'deletion' OR orphaned_at < now() - $1 :: interval
                 ORDER BY deletion_last_tried NULLS FIRST, orphaned_at
                 LIMIT $2 :: int8 FOR UPDATE SKIP LOCKED)
  RETURNING id :: uuid
  ```
  (`$1` = `realToFrac grace :: DiffTime`.) Mock: same filter/order using `Now`, sets `deletionLastTried = Just now`.
- Changes (`Wire.BackupStore.Postgres`, no signature change): the reservation claim in `CommitFull` and `CommitIncremental` gets `AND deletion_last_tried IS NULL`. Mock: the claim fails if `deletionLastTried /= Nothing`.
- Produces (`Wire.BackupSubsystem`): `CleanupOrphans :: BackupSubsystem m Word32`. Interpreter: `SelectCleanupBatch cfg.orphanGracePeriod cfg.cleanupBatchSize` → `DeleteObjects` → `DeleteOrphanRows gone` → `length gone`; info-log the count.
- Refactor (`Interpreter`): extract `deleteGone :: (Member BackupStore r, Member BackupObjectStore r) => [BackupFileId] -> Sem r [BackupFileId]` (`DeleteObjects` + `DeleteOrphanRows`). `deleteOrphansNow` = `deleteGone` + `MarkDeletionTried` for the rest; `CleanupOrphans` = `deleteGone` (already marked by the select).

- [ ] **Step 1: Write failing tests:**
  - `backupConfigGraceMustExceedTimeout`: `eitherDecode` of a config with `uploadTimeout: 1800`: `orphanGracePeriod: 1800` → `Left`; `1801` → `Right`; omitted → `Right` with `orphanGracePeriod == 3600`, `cleanupBatchSize == 100`; `cleanupBatchSize: 0` → `Left`. (Use the minimal valid object for slices 2–3 fields.)
  - `cleanupSkipsFreshReservation`: `reserveUpload` (no commit), `cleanupOrphans` → `0`; reservation still in `orphans`.
  - `cleanupDeletesExpiredReservation`: reserve, store an object under its id, `passTime (grace + 1)`, `cleanupOrphans` → `1`; `orphans` and object store empty.
  - `cleanupDeletesFailedDeletions`: e1 twice with `DeleteFailsFor {firstFull}` (inline fails, row stays); switch the mock to succeed (a second run with `DeleteSucceeds` over the same state), `cleanupOrphans` → `1`, `orphans` empty. No grace wait needed (`file_created_at` set).
  - `cleanupKeepsFailedRows`: as above but still failing → `0`; row stays, `deletionLastTried == Just now`.
  - `cleanupPrefersNeverTried`: `cleanupBatchSize = 1`; orphan A with `deletionLastTried = Just t`, orphan B never tried, B orphaned after A → first `cleanupOrphans` deletes B.
  - `commitFailsAfterCleanupSelected`: `reserveUpload`, `passTime (grace + 1)`, `selectCleanupBatch` (marks it), `commitFull` → `Left CommitReservationGone`; same for `commitIncremental`.
  - `cleanupRespectsBatchSize`: 3 eligible, batch 2 → `2`, then `1`, then `0`.
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m Backup' | grep -vE '...'`. Expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** the same command. Expected: pass. Then `make c package=wire-subsystems test=1 | grep -vE '...'` green.

### Task 3: cargohold: internal handler + config

**Files:**
- Modify: `services/cargohold/src/CargoHold/API/Backup.hs` (`cleanupBackups :: Handler BackupCleanupResult` = `runBackupEffects (BackupCleanupResult <$> cleanupOrphans)`)
- Modify: `services/cargohold/src/CargoHold/API/Public.hs` (`internalSitemap :<|> Named @"i-backups-cleanup" cleanupBackups`)
- Modify: `services/cargohold/cargohold.integration.yaml` (`backups.orphanGracePeriod: 3600`, `backups.cleanupBatchSize: 100`)
- Modify: `charts/wire-server/values.yaml` (`cargohold.config.backups.orphanGracePeriod: 3600`, `cleanupBatchSize: 100`), `charts/wire-server/templates/cargohold/configmap.yaml` (render both if keys are listed individually)
- Modify: `docs/src/developer/reference/config-options.md` (cargohold `backups.orphanGracePeriod`, `backups.cleanupBatchSize`; background-worker `cargohold`, `backupsCleanup.schedule`)

- [ ] **Step 1: Implement** per Files.
- [ ] **Step 2: Run** `make c package=cargohold test=1 | grep -vE '...'`. Expected: builds, no warnings, tests green.

### Task 4: CargoholdAPIAccess + background-worker cron job

**Files:**
- Create: `libs/wire-subsystems/src/Wire/CargoholdAPIAccess.hs`, `libs/wire-subsystems/src/Wire/CargoholdAPIAccess/Rpc.hs`; register in `wire-subsystems.cabal`
- Modify: `services/background-worker/src/Wire/BackgroundWorker/Options.hs`
- Modify: `services/background-worker/src/Wire/BackgroundWorker/Env.hs` (`cargoholdEndpoint :: Endpoint` = `opts.cargohold`)
- Modify: `services/background-worker/src/Wire/Effects.hs` (`CargoholdAPIAccess` next to `SparAPIAccess` in `BackgroundWorkerEffects` and the interpreter chain: `interpretCargoholdAPIAccessToRpc env.cargoholdEndpoint`)
- Create: `services/background-worker/src/Wire/BackupsCleanupWorker.hs`; register in `background-worker.cabal`
- Modify: `services/background-worker/src/Wire/BackgroundWorker/Workers.hs`, `services/background-worker/src/Wire/BackgroundWorker.hs` (`Workers.startWorker opts.jobs opts.meetingsCleanup opts.backupsCleanup`)
- Modify: `services/background-worker/background-worker.integration.yaml` (`cargohold: {host: 127.0.0.1, port: 8084}`, `backupsCleanup: {schedule: "* * * * *"}`)
- Modify: `charts/wire-server/values.yaml` (`background-worker.config.cargohold: {host: cargohold, port: 8080}`, `backupsCleanup: {schedule: "*/5 * * * *"}`), `charts/wire-server/templates/background-worker/configmap.yaml` (render `cargohold` like `spar`, and `backupsCleanup` **unconditionally**, no `with`: both fields are required), `hack/helm_vars/wire-server/values.yaml.gotmpl` (`backupsCleanup: {schedule: "* * * * *"}` next to `meetingsCleanup`)
- Test: create `services/background-worker/test/Test/Wire/BackupsCleanupWorkerSpec.hs` (cabal `other-modules`); modify `services/background-worker/test/Test/Wire/Util.hs` (`testEnv`: `cargoholdEndpoint = Endpoint "localhost" 0`)

**Interfaces:**
- Produces (`Wire.CargoholdAPIAccess`, **new effect beyond the fixed list**):
  ```haskell
  data CargoholdAPIAccess m a where
    CleanupBackups :: CargoholdAPIAccess m BackupCleanupResult
  makeSem ''CargoholdAPIAccess
  ```
- Produces (`Wire.CargoholdAPIAccess.Rpc`): `interpretCargoholdAPIAccessToRpc :: (Member (Error ParseException) r, Member Rpc r) => Endpoint -> InterpreterFor CargoholdAPIAccess r`. `CleanupBackups` = `method POST . paths ["i", "backups", "cleanup"] . expect2xx`, decoded with a local `decodeBodyOrThrow "cargohold"` (as in `SparAPIAccess.Rpc`).
- Produces (`Options`): `Opts.cargohold :: Endpoint`, `Opts.backupsCleanup :: BackupsCleanupConfig`, `newtype BackupsCleanupConfig = BackupsCleanupConfig { schedule :: CronSchedule }`. The `FromJSON` parses with `parseCronSchedule` and uses `parserThrowError [Key "schedule"]` on failure (as `MeetingsCleanupConfig`).
- Produces (`Wire.BackupsCleanupWorker`):
  ```haskell
  backupsCleanupMaxCalls :: Int  -- 20
  cleanupBackupsLoop :: (Member CargoholdAPIAccess r) => Int -> Sem r (Word32, Int)  -- (total deleted, calls); stops after a 0 result or max calls
  runBackupsCleanup :: AppT IO ()  -- runBackgroundWorkerEffects env extEnv (RequestId "backups-cleanup") Nothing (cleanupBackupsLoop backupsCleanupMaxCalls); Left err → Log.err; Right → Log.info total + calls
  ```
- Produces (`Workers.hs`): `startWorker :: JobConfig -> MeetingsCleanupConfig -> BackupsCleanupConfig -> AppT IO CleanupAction`. `JobRunnerConfig` gets `jobRunnerBackupsCleanupSchedule :: CronSchedule`. Extract `mkCronJob :: Text -> CronSchedule -> payload -> IO (cron job)` from the meetings cron construction (`SkipOverlap`, `setNotVisibleUntil (Just scheduledFor)`, `setMaxAttempts (Just 3)`, `throwIO . userError` on a bad schedule) and use it for both `"meetings-cleanup"` and `"backups-cleanup"` (`BackupsCleanup BackupsCleanupJob`). Add a third pool, `backupsWorkerConfig'` (`transactionalWorkerConfig … :: WorkerConfig (WireArbiter JobRegistry) BackupsJobPayload`, `cronJobs = [backupsCronJob]`, handler `BackupsCleanup _ -> runAppT env runBackupsCleanup`). Add `backupsJobPayloadTypeName`, and add `backupsQueueName` to the logged `queue_names`.

- [ ] **Step 1: Write failing tests** (`BackupsCleanupWorkerSpec`, mock `CargoholdAPIAccess` popping from `State [Word32]`, `0` when empty):
  - `cleanupLoopStops`: `[5, 3, 0, 7]` → `(8, 3)`; `replicate 30 1` → `(20, 20)`; `[]` → `(0, 1)`.
  - `parses backupsCleanup`: `BackupsCleanupConfig` from `{"schedule": "*/5 * * * *"}` → `Right`; `{"schedule": "nope"}` → `Left`.
- [ ] **Step 2: Run** `make c package=background-worker test=1 | grep -vE '...'`. Expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces and Files.
- [ ] **Step 4: Run** `make c package=wire-subsystems test=1` and `make c package=background-worker test=1` (filtered). Expected: green, no warnings.

### Task 5: integration tests + commit

**Files:**
- Modify: `integration/test/API/Cargohold.hs`:
  - `postBackupsCleanup :: (HasCallStack, MakesValue domain) => domain -> App Response` (`baseRequest domain Cargohold Unversioned "/i/backups/cleanup"`, POST)
  - `uploadBackupFullStalled :: (HasCallStack, MakesValue user) => user -> String -> Int -> App Response` (Z-Client = path client, epoch; `Content-Length: 1024`, `HTTP.RequestBodyStream 1024` popper yields 512 bytes, sleeps 3 s, then ends)
- Modify: `integration/test/Test/Backups.hs`

- [ ] **Step 1: Write tests:**
  - `testBackupCleanupExpiredReservation`: `withModifiedBackend def { cargoholdCfg = setField "backups.uploadTimeout" (1 :: Int) >=> setField "backups.orphanGracePeriod" (4 :: Int), backgroundWorkerCfg = setField "backupsCleanup.schedule" "0 0 1 1 *" }` (cron effectively off). Pooled dynamic backends may hold leftovers, so first drain: call `postBackupsCleanup` until `deleted == 0`, then wait 5 s and drain again. Then `setupBackupUser` on that domain and run `uploadBackupFullStalled`, accepting status ∈ {400, 500} or an HTTP-client exception (cargohold may close the connection early); the counts are the real assertion. `postBackupsCleanup` → 200 `deleted == 0`; wait 5 s → `deleted == 1`; again → `deleted == 0`.
  - `testBackupCleanupEndpoint`: default backend `postBackupsCleanup` → 200, `deleted` is an integer ≥ 0.
- [ ] **Step 2: Run** `make c package=integration | grep -vE '...'`. Expected: compiles. Do not run; list the test names for the human.
- [ ] **Step 3: Quality + commit**: append to `changelog.d/2-features/backup-api` ("background-worker periodically cleans up orphaned backup files (`backupsCleanup.schedule`, needs `cargohold` endpoint)."). Run `make format` and `make regen-local-nix-derivations`; `git status` shows only expected nix changes. Unit tests of wire-api, wire-subsystems, cargohold and background-worker (filtered) green; then
  ```bash
  git add libs/wire-api libs/wire-subsystems services/cargohold services/background-worker charts hack docs changelog.d integration nix
  git commit -m "Backups: orphan cleanup endpoint and background-worker cron job"
  ```

## Unresolved questions

- None. Decided: `cleanupBatchSize` default 100 so a (slice 7 versioned) batch finishes well within bg-worker's 30 s HTTP timeout; no metrics for now; integration schedule `* * * * *`, helm `*/5 * * * *`.
