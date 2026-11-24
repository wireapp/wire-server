# Backup API Slice 2: Full Backup Round Trip Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A team client with `automaticBackups` enabled can upload the full file of an epoch, list the epoch's files and download the full via a presigned S3 redirect.

**Architecture:** Tracer bullet through all layers: wire-api types/routes/errors → `BackupStore` (Postgres) + `BackupObjectStore` (S3, interpreted in cargohold) + `BackupSubsystem` (rules) in wire-subsystems → cargohold handlers running a Polysemy stack from `Handler` → config (cargohold yaml, helm, nginz) → unit + integration tests. Two commits, each vertical: (A) upload full + list epoch, (B) download full.

**Tech Stack:** Haskell, servant, polysemy, hasql / hasql-th / hasql-transaction, amazonka-s3 2.0 + amazonka-s3-streaming, conduit, hspec/QuickCheck, `integration/` testsuite.

**Spec:** `docs/superpowers/specs/2026-11-24-backup-api-design.md` (RFC: `2026-11-16 RFC Backup API(1).md`)

**Depends on:** Slice 1 plan (`2026-11-24-backup-slice-1-feature-flag.md`): `AutomaticBackupsConfig` in `Wire.API.Team.Feature`, galley get/put/internal routes, integration helpers for the flag.

## Global Constraints

- Routes `From 'V20`, prefix `/backups`.
- File ids: UUID v4 (`Wire.Sem.Random.uuid`), S3 key `backups/<uuid>` in the existing bucket.
- Postgres: `ClientId` stored as `text`; every mutating store transaction starts with `SELECT pg_advisory_xact_lock(hashtextextended($user::text || ':' || $client, 0))` (per-client key; exactly one lock per transaction; per-user operations loop over clients, one transaction each), isolation `ReadCommitted`, mode `Write`.
- Users without team: 403 `backup-disabled`.
- Error labels: `backup-forbidden` 403, `backup-disabled` 403, `backup-epoch-invalid` 409, `backup-too-large` 413, `backup-not-found` 404; reuse `invalid-length` 400, `incomplete-body` 400.
- Error bodies generic: no S3 keys, no internal ids, no exception text.
- Cargohold does not run migrations.
- Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`. Never run integration tests (compile only); the human runs them.
- Each commit: type-checks, no warnings, unit tests of changed packages green, `make format`, `make regen-local-nix-derivations` when deps changed.
- Prefer record dot syntax over lenses for simple cases.

## Review Focus

1. **Upload stalls/aborts mid-stream** (client disconnects): reservation orphan row stays (`file_created_at NULL`), no epoch change, 400/500 to the client. → unit test `putFailureKeepsReservation` (Task 2).
2. **Upload finishes after `uploadTimeout`** (slow client): must not commit, since cleanup may have deleted the object. → unit test `deadlineExceededDoesNotCommit` (Task 2).
3. **Another user reading my backups by guessing ids:** every query is keyed by caller user id → 404, not 403 (no existence oracle). → integration test `testBackupOtherUserCannotRead` (Task 4), `testBackupDownloadOtherUser` (Task 5).
4. **Epoch rule boundary at `maxBound` (4294967295)**: `max + 1` must not overflow to 0/1. → QuickCheck property `epochAcceptedNoOverflow` (Task 1).
5. **Unfinished multipart upload after timeout/disconnect**: must be aborted, else S3 parts leak. → covered by the verification note in Task 3 (`CargoHold/Backup/ObjectStore.hs`); manual check against garage.
6. **Replaced full downloaded via stale id**: 404 `backup-not-found`, never the new file. → integration test `testBackupDownloadReplacedFull` (Task 5).

---

## Commit A: upload full + list epoch

### Task 1: wire-api types, errors, routes

**Files:**
- Create: `libs/wire-api/src/Wire/API/Backup.hs`
- Modify: `libs/wire-api/src/Wire/API/Error/Cargohold.hs` (add constructors + `MapError`)
- Modify: `libs/wire-api/src/Wire/API/Routes/Public/Cargohold.hs` (add `BackupAPI`, append to `CargoholdAPI`)
- Modify: `libs/wire-api/wire-api.cabal`
- Test: `libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs`, new `libs/wire-api/test/unit/Test/Wire/API/Backup.hs` (register in `Main`/cabal like neighbours)

**Interfaces:**
- Produces (`Wire.API.Backup`):
  - `newtype BackupEpoch = BackupEpoch Word32` — `FromHttpApiData` rejects `0`; JSON integer; `ToParamSchema`, `ToSchema`, `Arbitrary` (≥ 1).
  - `newtype BackupFileId = BackupFileId UUID` — `FromHttpApiData`, `ToHttpApiData`, JSON string, `ToSchema`, `ToByteString`, `Arbitrary`.
  - `data BackupFile = BackupFile { id :: BackupFileId, size :: Word64, createdAt :: UTCTimeMillis }` — JSON `{"id","size","createdAt"}`.
  - `data BackupFiles = BackupFiles { full :: BackupFile, incrementals :: [BackupFile] }` — JSON `{"full","incrementals"}`.
  - `newtype BackupSource = BackupSource (ConduitT () ByteString (ResourceT IO) ())` deriving `FromSourceIO ByteString`, `ToSourceIO ByteString` (as `AssetSource`).
  - `type BackupBody = StreamBody' '[Description "Raw backup bytes"] NoFraming OctetStream BackupSource`
  - `epochAccepted :: Maybe BackupEpoch -> BackupEpoch -> Bool` — `Nothing` accepts only `1`; `Just m` accepts `m` and `m + 1` (no overflow: at `maxBound` only `m`).
  - `staleEpochs :: BackupEpoch -> BackupEpoch` — upper bound `e - 2` of epochs removed when `e` is created (callers delete `<= bound` only when `e >= 3`).
- Produces (`Wire.API.Error.Cargohold`): constructors `BackupForbidden` (403 `backup-forbidden` "Backups of other clients cannot be modified"), `BackupDisabled` (403 `backup-disabled` "Backups are not enabled"), `BackupEpochInvalid` (409 `backup-epoch-invalid` "Invalid backup epoch"), `BackupTooLarge` (413 `backup-too-large` "Backup file too large"), `BackupNotFound` (404 `backup-not-found` "Backup not found").
- Produces (routes, all `From 'V20 :> ZLocalUser :> ZClient`, path prefix `"backups" :> "clients" :> Capture "cid" ClientId :> "epochs" :> Capture "epoch" BackupEpoch`):
  - `Named "backup-upload-full"`: `:> "full" :> Header' '[Required, Strict] "Content-Length" Word64 :> BackupBody :> Post '[JSON] BackupFile`, `CanThrow` forbidden, disabled, epoch-invalid, too-large, `'InvalidLength`, `'IncompleteBody`.
  - `Named "backup-get-epoch"`: `:> Get '[JSON] BackupFiles`, `CanThrow` forbidden, disabled, not-found.
  - `type BackupAPI = <the two above>`; `CargoholdAPI` gets `:<|> BackupAPI` (Commit B appends download).

- [ ] **Step 1: Write failing tests** — `Test.Wire.API.Backup`:
  - `"BackupEpoch rejects 0"`: `parseUrlPiece @BackupEpoch "0"` is `Left`; `"1"`, `"4294967295"` parse; `"4294967296"` is `Left`.
  - `epochAccepted` table (RFC): `epochAccepted Nothing 1`, `not (epochAccepted Nothing 2)`; for `n`: `Just n` accepts `n`, `n+1`, rejects `n-1`, `n+2`.
  - QuickCheck `epochAcceptedNoOverflow`: `not (epochAccepted (Just maxBound) (BackupEpoch 1))` and `epochAccepted (Just maxBound) maxBound`.
  - JSON golden-ish: `toJSON (BackupFile ...)` equals the RFC example object (`"id"`, `"size": 1048576`, `"createdAt": "2026-11-12T10:15:30.123Z"`).
  - Add `BackupFile`, `BackupFiles`, `BackupEpoch`, `BackupFileId` to `Roundtrip/Aeson.hs`.
- [ ] **Step 2: Run** `make c package=wire-api test=1 | grep -vE '...'` — expected: compile failure (module missing).
- [ ] **Step 3: Implement** the module, errors and routes per Interfaces. Use `schema-profunctor` objects like other wire-api types.
- [ ] **Step 4: Run** `make c package=wire-api test=1 | grep -vE '...'` — expected: all wire-api tests pass; cargohold fails to compile (missing handlers) until Task 3 — expected.

### Task 2: wire-subsystems — migration, BackupStore, BackupObjectStore, BackupSubsystem

**Files:**
- Create: `libs/wire-subsystems/postgres-migrations/<ts>-backups.sql` (via `hack/bin/new-migration.sh backups`)
- Create: `libs/wire-subsystems/src/Wire/BackupStore.hs`, `.../BackupStore/Postgres.hs`
- Create: `libs/wire-subsystems/src/Wire/BackupObjectStore.hs`
- Create: `libs/wire-subsystems/src/Wire/BackupSubsystem.hs`, `.../BackupSubsystem/Interpreter.hs`
- Create: `libs/wire-subsystems/test/unit/Wire/MockInterpreters/BackupStore.hs`, `.../MockInterpreters/BackupObjectStore.hs`
- Test: `libs/wire-subsystems/test/unit/Wire/BackupSubsystem/InterpreterSpec.hs`
- Modify: `libs/wire-subsystems/wire-subsystems.cabal` (modules; `conduit`, `resourcet` deps if missing), `test/unit/Wire/MockInterpreters.hs` (re-export)

**Migration** (only what this slice uses; `backup_incrementals` comes in slice 3): `backup_epochs` and `backup_orphans` exactly as in the RFC SQL, plus on `backup_orphans` the generated column `kind text GENERATED ALWAYS AS (CASE WHEN file_created_at IS NULL THEN 'upload' ELSE 'deletion' END) STORED` (never written by the app), plus `CREATE INDEX backup_orphans_orphaned_at ON backup_orphans (orphaned_at);`.

**Interfaces:**
- Produces (`Wire.BackupStore`):
  ```haskell
  data CommitFullError = CommitEpochInvalid | CommitReservationGone

  data BackupStore m a where
    ReserveUpload :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m ()
    -- one tx: advisory lock; re-check epochAccepted against current max;
    -- claim reservation (DELETE orphan, must hit 1 row); if epoch exists: orphan old
    -- full (file_created_at = old full_created_at) and update the row, else insert;
    -- if e >= 3: orphan fulls of epochs <= staleEpochs e and delete those epoch rows.
    CommitFull :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> Word64 -> BackupStore m (Either CommitFullError BackupFile)
    LookupMaxEpoch :: UserId -> ClientId -> BackupStore m (Maybe BackupEpoch)
    LookupEpochFiles :: UserId -> ClientId -> BackupEpoch -> BackupStore m (Maybe BackupFiles)
  makeSem ''BackupStore
  ```
  `ReserveUpload` inserts an orphan with `file_created_at NULL`, `orphaned_at = now()`.
- Produces (`Wire.BackupStore.Postgres`): `interpretBackupStoreToPostgres :: (PGConstraints r) => InterpreterFor BackupStore r` — hasql-th statements; `CommitFull` via `runTransaction ReadCommitted Write`; reads via `runStatement`. `CommitFullError` → `Transaction.condemn` + `Left`.
- Produces (`Wire.BackupObjectStore`):
  ```haskell
  data PutObjectError = PutIncompleteBody | PutTimedOut | PutFailed
  data BackupObjectStore m a where
    PutObject :: BackupFileId -> Word64 -> ConduitT () ByteString (ResourceT IO) () -> BackupObjectStore m (Either PutObjectError ())
  makeSem ''BackupObjectStore
  ```
- Produces (`Wire.BackupSubsystem`):
  ```haskell
  data BackupConfig = BackupConfig { maxFullBytes :: Word64, uploadTimeout :: NominalDiffTime }  -- FromJSON; uploadTimeout as seconds
  data BackupError = BackupErrorForbidden | BackupErrorDisabled | BackupErrorEpochInvalid | BackupErrorNotFound
                   | BackupErrorTooLarge | BackupErrorInvalidLength | BackupErrorIncompleteBody | BackupErrorUploadFailed
  data BackupSubsystem m a where
    -- args: caller, calling client (Z-Client), path client, epoch, Content-Length, body
    UploadFull :: Local UserId -> ClientId -> ClientId -> BackupEpoch -> Word64 -> ConduitT () ByteString (ResourceT IO) () -> BackupSubsystem m BackupFile
    GetEpochFiles :: Local UserId -> ClientId -> ClientId -> BackupEpoch -> BackupSubsystem m BackupFiles
  makeSem ''BackupSubsystem
  ```
- Produces (`Wire.BackupSubsystem.Interpreter`):
  ```haskell
  interpretBackupSubsystem ::
    ( Member BackupStore r, Member BackupObjectStore r, Member BrigAPIAccess r, Member GalleyAPIAccess r,
      Member Now r, Member Random r, Member (Input BackupConfig) r, Member (Error BackupError) r ) =>
    InterpreterFor BackupSubsystem r
  ```
  Order in `UploadFull`: (1) path client ≠ Z-Client → `Forbidden`; (2) Z-Client ∉ `LookupClients [uid]` → `Forbidden`; (3) `GetTeamId` `Nothing` or `GetFeatureConfigForTeam @AutomaticBackupsConfig` status ≠ enabled → `Disabled`; (4) length `0` → `InvalidLength`, `> maxFullBytes` → `TooLarge`; (5) pre-check `epochAccepted <$> LookupMaxEpoch` → `EpochInvalid`; (6) `start <- Now.get`, `fid <- BackupFileId <$> Random.uuid`, `ReserveUpload`; (7) `PutObject` — `PutIncompleteBody` → `IncompleteBody`, others → `UploadFailed`; (8) if `Now.get > start + uploadTimeout` → `UploadFailed`; (9) `CommitFull` — `CommitEpochInvalid` → `EpochInvalid`, `CommitReservationGone` → `UploadFailed`.
  `GetEpochFiles`: checks (2) and (3) only (any client of the caller is readable); `Nothing` → `NotFound`.
- Produces (mocks): `inMemoryBackupStore :: (Member (State BackupState) r, Member Now r) => InterpreterFor BackupStore r` with `data BackupState = BackupState { epochs :: Map (UserId, ClientId, BackupEpoch) (UTCTime, BackupFile), orphans :: Map BackupFileId (Maybe UTCTime) }` (value = `file_created_at`); `inMemoryBackupObjectStore :: (Member (State (Map BackupFileId ByteString)) r, Member MockNow r, Member (Embed IO) r) => PutBehaviour -> InterpreterFor BackupObjectStore r` with `data PutBehaviour = PutSucceeds | PutFailsWith PutObjectError | PutTakes NominalDiffTime` (`PutTakes` calls `passTime`).

- [ ] **Step 1: Write failing tests** in `InterpreterSpec.hs` (run stack with `runM`, mocks above, `miniGalleyAPIAccess` with a team whose `automaticBackups` is enabled, a `BrigAPIAccess` stub answering `LookupClients` from a fixed `Map UserId (Set ClientId)`):
  - `uploadFullStoresEpoch`: upload epoch 1 → `BackupFile` with `size == 5`; `getEpochFiles` → `full.id` equal, `incrementals == []`; object store holds the bytes; `orphans` empty.
  - `forbiddenForOtherClient`: path client ≠ Z-Client → `Left BackupErrorForbidden`, no orphans.
  - `forbiddenForRemovedClient`: Z-Client not in brig stub → `Left BackupErrorForbidden`.
  - `disabledWithoutTeam`, `disabledWhenFeatureOff` → `Left BackupErrorDisabled`.
  - `tooLarge`: length `maxFullBytes + 1` → `Left BackupErrorTooLarge`; length 0 → `Left BackupErrorInvalidLength`.
  - `epochRules`: `{}`: 2 → `EpochInvalid`; 1 ok; 1 again ok and `orphans` contains the first full id with `Just createdAt`; 2 ok; 3 ok → epoch 1 gone (`getEpochFiles 1` → `NotFound`), its full id in `orphans`; 5 → `EpochInvalid`.
  - `putFailureKeepsReservation`: `PutFailsWith PutIncompleteBody` → `Left BackupErrorIncompleteBody`; `orphans` has the reserved id with `Nothing`; no epoch.
  - `deadlineExceededDoesNotCommit`: `PutTakes (uploadTimeout + 1)` → `Left BackupErrorUploadFailed`; no epoch; reservation still in `orphans`.
  - `readOtherClientOfSameUser`: client B of same user reads client A's epoch → ok.
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m Backup' | grep -vE '...'` — expected: compile failure.
- [ ] **Step 3: Implement** migration, effects, Postgres interpreter, subsystem interpreter and mocks per Interfaces. SQL for the replace path: RFC "Replacing a full in SQL" minus the incrementals CTE; lock statement replaces its `FOR UPDATE`.
- [ ] **Step 4: Run** the same command — expected: all `Backup` specs pass; then `make c package=wire-subsystems test=1 | grep -vE '...'` green.

### Task 3: cargohold wiring

**Files:**
- Modify: `services/cargohold/cargohold.cabal` (deps: `polysemy`, `polysemy-wire-zoo`, `wire-subsystems`, `hasql-pool`/`extended` as needed)
- Modify: `services/cargohold/src/CargoHold/Options.hs` (`Opts`: `postgresql :: !(Map Text Text)`, `postgresqlPassword :: !(Maybe FilePathSecrets)`, `postgresqlPool :: !PoolConfig`, `galley :: !Endpoint`, `backups :: !BackupConfig`)
- Modify: `services/cargohold/src/CargoHold/App.hs` (`Env.hasqlPool :: HasqlPoolExt.Pool`, init via `initPostgresPool opts.postgresqlPool opts.postgresql opts.postgresqlPassword` as in `Brig/App.hs:318`)
- Modify: `services/cargohold/src/CargoHold/S3.hs` (extract the streaming pipeline of `uploadV3` into `streamUploadObject :: AWS.Env -> Text -> Word64 -> ConduitT () ByteString (ResourceT IO) () -> IO (Either PutObjectError ())`; `uploadV3` uses it, logging and error mapping unchanged)
- Create: `services/cargohold/src/CargoHold/Backup/ObjectStore.hs` — `interpretBackupObjectStoreS3 :: (Member (Embed IO) r) => AWS.Env -> NominalDiffTime -> InterpreterFor BackupObjectStore r`; key `"backups/" <> UUID.toText uuid`; wraps `streamUploadObject` in `System.Timeout.timeout` → `PutTimedOut`. Verify amazonka-s3-streaming aborts the multipart upload on async exceptions (timeout, client disconnect); if not, catch and call `AbortMultipartUpload`, otherwise unfinished multipart parts leak (invisible to `DeleteObjects` and cleanup).
- Create: `services/cargohold/src/CargoHold/API/Backup.hs` — handlers `uploadFull`, `getEpochFiles`; `runBackupEffects :: Sem BackupEffects a -> Handler a` interpreting (outermost last): `BackupSubsystem`, `BackupStore` (Postgres), `BackupObjectStore` (S3), `BrigAPIAccess` (`interpretBrigAccess opts.brig`), `GalleyAPIAccess` (`interpretGalleyAPIAccessToRpc (foldMap expandVersionExp opts.settings.disabledAPIVersions) opts.galley`), `Rpc` (`runRpcWithHttp env.httpManager env.requestId`), `Now` (`nowToIO`), `Random` (`randomToIO`), `Input BackupConfig`, `Input Pool`, `TinyLog` (`loggerToTinyLogReqId`), errors, `Embed IO`. `BackupError` → `errorToWai @'Backup…` / existing `invalidLength`, `incompleteBody`, `serverError`; every other error (`UsageError`, `RpcException`, `ParseException`, `ClientError`) → log + `serverError`.
- Modify: `services/cargohold/src/CargoHold/API/Public.hs` (`servantSitemap` gets `:<|> backupAPI`)
- Modify: `services/cargohold/cargohold.integration.yaml` (`postgresql` + `postgresqlPool` copied from `brig.integration.yaml:22-32`, `galley: {host: 127.0.0.1, port: 8085}`, `backups: {maxFullBytes: 1048576, uploadTimeout: 2700}`)
- Modify: `integration/test/Testlib/ModService.hs:257-259` (`cargoholdCfg = setField "postgresql.dbname" resource.berPostgresqlDBName`)
- Modify: `charts/wire-server/templates/cargohold/configmap.yaml`, `.../cargohold/deployment.yaml`, `.../cargohold/secret.yaml` (pg password mount like galley: `/etc/wire/cargohold/secrets/pgPassword`), `charts/wire-server/values.yaml` (`cargohold.config.postgresql`, `postgresqlPool`, `backups: {maxFullBytes: 1073741824, uploadTimeout: 2700}`)
- Modify: `charts/nginz/values.yaml` (cargohold upstream `- path: /backups`, `envs: [all]`, `max_body_size: "0"`, `disable_request_buffering: true`), `services/nginz/integration-test/conf/nginz/nginx.conf` (`location ~* ^(/v[0-9]+)?/backups { include common_response_with_zauth.conf; proxy_pass http://cargohold; }`). Skip `deploy/dockerephemeral/federation-v*` (old-version backends never serve backups).
- Modify: `docs/src/developer/reference/config-options.md` (cargohold `postgresql*`, `galley`, `backups`), `changelog.d/2-features/backup-api`, `changelog.d/0-release-notes/WPB-25648-cargohold-postgres` (upgrade note: cargohold now requires `postgresql`, `postgresqlPool`, optional `postgresqlPassword` and a `galley` endpoint in its helm values)

**Interfaces:**
- Consumes: Task 1 routes/errors, Task 2 effects and interpreters.
- Produces: `runBackupEffects` (Commit B and later slices add effects/handlers here).

- [ ] **Step 1: Implement** per Files (wiring; behaviour is covered by Task 2 unit tests and Task 4 integration tests).
- [ ] **Step 2: Run** `make c package=cargohold | grep -vE '...'` — expected: builds, no warnings. Then `make regen-local-nix-derivations` and `git status` shows only the nix files of changed cabal deps.
- [ ] **Step 3: Run** `make c package=cargohold test=1 | grep -vE '...'` — expected: existing unit tests green.

### Task 4: integration tests (upload + list)

**Files:**
- Modify: `integration/test/API/Cargohold.hs` — `uploadBackupFull :: (HasCallStack, MakesValue user) => user -> String -> String -> Int -> ByteString -> App Response` (user, Z-Client, path client, epoch, bytes; sets `zUser`, `zClient`, `Content-Type: application/octet-stream`, explicit `Content-Length`), `getBackupEpoch :: (HasCallStack, MakesValue user) => user -> String -> String -> Int -> App Response`
- Create: `integration/test/Test/Backups.hs`, register in `integration/integration.cabal`
- Local helper in `Test/Backups.hs`: `setupBackupUser :: App (Value, String)` — team owner (`createTeam`), unlock + enable `automaticBackups` (`setTeamFeatureLockStatus`, `setTeamFeatureStatus`), `addClient` → `(owner, clientId)`.

- [ ] **Step 1: Write tests:**
  - `testBackupUploadFullAndList`: upload 1024 bytes to epoch 1 → 200, `size == 1024`; `getBackupEpoch` → 200, `full.id` equals upload `id`, `incrementals == []`.
  - `testBackupEpochRules`: epoch 2 first → 409 `backup-epoch-invalid`; 1 → 200; 1 again → 200 with new id; 2 → 200; 3 → 200; `getBackupEpoch` 1 → 404 `backup-not-found`; 2 → 200.
  - `testBackupForbiddenOtherClient`: second client of same user as path client → 403 `backup-forbidden`.
  - `testBackupRemovedClient`: delete client, upload with its id → 403 `backup-forbidden`.
  - `testBackupDisabled`: feature off → 403 `backup-disabled`; personal user (`randomUser` without team) → 403 `backup-disabled`.
  - `testBackupTooLarge`: 1048577 bytes → 413 `backup-too-large`.
  - `testBackupOtherUserCannotRead`: user B `getBackupEpoch` on A's client id → 404 `backup-not-found`.
- [ ] **Step 2: Run** `make c package=integration | grep -vE '...'` — expected: compiles. Do not run; list the new test names for the human.
- [ ] **Step 3: Quality + commit**: `make format`; `make c package=wire-api test=1`, `package=wire-subsystems test=1`, `package=cargohold` (filtered) green; then
  ```bash
  git add libs/wire-api libs/wire-subsystems services/cargohold services/nginz charts docs changelog.d integration nix
  git commit -m "Backups: upload full backup and list epoch files"
  ```

## Commit B: download full

### Task 5: download route end to end

**Files:**
- Modify: `libs/wire-api/src/Wire/API/Routes/Public/Cargohold.hs` — `Named "backup-download-full"`: prefix as above `:> "full" :> Capture "fullId" BackupFileId :> ZHostOpt :> MultiVerb 'GET '[JSON] '[ErrorResponse 'BackupNotFound, BackupRedirect] URI` where `BackupRedirect` = 302 with `Location` header (model on `AssetRedirect`, `Public/Cargohold.hs:82-86`; non-`Maybe` result), `CanThrow` disabled, forbidden, not-found. Note: `ZHostOpt` position must match the asset routes' convention.
- Modify: `Wire.BackupStore` — `LookupFull :: UserId -> ClientId -> BackupEpoch -> BackupFileId -> BackupStore m Bool` (+ Postgres, mock)
- Modify: `Wire.BackupObjectStore` — `PresignGet :: Maybe Domain -> BackupFileId -> BackupObjectStore m URI` (+ mock returning `https://s3.example/backups/<id>`)
- Modify: `Wire.BackupSubsystem` — `DownloadFull :: Local UserId -> ClientId -> Maybe Domain -> ClientId -> BackupEpoch -> BackupFileId -> BackupSubsystem m URI` (checks (2), (3); `False` → `NotFound`)
- Modify: `services/cargohold/src/CargoHold/S3.hs` — extract presigning of `signedURL` into `presignGetObject :: AWS.Env -> Word -> Text -> IO (Either String URI)` (key, TTL `settings.downloadLinkTTL`), `signedURL` uses it; `awsEnvForHost` logic reused for host selection.
- Modify: `CargoHold/Backup/ObjectStore.hs` (interpreter gets the multi-ingress map + TTL), `CargoHold/API/Backup.hs`, `CargoHold/API/Public.hs`
- Test: `InterpreterSpec.hs`, `integration/test/Test/Backups.hs`, `integration/test/API/Cargohold.hs` (`downloadBackupFull :: (HasCallStack, MakesValue user) => user -> String -> String -> Int -> String -> App Response`, uses `noRedirect`)

- [ ] **Step 1: Write failing unit tests:** `downloadFullReturnsPresignedUrl` (URI ends with the full id); `downloadReplacedFullNotFound` (old id after replacement → `Left BackupErrorNotFound`); `downloadOtherUserNotFound`.
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m Backup' | grep -vE '...'` — expected: compile failure.
- [ ] **Step 3: Implement** per Files.
- [ ] **Step 4: Run** same — expected: pass. `make c package=cargohold | grep -vE '...'` — no warnings.
- [ ] **Step 5: Integration tests:** `testBackupDownloadFull` (302, `Location` present; GET it without auth → 200, body equals uploaded bytes); `testBackupDownloadReplacedFull` (old id → 404 `backup-not-found`); `testBackupDownloadOtherUser` (404). `make c package=integration | grep -vE '...'` compiles.
- [ ] **Step 6: Quality + commit**: `make format`; unit tests of wire-api, wire-subsystems, cargohold green;
  ```bash
  git add libs services/cargohold integration
  git commit -m "Backups: download full backup via presigned redirect"
  ```

## Unresolved questions

- None blocking. Defaults (`maxFullBytes` 1 GiB, `uploadTimeout` 45 min) adjustable later.
