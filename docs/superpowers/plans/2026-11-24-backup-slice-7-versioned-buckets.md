# Backup API Slice 7: Versioned Buckets Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Backup deletion removes **all versions and delete markers** on versioned (or suspended) buckets, so deleted backups can't be restored (GDPR). Unversioned buckets keep the cheap keys-only path. Stores without `ListObjectVersions` fall back to plain exact keys (`backups/<id>`).

**Architecture:** Only the cargohold interpretation of `BackupObjectStore.DeleteObjects` changes: the pure decision logic goes in `CargoHold.Backup.Deletion` (unit-tested in `cargohold-tests`), and the S3 calls go in `CargoHold.Backup.ObjectStore`. There are no wire-api, store, subsystem or config changes. One commit.

**Tech Stack:** Haskell, amazonka-s3 2.0 (`GetBucketVersioning`, `ListObjectVersions`, `DeleteObjects`), unliftio (`pooledMapConcurrentlyN`), tasty / tasty-quickcheck (`cargohold-tests`).

**Spec:** `docs/superpowers/specs/2026-11-24-backup-api-design.md` (Cleanup → Versioning; RFC `2026-11-16 RFC Backup API(1).md`: Deletion and versioned buckets, Deletion in batches)

**Depends on:** Slice 5 (`CargoHold.Backup.Deletion`: `ObjectTarget`, `DeletionPlan`, `backupKey`, `keysOnlyPlan`, `deleteBatches`, `goneIds`; `deleteBackupObjects` in `CargoHold.Backup.ObjectStore`), slice 6 (cleanup uses the same `DeleteObjects`).

## Global Constraints

- One `GetBucketVersioning` per `DeleteObjects` interpretation (i.e. per inline deletion and per cleanup batch). Status absent → keys only (slice 5 path). `Enabled`, `Suspended` or request error → list versions.
- Listing: `ListObjectVersions` with prefix `backups/<id>`, one call (paginated) per file, at most `listingParallelism = 8` concurrently. Keep only versions and delete markers whose key equals `backupKey id` exactly.
- `ListObjectVersions` answering `NotImplemented` (HTTP 501 **or** error code `"NotImplemented"`) → plain exact key `backups/<id>` (no version id, no listing); the remaining files of the same call skip listing too. Any other listing error → that file isn't deleted and isn't reported gone (row stays for cleanup).
- `DeleteObjects`: all targets (key + version id) of all files, ≤ 1000 per call, `quiet = True`. A file is gone iff none of its targets failed; an empty listing = already gone.
- The interpreter never throws; failures are logged (warn) with the `AWS.Env` logger and reported as not gone.
- `CargoHold.Backup.Deletion` stays pure and free of amazonka types; conversions happen in `ObjectStore.hs`.
- Local garage: `GetBucketVersioning` returns no status, so integration tests only cover the keys-only path; versioned paths are covered only by unit tests.
- Operators need S3 permissions `s3:GetBucketVersioning`, `s3:ListBucketVersions`, `s3:ListBucket`, `s3:DeleteObject`, `s3:DeleteObjectVersion` on the bucket/prefix (document it).
- Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`. Never run integration tests.
- Commit: type-checks, no warnings, unit tests green, `make format`, `make regen-local-nix-derivations` if deps changed.

## Review Focus

1. **Versioned bucket: every version and delete marker is deleted** (a plain delete only adds a marker). → unit test `versionedDeletesAllVersionsAndMarkers` (Task 1).
2. **Unknown versioning state takes the safe path** (GetBucketVersioning error → list versions). → unit test `versioningErrorListsVersions` (Task 1).
3. **Listing failure keeps the row; `NotImplemented` falls back** instead of failing forever. → unit tests `listingFailedNotGone`, `classifyNotImplemented` (Task 1).
4. **Partial `DeleteObjects` errors**: a file with any failed version isn't reported gone. → unit test `partialDeleteErrors` (Task 1).
5. **Batches ≤ 1000 targets** even when one file's versions span batches. → QuickCheck `versionedBatchesBounded` (Task 1).

---

## Commit: version-aware deletion

### Task 1: pure planning in `CargoHold.Backup.Deletion`

**Files:**
- Modify: `services/cargohold/src/CargoHold/Backup/Deletion.hs`
- Test: `services/cargohold/test/unit/Test/CargoHold/Backup/DeletionTest.hs` (already registered by slice 5)

**Interfaces:**
- Produces:
  ```haskell
  data Versioning = VersioningNeverEnabled | VersioningMaybeOn            -- Eq, Show
  versioningFrom :: Either e (Maybe s) -> Versioning                       -- Right Nothing → NeverEnabled; else MaybeOn
  data Listing = Listed [ObjectTarget] | ListingFailed
  data ListError = ListNotImplemented | ListOtherError                     -- Eq, Show
  classifyListError :: Int -> Text -> ListError                            -- HTTP status, error code; 501 or "NotImplemented" → ListNotImplemented
  planDeletion :: [BackupFileId] -> Versioning -> Map BackupFileId Listing -> DeletionPlan
  -- NeverEnabled: keysOnlyPlan ids (map ignored)
  -- MaybeOn: Listed ts → targets = filter ((== backupKey i) . (.key)) ts; ListingFailed or absent → unlisted
  ```
  `keysOnlyPlan`, `deleteBatches` and `goneIds` stay as in slice 5.

- [ ] **Step 1: Write failing tests** (`a`, `b` fixed ids; `ka = backupKey a`):
  - `versioningErrorListsVersions`: `versioningFrom (Left ())` and `versioningFrom (Right (Just ()))` == `VersioningMaybeOn`; `versioningFrom (Right Nothing)` == `VersioningNeverEnabled`.
  - `neverEnabledKeysOnly`: `deleteBatches (planDeletion [a, b] VersioningNeverEnabled mempty)` == `[[ObjectTarget ka Nothing, ObjectTarget kb Nothing]]`.
  - `versionedDeletesAllVersionsAndMarkers`: `a ↦ Listed [ka "v1", ka "v2", ka "m1"]` → the single batch contains all three with `Just` version ids; `goneIds plan ∅ == [a]`.
  - `versionedIgnoresForeignKeys`: listing contains `ka <> "x"` → not in any batch.
  - `emptyListingIsGone`: `a ↦ Listed []` → no batches, `goneIds plan ∅ == [a]`.
  - `listingFailedNotGone`: `a ↦ Listed [ka "v1"]`, `b ↦ ListingFailed` → batches have only `a`'s target; `goneIds plan ∅ == [a]`.
  - `partialDeleteErrors`: `a ↦ [v1, v2]`, `b ↦ [v1]`, failed `{ka "v2"}` → `goneIds == [b]`.
  - `classifyNotImplemented`: `(501, "")` and `(400, "NotImplemented")` → `ListNotImplemented`; `(403, "AccessDenied")` → `ListOtherError`.
  - QuickCheck `versionedBatchesBounded`: random version counts per file (total 0–2500) → every batch has 1–1000 targets and `concat batches` == all targets.
- [ ] **Step 2: Run** `make c package=cargohold test=1 | grep -vE '...'`. Expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** the same command. Expected: `CargoHold.Backup.Deletion` tests pass.

### Task 2: S3 glue in `CargoHold.Backup.ObjectStore` + commit

**Files:**
- Modify: `services/cargohold/src/CargoHold/Backup/ObjectStore.hs`
- Modify: `services/cargohold/cargohold.cabal` (library deps if missing, e.g. `unliftio`)
- Modify: `docs/src/developer/reference/config-options.md` (cargohold `backups`: required S3 permissions, versioning note), `changelog.d/2-features/backup-api`

**Interfaces:**
- Changes `deleteBackupObjects :: AWS.Env -> [BackupFileId] -> IO [BackupFileId]` (same signature as slice 5):
  1. `[]` → `[]`.
  2. `v <- versioningFrom <$> sendCatch (newGetBucketVersioning bucket)` (`getBucketVersioningResponse_status`).
  3. If `VersioningMaybeOn`: `listings <- Map.fromList <$> pooledMapConcurrentlyN listingParallelism (\i -> (i,) <$> listFile i) ids`.
  4. `plan = planDeletion ids v listings`; run `deleteBatches plan` sequentially (slice 5 batch call, now with `objectIdentifier_versionId`); `goneIds plan failed`.
- New local helpers:
  ```haskell
  listFile :: AWS.Env -> BackupFileId -> IO Listing
  -- ListObjectVersions prefix (paginateEither): versions (objectVersion_key/_versionId) ++ delete markers (deleteMarkerEntry_key/_versionId) → Listed
  -- Left (ServiceError se) with classifyListError (statusCode (se ^. serviceError_status)) (toText (se ^. serviceError_code)) == ListNotImplemented → Listed [ObjectTarget (key fid) Nothing], and set a per-call flag so remaining files skip listing
  -- other Left / exception → log, ListingFailed
  listingParallelism :: Int  -- 8
  ```

- [ ] **Step 1: Implement** per Interfaces. Check garage's actual answers: `GetBucketVersioning` gives status `Nothing` (keys-only), and `ListObjectVersions` gives 501 or code `NotImplemented`. Use `aws s3api` against `deploy/dockerephemeral` garage if available; else note it for the human.
- [ ] **Step 2: Run** `make c package=cargohold test=1 | grep -vE '...'`. Expected: builds, no warnings, tests green. `make c package=integration | grep -vE '...'` compiles. Slice 5's `testBackupDeleteRemovesObject` and `testBackupReplaceRemovesOldObject` exercise the keys-only path against garage (human runs them).
- [ ] **Step 3: Quality + commit**: changelog line ("Backup deletion removes all object versions on versioned S3 buckets; requires `s3:GetBucketVersioning`, `s3:ListBucketVersions`, `s3:DeleteObjectVersion`."). Run `make format` and `make regen-local-nix-derivations` if deps changed; then
  ```bash
  git add services/cargohold docs changelog.d nix
  git commit -m "Backups: delete all object versions on versioned buckets"
  ```

## Unresolved questions

- None. Decided: no `ListObjectsV2` fallback; on `NotImplemented` delete by exact key (`backups/<id>`) and skip listing for the remaining files of that call (RFC deviation, recorded); parallelism 8; Object Lock / MFA delete: log + `deletion_last_tried` only, no alerting.
