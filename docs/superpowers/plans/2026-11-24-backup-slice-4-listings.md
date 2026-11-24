# Backup API Slice 4: Listings Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A client lists the restorable epochs of all its user's clients (`GET /backups`) or of one client (`GET /backups/clients/<cid>`), with per-epoch summaries.

**Architecture:** One vertical commit: wire-api types/routes → `BackupStore.ListBackups` (one aggregate SQL) → `BackupSubsystem` ops → cargohold handlers → unit + integration tests. No migration, no config.

**Tech Stack:** Haskell, servant, polysemy, hasql / hasql-th, hspec, `integration/` testsuite.

**Spec:** `docs/superpowers/specs/2026-11-24-backup-api-design.md` (RFC: `2026-11-16 RFC Backup API(1).md`)

**Depends on:** Slice 2 plan (`2026-11-24-backup-slice-2-full-round-trip.md`) and slice 3 plan (`2026-11-24-backup-slice-3-incrementals.md`): `backup_incrementals`, `BackupState.incrementals`, `inMemoryBackupStore :: Word32 -> …`, `uploadBackupIncremental`.

## Global Constraints

- Routes `From 'V20 :> ZLocalUser :> ZClient`, prefix `/backups`.
- Checks: slice 2 (2) Z-Client exists → else `backup-forbidden` 403; (3) team + `automaticBackups` enabled → else `backup-disabled` 403. No path-client check (reads).
- Every query keyed by caller user id. Foreign/unknown `cid` → 200 `{"client": cid, "epochs": []}` (no existence oracle).
- Only current files counted (no orphans). `fileCount` includes the full; `size` = full + incrementals; `createdAt` = newest current file.
- Order: epochs ascending; clients ascending by `ClientId` `Ord`.
- `ClientId` from `text` via existing `PostgresUnmarshall Text ClientId` (`libs/wire-api/src/Wire/API/PostgresMarshall.hs:1030`).
- No nginz change (slice 2 location covers `/backups`).
- Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`. Integration tests: compile only.
- Commit: type-checks, no warnings, unit tests of changed packages green, `make format`.

## Review Focus

1. **Other users' backups never listed**; foreign `cid` gives empty epochs, not 403/404. → unit test `listNeverShowsOtherUsers` (Task 2); integration `testBackupListOtherUser` (Task 4).
2. **Orphans not counted** (replaced fulls/incrementals, reservations). → unit test `listExcludesOrphans` (Task 2); integration `testBackupListAfterReplace` (Task 4).
3. **Aggregate SQL types**: `SUM(bigint)` is `numeric`, `COUNT` is `bigint`, `MAX` of no incrementals is `NULL`. → integration `testBackupList` (Task 4; real Postgres), unit `listAggregates` (Task 2).
4. **Removed client / disabled team can't list.** → unit test `listChecks` (Task 2); integration `testBackupListDisabled` (Task 4).

---

## Commit: listings

### Task 1: wire-api types, routes

**Files:**
- Modify (from slice 2): `libs/wire-api/src/Wire/API/Backup.hs`
- Modify: `libs/wire-api/src/Wire/API/Routes/Public/Cargohold.hs` (`BackupAPI`)
- Test: `libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs`, `libs/wire-api/test/unit/Test/Wire/API/Backup.hs` (from slice 2)

**Interfaces:**
- Produces (`Wire.API.Backup`), `schema-profunctor` JSON, `Arbitrary`:
  ```haskell
  data EpochSummary = EpochSummary { epoch :: BackupEpoch, createdAt :: UTCTimeMillis, fileCount :: Word32, size :: Word64 }
  data ClientBackups = ClientBackups { client :: ClientId, epochs :: [EpochSummary] }
  data BackupList = BackupList { clients :: [ClientBackups] }
  ```
- Produces (routes, appended to `BackupAPI`):
  - `Named "backup-list"`: `"backups" :> Get '[JSON] BackupList`; `CanThrow` forbidden, disabled.
  - `Named "backup-get-client"`: `"backups" :> "clients" :> Capture "cid" ClientId :> Get '[JSON] ClientBackups`; `CanThrow` forbidden, disabled.

- [ ] **Step 1: Write failing tests:**
  - `toJSON BackupList{…}` equals the RFC `GET /backups` example (`"client": "4b2f7a9c1d3e5f60"`, `"epoch": 2`, `"createdAt": "2026-11-12T10:35:30.456Z"`, `"fileCount": 2`, `"size": 1050624`).
  - Roundtrip: `EpochSummary`, `ClientBackups`, `BackupList`.
- [ ] **Step 2: Run** `make c package=wire-api test=1 | grep -vE '...'` — expected: compile failure.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run** same — expected: green; cargohold fails until Task 3.

### Task 2: wire-subsystems — store, subsystem, mock

**Files:**
- Modify (from slice 2): `libs/wire-subsystems/src/Wire/BackupStore.hs`, `.../BackupStore/Postgres.hs`, `.../BackupSubsystem.hs`, `.../BackupSubsystem/Interpreter.hs`, `test/unit/Wire/MockInterpreters/BackupStore.hs`
- Test (from slice 2): `libs/wire-subsystems/test/unit/Wire/BackupSubsystem/InterpreterSpec.hs`

**Interfaces:**
- Produces (`Wire.BackupStore`): `ListBackups :: UserId -> Maybe ClientId -> BackupStore m [ClientBackups]` — no lock; one statement:
  ```sql
  SELECT e.client_id :: text, e.epoch_no :: bigint,
         GREATEST(e.full_created_at, MAX(i.created_at)) :: timestamptz,
         (1 + COUNT(i.id)) :: bigint,
         (e.full_size + COALESCE(SUM(i.size), 0)) :: bigint
    FROM backup_epochs e LEFT JOIN backup_incrementals i ON i.full_id = e.full_id
   WHERE e.user_id = $1 :: uuid AND e.client_id = COALESCE($2 :: text?, e.client_id)
   GROUP BY e.client_id, e.epoch_no, e.full_created_at, e.full_size
   ORDER BY e.client_id, e.epoch_no
  ```
  Rows grouped in Haskell into `Map ClientId [EpochSummary]` (row order kept) → `[ClientBackups]`. Clients without epochs absent.
- Produces (`Wire.BackupSubsystem`):
  ```haskell
  ListBackups :: Local UserId -> ClientId -> BackupSubsystem m BackupList                       -- caller, Z-Client
  GetClientBackups :: Local UserId -> ClientId -> ClientId -> BackupSubsystem m ClientBackups   -- caller, Z-Client, path client
  ```
  Both: checks (2), (3). `GetClientBackups`: `ListBackups uid (Just cid)`; `[]` → `ClientBackups cid []`.
- Mock: `ListBackups` from `epochs` + `incrementals` state, same semantics.

- [ ] **Step 1: Write failing tests** (`passTime 1` between uploads):
  - `listAggregates`: epoch 1 full 5 B + incs 3 B, 4 B; epoch 2 full 6 B → `epochs == [EpochSummary 1 t_inc2 3 12, EpochSummary 2 t_full2 1 6]` (`t_*` = returned `createdAt`).
  - `listMultipleClients`: clients A, B of same user each with epoch 1 → 2 entries, ascending `ClientId`; `GetClientBackups … A` = A's entry.
  - `listNeverShowsOtherUsers`: user B uploads; user A `ListBackups` → `clients == []`; A `GetClientBackups` on B's cid → `ClientBackups cidB []`.
  - `getClientBackupsEmpty`: no uploads → `epochs == []`.
  - `listExcludesOrphans`: full 5 B + inc 3 B, replace full 7 B → `fileCount 1`, `size 7`; extra store-level `ReserveUpload` (reservation) not counted.
  - `listChecks`: Z-Client not in brig stub → `Left BackupErrorForbidden`; feature off → `Left BackupErrorDisabled` (both ops).
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m Backup' | grep -vE '...'` — expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** same — pass; `make c package=wire-subsystems test=1 | grep -vE '...'` green.

### Task 3: cargohold wiring

**Files:**
- Modify (from slice 2): `services/cargohold/src/CargoHold/API/Backup.hs` (handlers `listBackups`, `getClientBackups`)
- Modify: `services/cargohold/src/CargoHold/API/Public.hs`
- Modify: `changelog.d/2-features/backup-api`

- [ ] **Step 1: Implement.**
- [ ] **Step 2: Run** `make c package=cargohold test=1 | grep -vE '...'` — expected: builds, no warnings, green.

### Task 4: integration tests + commit

**Files:**
- Modify: `integration/test/API/Cargohold.hs`:
  - `listBackups :: (HasCallStack, MakesValue user) => user -> String -> App Response` (user, Z-Client)
  - `getClientBackups :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response` (user, Z-Client, path client)
- Modify (from slice 2): `integration/test/Test/Backups.hs`

- [ ] **Step 1: Write tests:**
  - `testBackupList`: epoch 1 full 1024 + inc 512 → `listBackups` 200, `clients` = one entry, `client == cid`, `epochs == [{epoch 1, createdAt = inc createdAt, fileCount 2, size 1536}]`.
  - `testBackupGetClient`: same entry via `getClientBackups`; second client (no backups) → `{"client": cid2, "epochs": []}`.
  - `testBackupListFromOtherClient`: second client of same user lists → sees first client's entry.
  - `testBackupListOtherUser`: user B `listBackups` → `clients == []`; `getClientBackups` A's cid → 200, `epochs == []`.
  - `testBackupListAfterReplace`: full + inc, new full 2048 → `fileCount 1`, `size 2048`.
  - `testBackupListDisabled`: feature off → 403 `backup-disabled` (both routes); removed Z-Client → 403 `backup-forbidden`.
- [ ] **Step 2: Run** `make c package=integration | grep -vE '...'` — compiles. Do not run; list new test names.
- [ ] **Step 3: Quality + commit**: `make format`; `make c package=wire-api test=1`, `package=wire-subsystems test=1`, `package=cargohold test=1` (filtered) green;
  ```bash
  git add libs/wire-api libs/wire-subsystems services/cargohold changelog.d integration
  git commit -m "Backups: list backups of all clients and of one client"
  ```

## Unresolved questions

- None. Decided: clients ordered by `ClientId` `Ord`.
