# Backup API Slice 8: Server-side Deletion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** When a client is removed or a user is deleted, brig orphans that client's or user's backups. Team deletion is covered because galley deletes every member through brig's `DELETE /i/users/:uid`. S3 objects are deleted by cargohold's cleanup cron (slice 6, every 5 min).

**Architecture:** brig runs `BackupStore` (Postgres interpreter from wire-subsystems) against the DB it shares with cargohold. Client removal: `OrphanClient` in `ClientSubsystem.execDelete`. User deletion: `orphanUserBackups` (`ListBackupClients`, then one `OrphanClient` transaction per client) at the start of `deleteAccount`. There is no RabbitMQ job, no background-worker handler, no internal cargohold endpoint and no `CargoholdAPIAccess`. Two vertical commits: (A) client removal, (B) user deletion.

**Tech Stack:** Haskell, polysemy, hasql / hasql-th / hasql-transaction, hspec/QuickCheck, `integration/` testsuite.

**Spec:** `docs/superpowers/specs/2026-11-24-backup-api-design.md` (sections brig, Decisions, RFC deviations, Concurrency, Commit sequence 8). RFC: `2026-11-16 RFC Backup API(1).md`, section "Server-side deletion".

**Depends on:** Slices 2–7. Used as given:
- `Wire.BackupStore`: `OrphanClient :: UserId -> ClientId -> BackupStore m [BackupFileId]` (slice 5). It runs one transaction with the per-client advisory lock, orphans committed files with `file_created_at` set and deletes the epochs.
- `CommitIncremental` (slice 3).
- `interpretBackupStoreToPostgres :: (PGConstraints r) => Word32 -> InterpreterFor BackupStore r` (slice 3).
- Mock `inMemoryBackupStore :: Word32 -> …`, `BackupState`, `MockOrphan` (slices 2/3/5).
- Integration helpers:
  - `setupBackupUser`, `uploadBackupFull`, `getBackupEpoch`, `downloadBackupFull` (slice 2)
  - `getClientBackups` (slice 4)
  - `postBackupsCleanup` (slice 6)

## Global Constraints

- brig depends only on `BackupStore` and its wire-subsystems Postgres interpreter. There is no S3 in brig and no call to cargohold. Objects go away with the next cleanup run (≤ 5 min plus `postBackupsCleanup` batching).
- Locks:
  - Exactly one advisory lock per transaction.
  - Per-user deletion loops over `ListBackupClients` and runs one `OrphanClient` transaction per client. There is no multi-lock, and a retry resumes where the last run stopped.
  - `ListBackupClients` only reads and takes no lock: `SELECT DISTINCT client_id FROM backup_epochs WHERE user_id = $1`. It uses the PK prefix, so no migration is needed.
- Orphaning happens before any state change:
  - In `execDelete`: before cookie revocation, the delete queue and `ClientStore.delete`.
  - In `deleteAccount`: before freeing keys.
  - A Postgres error aborts the operation, the client or user stays intact, and the caller retries. Exception: `addClient` eviction, where the new client is already stored (see Unresolved questions).
- The ids returned by `OrphanClient` are ignored in brig, because the cron deletes those objects.
- The backup tables exist before brig serves: brig runs `runAllMigrations` at startup (`Brig/Run.hs:87`).
- Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`. Never run integration tests (compile only); the human runs them.
- Each commit must type-check, have no warnings, have green unit tests in the changed packages and pass `make format`. Run `make regen-local-nix-derivations` if dependencies change (none expected).
- Prefer record dot syntax over lenses for simple cases.

## Review Focus

1. **Same `ClientId` under two users:** orphaning is keyed by `(user, client)` and must never touch the other user's backups. → unit tests `removing client orphans its backups` (Task 2) and `orphanUserBackupsOrphansAllClients` (Task 4).
2. **Orphaning fails (Postgres down):** client removal aborts before any state change. The client stays, no deletion is enqueued, and a retry works. → unit test `orphan failure keeps client` (Task 2).
3. **Every client-removal path orphans:**
   - explicit removal
   - eviction of temporary clients in `addClient`
   - legal-hold client removal
   - `noPrekeys`, which shares `execDelete` and is not tested separately

   A rejected removal orphans nothing. → unit tests `removing client orphans its backups`, `evicting temporary client orphans its backups`, `removing legal hold client orphans its backups` and `legal hold client cannot be removed` (extended) (Task 2).
4. **User deletion is idempotent and resumable:** a second run is a no-op, and reservations (`file_created_at NULL`) stay for cleanup. → unit tests `orphanUserBackupsIdempotent`, `orphanUserBackupsKeepsReservations` (Task 4).
5. **Team deletion removes every member's backups** without a galley change. → integration test `testBackupsDeletedOnTeamDeletion` (Task 5).

---

## Commit A: client removal

### Task 1: (none)

`BackupStore` interpreters are already config-free (slice 3 passes `maxFilesPerEpoch` per `CommitIncremental` call), so brig can run `interpretBackupStoreToPostgres` directly. Nothing to do.

### Task 2: `execDelete` orphans the client's backups

**Ordering decision:** orphan **before** cookie revocation, `enqueueClientDeletion` and `ClientStore.delete`. If it fails, the client is intact and the caller can retry. Doing it after would leave a deleted client whose live epochs no cron ever sees.

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/ClientSubsystem/Interpreter.hs`
- Modify: `libs/wire-subsystems/test/unit/Wire/MiniBackend.hs`
- Modify: `services/brig/src/Brig/CanonicalInterpreter.hs`
- Test: `libs/wire-subsystems/test/unit/Wire/ClientSubsystem/InterpreterSpec.hs`

**Interfaces:**
- `execDelete`:
  ```haskell
  execDelete :: (Member BackupStore r, Member DeleteQueue r, Member AuthenticationSubsystem r, Member ClientStore r) => UserId -> Maybe ConnId -> Client -> Sem r ()
  execDelete u con c = do
    void $ BackupStore.orphanClient u c.clientId
    -- existing: revokeCookies, enqueueClientDeletion, ClientStore.delete
  ```
- Add `Member BackupStore r` next to every `Member DeleteQueue r` in the file:
  - `runClientSubsystem`, `addClient`, `execDelete`, `rmClient`, `removeLegalHoldClient`, `noPrekeys`
  - `claimPrekey`, `claimLocalPrekey`
  - `claimMultiPrekeyBundles`, `claimMultiPrekeyBundlesV3`, `claimMultiPrekeyBundlesInternal`, `claimLocalMultiPrekeyBundles`
- Brig:
  - Add `BackupStore` to `BrigLowerLevelEffects` next to `UserGroupStore`.
  - Add `. interpretBackupStoreToPostgres` next to `. interpretUserGroupStoreToPostgres` in `runBrigToIO`. It sits inside `runInputConst e.hasqlPool` and `mapError postgresUsageErrorToHttpError`, so no local pool trick is needed.
  - No cabal change.
- MiniBackend:
  - `runClientSubsystem` is used twice (lines ~374 and ~840), so add `BackupStore` to `MiniBackendLowerEffects`.
  - Interpret it inside `interpretNowConst`, with `inMemoryBackupStore` over a local `evalState` of an empty `BackupState` (not observed). Add `emptyBackupState` to the mock if it is missing.
- `ClientSubsystem` test stack:
  - `ClientSubsystemTestEffects` adds:
    - `BackupStore`, interpreted by `inMemoryBackupStore` inside `interpretNowConst`.
    - `State BackupState`, at the outer end next to `State [Push]` and run with `runState` there. This keeps the final state observable after a `Left`.
  - `ClientSubsystemTestResult` gets `backups :: BackupState`.
  - Add `runClientSubsystemTestWith :: (forall r. (Member (State BackupState) r, Member Now r, Member (Error ClientError) r) => InterpreterFor BackupStore r) -> [StoredUser] -> Sem (ClientSubsystem ': ClientSubsystemTestEffects) a -> ClientSubsystemTestResult a`. `runClientSubsystemTest = runClientSubsystemTestWith inMemoryBackupStore`.
  - Test-local `failingBackupStore`: `OrphanClient` throws a sentinel `ClientError` (the stack has no `UsageError`), and every other op delegates to `inMemoryBackupStore`.
- Tests seed backups by calling `reserveUpload` and `commitFull` directly in the test action, because `BackupStore` is in the stack.

- [ ] **Step 1: Write failing tests.** Fixture: user U with clients A and B, user V with the same `ClientId` A. Each has epoch 1, committed via the store.
  - `removing client orphans its backups`: `removeClient U conn A Nothing` →
    - no `epochs` key `(U, A, _)`
    - U/A's full is in `orphans` with `fileCreatedAt == Just _`
    - U/B and V/A are intact
  - `evicting temporary client orphans its backups`: add temporary client t1 and commit epoch 1 for it, then add temporary client t2 (`lpk1 /= lpk2 ==>`) → no `(U, t1, _)` epochs, and t1's full is orphaned.
  - `removing legal hold client orphans its backups`: add a `LegalHoldClientType` client with epoch 1, then `removeLegalHoldClient U` → its epoch is orphaned.
  - Extend `legal hold client cannot be removed`: `backups` is unchanged.
  - `orphan failure keeps client` (`runClientSubsystemTestWith failingBackupStore`): the action is `(,) <$> try @ClientError (removeClient …) <*> lookupLocalClient U A` →
    - `(Left sentinel, Just _)`
    - `deletions == []`
    - `backups` epochs unchanged
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m ClientSubsystem' | grep -vE '...'`. Expected: compile failure, then failing tests.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** `make c package=wire-subsystems test=1 | grep -vE '...'` and `make c package=brig | grep -vE '...'`. Expected: green, no warnings.

### Task 3: integration test (client removal) + commit

**Files:**
- Modify: `integration/test/Test/Backups.hs`. Add a local helper `assertBackupObjectGone :: (HasCallStack, MakesValue domain) => domain -> String -> App ()`: `eventually` (`postBackupsCleanup domain`, then an unauthenticated GET of the presigned `Location` → 404). It loops because orphans from other tests may fill a cleanup batch.

- [ ] **Step 1: Write tests:**
  - `testBackupsDeletedOnClientRemoval`:
    1. `setupBackupUser`, then a second client B.
    2. Upload epoch 1 for client A. `downloadBackupFull` → 302; keep `Location`.
    3. `deleteClient u A` → 200.
    4. Immediately (synchronous): `getBackupEpoch u B A 1` → 404 `backup-not-found`, and `getClientBackups u B A` → 200 with `epochs == []`.
    5. `assertBackupObjectGone OwnDomain location`.
- [ ] **Step 2: Run** `make c package=integration | grep -vE '...'`. Expected: it compiles. Don't run it; list the test name for the human.
- [ ] **Step 3: Quality + commit:**
  - Run `make format`.
  - wire-subsystems and cargohold unit tests must be green, and brig must build (all filtered).
  - Append to `changelog.d/2-features/backup-api`: "Backups of removed clients are deleted server-side."
  ```bash
  git add libs/wire-subsystems services/brig services/cargohold integration changelog.d
  git commit -m "Backups: orphan backups on client removal"
  ```

## Commit B: user deletion (covers team deletion)

### Task 4: `ListBackupClients`, `orphanUserBackups`

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/BackupStore.hs`, `.../BackupStore/Postgres.hs`
- Modify: `libs/wire-subsystems/test/unit/Wire/MockInterpreters/BackupStore.hs`
- Test: `libs/wire-subsystems/test/unit/Wire/BackupSubsystem/InterpreterSpec.hs` (new `describe "orphanUserBackups"`, reusing the existing fixtures)

**Interfaces:**
- `ListBackupClients :: UserId -> BackupStore m [ClientId]`
  - Postgres: hasql-th statement via `runStatement` (SQL in Global Constraints).
  - Mock: distinct `ClientId`s of the `epochs` keys for this user.
- `orphanUserBackups :: (Member BackupStore r) => UserId -> Sem r ()`: run `listBackupClients`, then `orphanClient` for each client and discard the ids.

- [ ] **Step 1: Write failing tests.** Fixture: U/A has epochs 1 and 2 (epoch 2 has one incremental), U/B has epoch 1, and V/A has epoch 1.
  - `listBackupClientsDistinct`: `sort <$> listBackupClients U == [A, B]`; an unknown user gives `[]`.
  - `orphanUserBackupsOrphansAllClients`:
    - no `epochs` key with U
    - all of U's full and incremental ids are in `orphans` with `Just createdAt`
    - V/A is intact
  - `orphanUserBackupsIdempotent`: the state after the second run equals the state after the first. An unknown user leaves the state unchanged.
  - `orphanUserBackupsKeepsReservations`: after `reserveUpload` for U/A (not committed), run the helper → the reservation is still in `orphans` with `fileCreatedAt == Nothing`.
- [ ] **Step 2: Run** `cabal test wire-subsystems-tests --test-options='-m Backup' | grep -vE '...'`. Expected: compile failure.
- [ ] **Step 3: Implement** per Interfaces.
- [ ] **Step 4: Run** the same command. Expected: all `Backup` specs pass.

### Task 5: brig `deleteAccount` orphans + integration tests + commit

**Files:**
- Modify: `services/brig/src/Brig/API/User.hs`:
  - `deleteAccount` (line ~1177): call `orphanUserBackups uid` right after the log line.
  - Add `Member BackupStore r` to `deleteSelfUser`, `verifyDeleteUser` and `ensureAccountDeleted`.
- Modify: `services/brig/src/Brig/API/Public.hs`: `servantSitemap`, `deleteSelfUser`, `verifyDeleteUser`.
- Modify: `services/brig/src/Brig/API/Internal.hs`: `servantSitemap`, `accountAPI`, `deleteUserNoAuthH`.
- Modify: `services/brig/src/Brig/Team/API.hs`: `servantAPI`, `deleteInvitation` (SCIM pending user).
- Modify: `services/brig/src/Brig/InternalEvent/Process.hs`: `onEvent` (the `DeleteUser` event).
- Modify: `integration/test/API/Galley.hs` (**added; no helper exists**): `deleteTeam :: (HasCallStack, MakesValue user) => user -> String -> App Response`. It sends `DELETE /teams/:tid` with `zConnection "conn"` and `addJSONObject ["password" .= defPassword]`.
- Test: `integration/test/Test/Backups.hs`.

**Interfaces:**
```haskell
deleteAccount :: (..., Member BackupStore r) => User -> Sem r ()
```
- The call is unconditional; for personal users it is one empty read.
- `ensureAccountDeleted` skips `deleteAccount` when the user is already deleted. That is fine: the earlier run orphaned the backups.
- SCIM pending-user delete calls `SparAPIAccess.deleteScimUser` before `deleteAccount`. Pending users have no clients and therefore no backups, so this is accepted.
- `deleteBot` (Provider) is not affected: bots have no team.

- [ ] **Step 1: Implement**, adding constraints until `make c package=brig | grep -vE '...'` builds without warnings. The expected set is the Files list above. brig has no unit-testable seam here; Task 4 tests the helper.
- [ ] **Step 2: Write integration tests** (using `assertBackupObjectGone` from Task 3):
  - `testBackupsDeletedOnUserDeletion`:
    1. `createTeam OwnDomain 2` → member `m`. Enable `automaticBackups` and `addClient m`.
    2. Upload epoch 1. `downloadBackupFull` → keep `Location`.
    3. `SetupHelpers.deleteUser m`.
    4. `assertBackupObjectGone OwnDomain location`.
    - A sole owner can't self-delete, so the test uses a member. A deleted user can't authenticate, so the test checks the object, not the API.
  - `testBackupsDeletedOnTeamDeletion`:
    1. `createTeam OwnDomain 2` → owner and member, each with a client and an uploaded epoch 1, `Location`s kept.
    2. `deleteTeam owner tid` → 202.
    3. `assertBackupObjectGone` for both. Team deletion is async, and `eventually` covers that.
- [ ] **Step 3: Run** `make c package=integration | grep -vE '...'`. Expected: it compiles. List the test names for the human.
- [ ] **Step 4: Quality + commit:**
  - Run `make format`, `make c package=wire-subsystems test=1 | grep -vE '...'` (green) and `make c package=brig test=1 | grep -vE '...'` (green).
  - Changelog line: "… and of deleted users and teams."
  ```bash
  git add libs/wire-subsystems services/brig integration changelog.d
  git commit -m "Backups: orphan backups on user and team deletion"
  ```

## Unresolved questions

- None. Decided: `maxFilesPerEpoch` per call is in slice 3; upload-vs-orphan race accepted (RFC; window ≤ `uploadTimeout`); Postgres failure in `addClient` eviction / `noPrekeys` → 500 (Postgres assumed reliable).
