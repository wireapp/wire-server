# fan-in-perf `consume-ack` (experiment C) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `consume-ack` sub-command to `tools/fan-in-perf`: experiment B (`consume`) plus every simulated client acknowledging the notifications it received, measuring ack writes/s, ack latency and ack errors next to push/consume rates.

**Architecture:** Two new store ops (`InitAcks`, `AckNotification`) in `Wire.FanInNotificationsStore`. The reader population becomes coherent (teams → users → clients), so every reader has one `(UserId, ClientId)` identity used for fetching and acking. The reader loop of B is shared; the mode (`NoAcks | Acks {ackEvery}`) is plain data. A pure `AckState` (map stream → pending ack with count) decides when acks fall due; ack writes go through the reader pool and its `QSem` gate.

**Tech Stack:** Haskell (GHC2021, `OverloadedRecordDot`, `DuplicateRecordFields`), Polysemy, hasql + hasql-th, optparse-applicative, prometheus-client, async, hspec/QuickCheck (`hspec-discover`).

**Spec:** `docs/superpowers/specs/2026-10-14-fan-in-perf-consume-ack-design.md` (B: `docs/superpowers/specs/2026-10-12-fan-in-perf-consume-design.md`; where B and the C spec disagree — population — C wins).

## Global Constraints

- Build/test commands always end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` (AGENTS.md). Tool: `make c package=fan-in-perf test=1 | grep -vE '…'`. Single spec: `cabal test fan-in-perf-tests --test-options='-m "<describe text>"' | grep -vE '…'`. Store lib: `make c package=wire-subsystems test=1 | grep -vE '…'`.
- No compiler warnings (`-Wall`, incl. `-Wmissing-fields`, `-Wincomplete-patterns`). All unit tests of changed packages green. `make format` before every commit.
- Do NOT run integration tests (`make ci`, `*-integration`). Do not start/stop docker. DB only in Task 11 (manual acceptance; the user provided the connection string in the session — never write it into files or commits; use `$DB`).
- Tool DB access only through `Wire.FanInNotificationsStore` / `Wire.FanInNotificationsAdmin`. No SQL in the tool.
- Prefer record dot syntax over lenses.
- `consume-ack` = all `consume` flags + `--ack-every N` (default 100, bounds 1..10_000). `consume-ack` rejects `connections` and `user`/`clients` entries with different STREAMS.
- Acks: one counter per client across all streams (`unacked` = sum of per-stream counts); due when `unacked >= N`; ack id = highest delivered id of the stream; upsert uses `GREATEST`; failed acks go back into the state. No flush at page end or drain.
- Start positions in `consume-ack`: `InitAcks` per reader stream before writers start; print `consume-ack: acks reset to latest (K streams)`.
- Metrics prefix `fanin_perf_`, label `experiment="consume-ack"` (`"consume"` for B). Ack metrics registered only in `Acks` mode; each metric registered exactly once per process.
- Commit messages: extremely concise; end with the attribution line given by the harness.
- Out of scope: open `REVIEW:` comments (except where a task removes the code they refer to), `connections` acks, missed-notification check, reconnects, server flow control, experiment D.

## Review Focus

Not covered by the spec's own test list, most likely first; each is pinned by a test in the named task:

1. `--ack-every` larger than all deliveries of a run → `acks=0`, status/summary print 0 and never `NaN` (Task 8: empty-run test in `Acks` mode).
2. Ack write fails (error result or exception) → reader keeps running, ack error counted and reported, notifications stay unacked and are retried at the next ack point (Task 10: `ackPending` / `acknowledge` tests).
3. Stream empty at startup (`InitAcks` → `Nothing`) → cursor nil UUID; the first ack later creates the row (Task 9: `startPositions` test; Task 11: DB count query).
4. `consume-ack --targets team:1,epoch:50` (no `user`/`clients`) → synthetic, unique identities (Task 3: synthetic-users test).
5. `consume` (B) unchanged for users: status line string identical, no ack metrics, unequal `user`/`clients` STREAMS still accepted (Task 8 NoAcks status test, Task 4 options test, Task 3 unequal-pools test).

## File Structure

| File | Change |
|---|---|
| `libs/wire-subsystems/src/Wire/FanInNotificationsStore.hs` | `InitAcks`, `AckNotification` constructors |
| `libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs` | 8 statements, interpreter cases |
| `tools/fan-in-perf/src/FanInPerf/Targets.hs` | user/clients pools = prefixes of one uid vector |
| `tools/fan-in-perf/src/FanInPerf/Population.hs` | rewrite: `SimTeam`/`SimUser`/`Reader`, `readers`, `readerStreams` |
| `tools/fan-in-perf/src/FanInPerf/Produce.hs` | `WriterSetup.populationGen` |
| `tools/fan-in-perf/src/FanInPerf/Options.hs` | `ReaderMode`, `experimentLabel`, `consume-ack` |
| `tools/fan-in-perf/src/FanInPerf/Acks.hs` (new) | pure `Pending`, `AckState`, `deliver`, `restore` |
| `tools/fan-in-perf/src/FanInPerf/ConsumeStats.hs` | ack counters, ack latency, ack rates |
| `tools/fan-in-perf/src/FanInPerf/Metrics.hs` | `AckMetrics` |
| `tools/fan-in-perf/src/FanInPerf/Terminal.hs` | mode-aware status line and summary |
| `tools/fan-in-perf/src/FanInPerf/Consume.hs` | `PassResult`, `isolated`, `startPositions`, `ackOne`, `ackPending`, `acknowledge`, wiring |
| `tools/fan-in-perf/fan-in-perf.cabal` | `FanInPerf.Acks`, `FanInPerf.AcksSpec` |
| `tools/fan-in-perf/test/FanInPerf/*Spec.hs` | per task |
| `tools/fan-in-perf/README.md`, `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json`, B spec | docs, panels, drift note |

---

### Task 1: Store ops `InitAcks`, `AckNotification`

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsStore.hs`
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs`
- Modify: `tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs` (fake store must stay total)

**Interfaces:**
- Produces: `initAcks :: Member FanInNotificationsStore r => UserId -> ClientId -> Stream -> Sem r (Maybe UUID)`, `ackNotification :: Member FanInNotificationsStore r => UserId -> ClientId -> Stream -> UUID -> Sem r ()` (generated by `makeSem`).

SQL has no unit coverage (as in B); correctness is checked in Task 11 against the DB. The test of this task is "compiles without warnings, existing tests green".

- [ ] **Step 1: Add the constructors**

In `FanInNotificationsStore.hs`, append to the GADT:

```haskell
  -- | RFC "when nothing is acknowledged": sets the ack of the client (user id,
  -- client id) for the stream to the stream's latest notification and returns
  -- it. 'Nothing': nothing was ever pushed to the stream; no ack row is written.
  InitAcks :: UserId -> ClientId -> Stream -> FanInNotificationsStore m (Maybe UUID)
  -- | Raises the client's ack of the stream to the id; never lowers it.
  AckNotification :: UserId -> ClientId -> Stream -> UUID -> FanInNotificationsStore m ()
```

- [ ] **Step 2: Interpreter cases and statements**

In `Postgres.hs`, add to `interpretFanInNotificationsStoreToPostgres`:

```haskell
  InitAcks uid cid stream -> initAcksImpl uid cid stream
  AckNotification uid cid stream nid -> ackNotificationImpl uid cid stream nid
```

Add below `getLastNotificationIdImpl`'s statements:

```haskell
clientInt :: ClientId -> Int64
clientInt c = fromIntegral c.clientToWord64

-- | Ack rows belong to the client (@uid@, @cid@); the stream's own key selects
-- the latest notification. Conflict targets are the ack tables' primary keys
-- (the RFC's SQL lacks them).
initAcksImpl :: (PGConstraints r) => UserId -> ClientId -> Stream -> Sem r (Maybe UUID)
initAcksImpl uid cid = \case
  StreamUser u -> runStatement (uid.toUUID, clientInt cid, u.toUUID) initUserAckStatement
  StreamClient u c -> runStatement (uid.toUUID, clientInt cid, u.toUUID, clientInt c) initClientAckStatement
  StreamTeam t -> runStatement (uid.toUUID, clientInt cid, t.toUUID) initTeamAckStatement
  StreamEpoch g e -> runStatement (uid.toUUID, clientInt cid, g.unGroupId, fromIntegral e.epochNumber) initEpochAckStatement

initUserAckStatement :: Statement (UUID, Int64, UUID) (Maybe UUID)
initUserAckStatement =
  [maybeStatement|
    insert into user_notification_acks (user_id, client_id, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), notification_id
      from last_user_notifications where user_id = ($3 :: uuid)
      on conflict (user_id, client_id) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

initClientAckStatement :: Statement (UUID, Int64, UUID, Int64) (Maybe UUID)
initClientAckStatement =
  [maybeStatement|
    insert into client_notification_acks (user_id, client_id, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), notification_id
      from last_client_notifications where user_id = ($3 :: uuid) and client_id = ($4 :: int8)
      on conflict (user_id, client_id) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

-- RFC: selected from team_notifications with an invalid GROUP BY; the last_*
-- table holds exactly the wanted id.
initTeamAckStatement :: Statement (UUID, Int64, UUID) (Maybe UUID)
initTeamAckStatement =
  [maybeStatement|
    insert into team_notification_acks (user_id, client_id, team_id, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), team_id, notification_id
      from last_team_notifications where team_id = ($3 :: uuid)
      on conflict (user_id, client_id, team_id) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

-- RFC: wrote the nonexistent conversation_notification_acks with EXCLUDED.notificationId.
initEpochAckStatement :: Statement (UUID, Int64, ByteString, Int64) (Maybe UUID)
initEpochAckStatement =
  [maybeStatement|
    insert into epoch_notification_acks (user_id, client_id, group_id, epoch, last_notification_id)
      select ($1 :: uuid), ($2 :: int8), group_id, epoch, notification_id
      from last_epoch_notifications where group_id = ($3 :: bytea) and epoch = ($4 :: int8)
      on conflict (user_id, client_id, group_id, epoch) do update
        set last_notification_id = excluded.last_notification_id
      returning last_notification_id :: uuid
  |]

-- | @greatest@: a late or repeated ack never moves the ack backwards.
ackNotificationImpl :: (PGConstraints r) => UserId -> ClientId -> Stream -> UUID -> Sem r ()
ackNotificationImpl uid cid stream nid = case stream of
  StreamUser _ -> runStatement (uid.toUUID, clientInt cid, nid) ackUserStatement
  StreamClient _ _ -> runStatement (uid.toUUID, clientInt cid, nid) ackClientStatement
  StreamTeam t -> runStatement (uid.toUUID, clientInt cid, t.toUUID, nid) ackTeamStatement
  StreamEpoch g e -> runStatement (uid.toUUID, clientInt cid, g.unGroupId, fromIntegral e.epochNumber, nid) ackEpochStatement

ackUserStatement :: Statement (UUID, Int64, UUID) ()
ackUserStatement =
  [resultlessStatement|
    insert into user_notification_acks (user_id, client_id, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: uuid)
      on conflict (user_id, client_id) do update
        set last_notification_id = greatest(user_notification_acks.last_notification_id, excluded.last_notification_id)
  |]

ackClientStatement :: Statement (UUID, Int64, UUID) ()
ackClientStatement =
  [resultlessStatement|
    insert into client_notification_acks (user_id, client_id, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: uuid)
      on conflict (user_id, client_id) do update
        set last_notification_id = greatest(client_notification_acks.last_notification_id, excluded.last_notification_id)
  |]

ackTeamStatement :: Statement (UUID, Int64, UUID, UUID) ()
ackTeamStatement =
  [resultlessStatement|
    insert into team_notification_acks (user_id, client_id, team_id, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: uuid, $4 :: uuid)
      on conflict (user_id, client_id, team_id) do update
        set last_notification_id = greatest(team_notification_acks.last_notification_id, excluded.last_notification_id)
  |]

ackEpochStatement :: Statement (UUID, Int64, ByteString, Int64, UUID) ()
ackEpochStatement =
  [resultlessStatement|
    insert into epoch_notification_acks (user_id, client_id, group_id, epoch, last_notification_id)
      values ($1 :: uuid, $2 :: int8, $3 :: bytea, $4 :: int8, $5 :: uuid)
      on conflict (user_id, client_id, group_id, epoch) do update
        set last_notification_id = greatest(epoch_notification_acks.last_notification_id, excluded.last_notification_id)
  |]
```

If hasql-th rejects a statement's syntax (e.g. `returning` after `on conflict`), report the exact parser error and stop; do not fall back to string SQL without asking.

Optionally refactor `fetchNotificationsImpl`/`getLastNotificationIdImpl` to use `clientInt` (same file, same conversion).

- [ ] **Step 3: Keep the test fake total**

In `tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs`, `fakeStore` gets two cases (Task 9 replaces them with recording versions):

```haskell
  InitAcks _ _ s -> fmap fst . listToMaybe . reverse . fromMaybe [] . lookup s <$> readIORef rows
  AckNotification {} -> pure ()
```

- [ ] **Step 4: Build and test both packages**

Run: `make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings, all tests pass.

- [ ] **Step 5: Format and commit**

```bash
make format
git add libs/wire-subsystems/src/Wire/FanInNotificationsStore.hs libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs
git commit -m "FanInNotificationsStore: InitAcks, AckNotification"
```

---

### Task 2: Shared user ids for `user` and `clients` pools

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Targets.hs` (`mkEntries`, `mkPool`)
- Test: `tools/fan-in-perf/test/FanInPerf/TargetsSpec.hs`

**Interfaces:**
- Produces: unchanged signature `mkEntries :: RandomGen g => Int -> NonEmpty TargetConfig -> g -> (V.Vector Entry, g)`; new invariant: the `UserPool` vector and the uids of the `ClientsPool` vector are prefixes of one distinct uid vector of length `max S_u S_c`.

- [ ] **Step 1: Write the failing test**

Add to `TargetsSpec.hs` (inside `spec`):

```haskell
  describe "mkEntries" $
    prop "user and clients pools are prefixes of one distinct uid vector" $ \seed ->
      let specs = TargetConfig KindUser 30 1 :| [TargetConfig KindClients 50 1, TargetConfig KindTeam 5 1]
          (entries, _) = mkEntries 2 specs (mkStdGen seed)
          userIds = [u | e <- V.toList entries, UserPool v <- [e.pool], u <- V.toList v]
          clientUids = [u | e <- V.toList entries, ClientsPool v <- [e.pool], (u, _) <- V.toList v]
       in length userIds == 30
            && length clientUids == 50
            && userIds == take 30 clientUids
            && length (nubOrd clientUids) == 50
```

- [ ] **Step 2: Run it, expect FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "mkEntries"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: FAIL (`userIds == take 30 clientUids` is false).

- [ ] **Step 3: Implement**

Replace `mkEntries` and `mkPool` in `Targets.hs`:

```haskell
-- | The user and clients pools are prefixes of one uid vector, so a reader
-- client (user, client id) can read both the user's and its own client stream
-- (see "FanInPerf.Population"). Different tables, so no extra contention.
mkEntries :: (RandomGen g) => Int -> NonEmpty TargetConfig -> g -> (V.Vector Entry, g)
mkEntries clientsPerUser specs g0 =
  let (gUsers, g1) = split g0
      sharedUsers = maximum (0 : [s.streams | s <- NE.toList specs, s.kind `elem` [KindUser, KindClients]])
      users = V.unfoldrExactN sharedUsers genId gUsers
      step g s =
        let (p, g') = mkPool clientsPerUser users s g
         in (g', Entry s p)
      (g2, entries) = mapAccumL step g1 (NE.toList specs)
   in (V.fromList entries, g2)

mkPool :: (RandomGen g) => Int -> V.Vector UserId -> TargetConfig -> g -> (StreamPool, g)
mkPool clientsPerUser users s g0 =
  let (g1, g2) = split g0
   in ( case s.kind of
          KindUser -> UserPool (V.take s.streams users)
          KindClients -> ClientsPool (V.map (,clientIds) (V.take s.streams users))
          KindTeam -> TeamPool (V.unfoldrExactN s.streams genId g1)
          KindEpoch -> EpochPool (V.unfoldrExactN s.streams genGroupEpoch g1)
          KindConnections -> ConnectionsPool (V.unfoldrExactN s.streams genId g1),
        g2
      )
  where
    -- Client ids 1..C per user: distinct, so one push never inserts the same
    -- (user, client, notification) row twice.
    clientIds = ClientId 1 :| map ClientId [2 .. fromIntegral clientsPerUser]
```

Remove the now unused `genUserClients`. Export `genId` (Task 3 uses it for synthetic users).

- [ ] **Step 4: Run all tool tests, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings (`PopulationSpec` still passes: its expectations do not depend on uid values).

- [ ] **Step 5: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Targets.hs tools/fan-in-perf/test/FanInPerf/TargetsSpec.hs
git commit -m "fan-in-perf: share user ids of user and clients pools"
```

---

### Task 3: Coherent population (teams → users → clients)

**Files:**
- Modify (rewrite): `tools/fan-in-perf/src/FanInPerf/Population.hs`
- Modify: `tools/fan-in-perf/src/FanInPerf/Produce.hs` (`WriterSetup`, `prepareWriters`)
- Modify: `tools/fan-in-perf/src/FanInPerf/Consume.hs` (use `readers`/`readerStreams`)
- Test (rewrite): `tools/fan-in-perf/test/FanInPerf/PopulationSpec.hs`; adjust `tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs`

**Interfaces:**
- Consumes: `genId` (Task 2), `Entry`, `StreamPool` from `FanInPerf.Targets`.
- Produces:
  ```haskell
  data SimTeam = SimTeam {tid :: !TeamId, members :: !(NonEmpty SimUser)}
  data SimUser = SimUser {uid :: !UserId, epochs :: ![(GroupId, Epoch)], clients :: !(NonEmpty ClientId)}
  data Reader = Reader {team :: !(Maybe TeamId), user :: !SimUser, cid :: !ClientId}
  data Population = Population {teams :: [SimTeam], personalUsers :: [SimUser], kinds :: Set TargetKind, subscribers :: Map Key Int}
  mkPopulation :: Int -> Int -> Int -> V.Vector Entry -> StdGen -> Either Text Population
  -- mkPopulation clients epochsPerClient clientsPerUser entries gen
  readers :: Population -> [Reader]
  readerStreams :: Set TargetKind -> Reader -> [Stream]
  streamKey :: Stream -> Key   -- now exported (Task 5)
  streamKind, expectedDeliveries, coverage   -- unchanged
  ```
  `WriterSetup` gains `populationGen :: StdGen`. `Consume.loadCursors :: Member FanInNotificationsStore r => [Stream] -> Sem r [Subscription]` (was `Client -> …`; Task 9 replaces it).

- [ ] **Step 1: Rewrite the tests**

Replace `PopulationSpec.hs` with:

```haskell
module FanInPerf.PopulationSpec (spec) where

import Data.Containers.ListUtils (nubOrd)
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Population
import FanInPerf.Targets hiding (spec)
import Imports
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wire.FanInNotificationsStore (FanInPush (..), Stream (..))

entriesOf :: Int -> [TargetConfig] -> V.Vector Entry
entriesOf clientsPerUser cfgs = fst (mkEntries clientsPerUser (NE.fromList cfgs) (mkStdGen 7))

-- | @population clients epochsPerClient clientsPerUser entries@
population :: Int -> Int -> Int -> V.Vector Entry -> Population
population n e c es = either (error . T.unpack) id (mkPopulation n e c es (mkStdGen 3))

allUsers :: Population -> [SimUser]
allUsers p = [u | t <- p.teams, u <- toList t.members] <> p.personalUsers

streamsOf :: Population -> [[Stream]]
streamsOf p = map (readerStreams p.kinds) (readers p)

-- the only push a single-entry spec can produce, for any seed
pushOf :: V.Vector Entry -> FanInPush
pushOf es = fst (genPush (mkPayload 0) es (mkStdGen 1))

spec :: Spec
spec = do
  describe "mkPopulation" $ do
    it "spreads clients over a user pool; shared streams are allowed" $ do
      let p = population 25 3 1 (entriesOf 1 [TargetConfig KindUser 10 1])
      all (\ss -> length ss == 1 && all ((== KindUser) . streamKind) ss) (streamsOf p) `shouldBe` True
      length (readers p) `shouldBe` 25
      sum (Map.elems p.subscribers) `shouldBe` 25
      all (`elem` [2, 3]) (Map.elems p.subscribers) `shouldBe` True

    it "puts every client on the single team of a one-team pool" $ do
      let es = entriesOf 1 [TargetConfig KindTeam 1 1]
          p = population 25 3 1 es
      expectedDeliveries p.subscribers (pushOf es) `shouldBe` 25

    it "gives each user E distinct consecutive epoch streams" $ do
      let p = population 10 3 1 (entriesOf 1 [TargetConfig KindEpoch 10 1])
      all (\ss -> length (nubOrd (map show ss)) == 3) (streamsOf p) `shouldBe` True
      sum (Map.elems p.subscribers) `shouldBe` 30

    it "rejects more epochs per client than epoch streams" $
      isLeft (mkPopulation 10 11 1 (entriesOf 1 [TargetConfig KindEpoch 10 1]) (mkStdGen 3)) `shouldBe` True

    it "numbers the clients of a user 1..k, and rejects more than --clients-per-user" $ do
      let es = entriesOf 2 [TargetConfig KindClients 4 1]
      -- 4 clients: one per user (client id 1); a push to client ids [1,2] reaches 1 subscriber
      expectedDeliveries (population 4 3 2 es).subscribers (pushOf es) `shouldBe` 1
      -- 8 clients: both client ids subscribed
      expectedDeliveries (population 8 3 2 es).subscribers (pushOf es) `shouldBe` 2
      isLeft (mkPopulation 9 3 2 es (mkStdGen 3)) `shouldBe` True

    it "rejects the connections kind" $
      isLeft (mkPopulation 5 3 1 (entriesOf 1 [TargetConfig KindConnections 5 1]) (mkStdGen 3)) `shouldBe` True

    it "combines kinds per client: user, client, team, epochs" $ do
      let p = population 6 2 1 (entriesOf 1 [TargetConfig KindUser 6 1, TargetConfig KindClients 6 1, TargetConfig KindTeam 2 1, TargetConfig KindEpoch 4 1])
      all (\ss -> map streamKind ss == [KindUser, KindClients, KindTeam, KindEpoch, KindEpoch]) (streamsOf p) `shouldBe` True

    it "counts N subscribers per kind and E x N for epochs" $ do
      let p = population 30 2 3 (entriesOf 3 [TargetConfig KindUser 10 1, TargetConfig KindClients 10 1, TargetConfig KindTeam 4 1, TargetConfig KindEpoch 9 1])
      sum (Map.elems p.subscribers) `shouldBe` 30 * (1 + 1 + 1 + 2)

    it "puts every user in exactly one team; all its clients share team and epochs" $ do
      let p = population 40 2 4 (entriesOf 4 [TargetConfig KindUser 10 1, TargetConfig KindTeam 3 1, TargetConfig KindEpoch 7 1])
          uids = [u.uid | t <- p.teams, u <- toList t.members]
      (length uids, length (nubOrd uids), null p.personalUsers) `shouldBe` (10, 10, True)
      map (length . (.members)) p.teams `shouldBe` [4, 3, 3]
      all (\u -> length u.clients == 4 && length u.epochs == 2) (allUsers p) `shouldBe` True

    it "creates ceil(N / clients-per-user) unique synthetic users without user or clients targets" $ do
      let p = population 25 1 2 (entriesOf 2 [TargetConfig KindTeam 2 1, TargetConfig KindEpoch 50 1])
      length (allUsers p) `shouldBe` 13
      length (nubOrd (map (.uid) (allUsers p))) `shouldBe` 13
      length (readers p) `shouldBe` 25

    it "accepts user and clients pools of different sizes (consume)" $
      length (readers (population 10 1 1 (entriesOf 1 [TargetConfig KindUser 10 1, TargetConfig KindClients 5 1]))) `shouldBe` 10

    prop "identities are unique, 1..k per user, and match the user and client streams" $ \(Positive n0) (Positive s0) (Positive c0) ->
      let n = min 300 n0
          s = min 50 s0
          c = 1 + c0 `mod` 4
          es = entriesOf c [TargetConfig KindUser s 1, TargetConfig KindClients s 1, TargetConfig KindTeam 3 1, TargetConfig KindEpoch 7 1]
       in case mkPopulation n 2 c es (mkStdGen 3) of
            Left _ -> n > s * c -- only the capacity check may reject
            Right p ->
              let rs = readers p
                  ids = [(r.user.uid, r.cid) | r <- rs]
                  matches r = \case
                    StreamUser u -> u == r.user.uid
                    StreamClient u cid -> (u, cid) == (r.user.uid, r.cid)
                    _ -> True
               in length rs == n
                    && length (nubOrd ids) == n
                    && all (\r -> all (matches r) (readerStreams p.kinds r)) rs
                    && all (\u -> toList u.clients == map ClientId [1 .. fromIntegral (length u.clients)]) (allUsers p)

  describe "expectedDeliveries" $
    it "is 0 for a push to a stream nobody reads" $ do
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
          p = population 20 3 1 es
          -- stream pools are fixed, so scan pushes until one hits an unread stream
          counts = [expectedDeliveries p.subscribers (fst (genPush (mkPayload 0) es (mkStdGen s))) | s <- [1 .. 200]]
      (0 `elem` counts, all (`elem` [0, 1]) counts) `shouldBe` (True, True)

  describe "coverage" $ do
    it "is the share of pool streams with a reader" $
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
       in coverage (population 20 3 1 es).subscribers es `shouldBe` [(KindUser, 0.2)]
    it "is 1 when every stream has a reader" $
      let es = entriesOf 1 [TargetConfig KindTeam 1 1, TargetConfig KindUser 10 1]
       in map snd (coverage (population 10 3 1 es).subscribers es) `shouldBe` [1, 1]
```

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "mkPopulation"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile errors (`readers`, `readerStreams`, `SimUser` not defined; `mkPopulation` arity).

- [ ] **Step 3: Rewrite `Population.hs`**

Keep `Key`, `streamKind`, `targetKeys`, `expectedDeliveries`, `coverage` as they are. Replace the rest:

```haskell
-- | The simulated readers: teams of users, each user with its clients. Derived
-- from the pools the writers use, so readers poll streams that get written.
-- A reader is one client of one user and fetches and acks as that
-- (user, client): 'readerStreams' derives its user and client streams from
-- the same pair.
module FanInPerf.Population
  ( SimTeam (..),
    SimUser (..),
    Reader (..),
    Key,
    streamKey,
    Population (..),
    mkPopulation,
    readers,
    readerStreams,
    streamKind,
    expectedDeliveries,
    coverage,
  )
where

import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Targets
import Imports
import System.Random (StdGen)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.FanInNotificationsStore

data SimTeam = SimTeam
  { tid :: !TeamId,
    members :: !(NonEmpty SimUser)
  }

data SimUser = SimUser
  { uid :: !UserId,
    -- | [] without 'epoch' in --targets; shared by all clients of the user
    epochs :: ![(GroupId, Epoch)],
    -- | 1..k, one reader thread each
    clients :: !(NonEmpty ClientId)
  }

-- | What one reader thread needs.
data Reader = Reader
  { team :: !(Maybe TeamId),
    user :: !SimUser,
    cid :: !ClientId
  }

data Population = Population
  { -- | 'team' in --targets: every user is in exactly one team ...
    teams :: [SimTeam],
    -- | ... otherwise all users are here.
    personalUsers :: [SimUser],
    -- | kinds present in --targets
    kinds :: Set TargetKind,
    -- | Number of readers of each stream (streams nobody reads are absent).
    subscribers :: Map Key Int
  }

readers :: Population -> [Reader]
readers p =
  [Reader (Just t.tid) u c | t <- p.teams, u <- toList t.members, c <- toList u.clients]
    <> [Reader Nothing u c | u <- p.personalUsers, c <- toList u.clients]

-- | The only place reader streams come from.
readerStreams :: Set TargetKind -> Reader -> [Stream]
readerStreams kinds r =
  [StreamUser r.user.uid | KindUser `Set.member` kinds]
    <> [StreamClient r.user.uid r.cid | KindClients `Set.member` kinds]
    <> [StreamTeam t | Just t <- [r.team]]
    <> [uncurry StreamEpoch e | e <- r.user.epochs]

-- | @mkPopulation clients epochsPerClient clientsPerUser entries gen@. Users
-- are the shared uids of the user/clients pools ('mkEntries'), otherwise
-- @ceil (clients / clientsPerUser)@ synthetic ones drawn from @gen@. Clients
-- go round-robin over the users: user @u@ gets client ids @1..k_u@ (counts
-- differ by at most one); users without clients are left out. User @u@ is in
-- team @u mod S_t@ and reads @E@ consecutive epoch streams from @u * E@.
-- 'entries' is never empty (the CLI requires at least one target entry).
mkPopulation :: Int -> Int -> Int -> V.Vector Entry -> StdGen -> Either Text Population
mkPopulation n epochs clientsPerUser entries gen = do
  when (any ((== KindConnections) . (.spec.kind)) es) $
    Left "connections targets are not supported by consume"
  let kinds = Set.fromList (map (.spec.kind) es)
      uids = fromMaybe (V.unfoldrExactN (ceilDiv n clientsPerUser) genId gen) (poolUsers es)
      s = V.length uids
  when (KindClients `Set.member` kinds && n > s * clientsPerUser) $
    Left ("clients: " <> showT n <> " clients need at least that many (user, client id) pairs, the pool has " <> showT (s * clientsPerUser))
  epochsOf <- userEpochs epochs es
  let users = [(u, SimUser (uids V.! u) (epochsOf u) (clientIds (ceilDiv (n - u) s))) | u <- [0 .. min n s - 1]]
      (teams, personalUsers) = case [v | Entry _ (TeamPool v) <- es] of
        v : _ -> (groupTeams v users, [])
        [] -> ([], map snd users)
      p = Population {teams, personalUsers, kinds, subscribers = mempty}
  pure p {subscribers = Map.fromListWith (+) [(streamKey st, 1) | r <- readers p, st <- readerStreams kinds r]}
  where
    es = V.toList entries
    ceilDiv a b = (a + b - 1) `div` b
    clientIds k = ClientId 1 :| map ClientId [2 .. fromIntegral k]
    showT = T.pack . show

-- | The longer of the user and clients pools holds all shared uids (both are
-- prefixes of one vector, see 'mkEntries').
poolUsers :: [Entry] -> Maybe (V.Vector UserId)
poolUsers es =
  listToMaybe . sortOn (Down . V.length) $
    [v | Entry _ (UserPool v) <- es] <> [V.map fst v | Entry _ (ClientsPool v) <- es]

-- | @E@ consecutive epoch streams per user index; none without 'epoch'.
userEpochs :: Int -> [Entry] -> Either Text (Int -> [(GroupId, Epoch)])
userEpochs e es = case [v | Entry _ (EpochPool v) <- es] of
  [] -> Right (const [])
  v : _
    | e > V.length v -> Left ("epoch: --epochs-per-client " <> T.pack (show e) <> " exceeds the " <> T.pack (show (V.length v)) <> " epoch streams")
    | otherwise -> Right (\u -> [v V.! ((u * e + j) `mod` V.length v) | j <- [0 .. e - 1]])

-- | User @u@ joins team @u mod S_t@; teams without members are left out.
groupTeams :: V.Vector TeamId -> [(Int, SimUser)] -> [SimTeam]
groupTeams v users =
  [ SimTeam (v V.! t) ms
  | (t, ms) <- Map.toList (Map.fromListWith (flip (<>)) [(u `mod` V.length v, su :| []) | (u, su) <- users])
  ]
```

Keep `streamKey` as is (now exported). Delete `Client` and `clientStreams`. `Down` comes from `Data.Ord` if `Imports` lacks it.

- [ ] **Step 4: Adapt `Produce` and `Consume`**

`Produce.hs`: `WriterSetup` gets `populationGen :: StdGen`; in `prepareWriters`:

```haskell
  let (entries, gen') = mkEntries opts.clientsPerUser opts.targets (mkStdGen seed)
      (populationGen, gen'') = split gen'
      payload = mkPayload opts.payloadBytes
      gens = take opts.writers (unfoldr (Just . split) gen'')
```

`Consume.hs`:
- `loadCursors :: (Member FanInNotificationsStore r) => [Stream] -> Sem r [Subscription]`, iterating the given streams (body otherwise unchanged).
- `runConsume`: `mkPopulation opts.clients opts.epochsPerClient p.clientsPerUser setup.entries setup.populationGen`.
- `loadSubscriptions`: `forConcurrently (readers population) (throughGate gate . runStore env . loadCursors . readerStreams population.kinds)`.
- `readerStats <- replicateM opts.clients newReaderStats` stays (`length (readers population) == opts.clients`); `mapConcurrently_ readerClient (zip readerStats clientSubs)` is unchanged.

`ConsumeSpec.hs`: drop the `Client` import; `loadCursors (Client [team])` → `loadCursors [team]` (3 places).

- [ ] **Step 5: Run all tool tests, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings.

- [ ] **Step 6: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Population.hs tools/fan-in-perf/src/FanInPerf/Produce.hs tools/fan-in-perf/src/FanInPerf/Consume.hs tools/fan-in-perf/test/FanInPerf/PopulationSpec.hs tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs
git commit -m "fan-in-perf: coherent population (teams, users, clients)"
```

---

### Task 4: `consume-ack` options, `ReaderMode`, experiment label

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Options.hs`
- Modify: `tools/fan-in-perf/src/FanInPerf/Consume.hs` (label instead of `"consume"` literals)
- Test: `tools/fan-in-perf/test/FanInPerf/OptionsSpec.hs`

**Interfaces:**
- Produces:
  ```haskell
  data ReaderMode = NoAcks | Acks {ackEvery :: Int} deriving (Eq, Show)
  experimentLabel :: ReaderMode -> Text   -- "consume" | "consume-ack"
  -- ConsumeOptions gains (last field): mode :: ReaderMode
  ```

- [ ] **Step 1: Write the failing tests**

Add to `OptionsSpec.hs`:

```haskell
  it "parses consume as NoAcks" $ do
    o <- parseOk ["--db", "x", "consume", "--targets", "user:10"]
    case o.command of
      Consume c -> c.mode `shouldBe` NoAcks
      _ -> expectationFailure "not a consume command"

  it "parses consume-ack with defaults and --ack-every" $ do
    o <- parseOk ["--db", "x", "consume-ack", "--targets", "user:10,clients:10,team:1"]
    case o.command of
      Consume c -> (c.mode, c.clients) `shouldBe` (Acks 100, 2000)
      _ -> expectationFailure "not a consume command"
    o1 <- parseOk ["--db", "x", "consume-ack", "--targets", "team:1", "--ack-every", "1"]
    fmap (.mode) (consumeOf o1) `shouldBe` Just (Acks 1)
    o2 <- parseOk ["--db", "x", "consume-ack", "--targets", "team:1", "--ack-every", "10000"]
    fmap (.mode) (consumeOf o2) `shouldBe` Just (Acks 10000)

  it "rejects bad consume-ack input" $ do
    let ack extra = ["--db", "x", "consume-ack", "--targets", "team:1"] <> extra
    isNothing (parse (ack ["--ack-every", "0"])) `shouldBe` True
    isNothing (parse (ack ["--ack-every", "10001"])) `shouldBe` True
    -- one identity per reader: user and clients pools must have the same size
    isNothing (parse ["--db", "x", "consume-ack", "--targets", "user:10,clients:5"]) `shouldBe` True
    isNothing (parse ["--db", "x", "consume-ack", "--targets", "connections:5"]) `shouldBe` True
    -- consume keeps accepting different sizes and has no --ack-every
    isNothing (parse ["--db", "x", "consume", "--targets", "user:10,clients:5"]) `shouldBe` False
    isNothing (parse ["--db", "x", "consume", "--targets", "user:10", "--ack-every", "5"]) `shouldBe` True

  it "labels the experiments" $
    map experimentLabel [NoAcks, Acks 1] `shouldBe` ["consume", "consume-ack"]
```

with helper (top level in the spec module):

```haskell
consumeOf :: Options -> Maybe ConsumeOptions
consumeOf o = case o.command of
  Consume c -> Just c
  _ -> Nothing
```

Import `ReaderMode (..)`, `experimentLabel`, `ConsumeOptions (..)` from `FanInPerf.Options` if the import list is explicit.

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "consume"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: `ReaderMode` / `mode` not in scope.

- [ ] **Step 3: Implement**

In `Options.hs`:
- Export `ReaderMode (..)`, `experimentLabel`.
- Add after `ConsumeOptions`:

```haskell
-- | Whether reader clients acknowledge what they received (experiment C) or
-- only keep cursors in memory (B).
data ReaderMode = NoAcks | Acks {ackEvery :: Int}
  deriving (Eq, Show)

-- | Value of the @experiment@ metric label and prefix of terminal lines.
experimentLabel :: ReaderMode -> Text
experimentLabel = \case
  NoAcks -> "consume"
  Acks _ -> "consume-ack"
```

- `ConsumeOptions` gains last field `mode :: ReaderMode`.
- Subcommands:

```haskell
          <> O.command "consume" (info (Consume <$> consumeParserWith consumeTargetsReader (pure NoAcks)) (progDesc "Experiment B: maximal rate of adding and consuming notifications by simulated clients"))
          <> O.command "consume-ack" (info (Consume <$> consumeParserWith consumeAckTargetsReader ackModeParser) (progDesc "Experiment C: like consume, but every client acknowledges the notifications it received"))
```

- Rename `consumeParser` to `consumeParserWith :: ReadM (NonEmpty TargetConfig) -> Parser ReaderMode -> Parser ConsumeOptions`, using the targets reader for `produceParserWith` and appending `<*> modeParser`.
- Add:

```haskell
ackModeParser :: Parser ReaderMode
ackModeParser =
  Acks <$> option (positive maxAckEvery) (long "ack-every" <> metavar "N" <> value 100 <> showDefault <> help "A client acknowledges after every N-th notification it received (counted over all its streams)")

-- | A reader fetches and acks as one (user, client): its user and client
-- streams must come from the same users (see "FanInPerf.Population").
consumeAckTargetsReader :: ReadM (NonEmpty TargetConfig)
consumeAckTargetsReader = do
  ts <- consumeTargetsReader
  let streamsOf k = [t.streams | t <- toList ts, t.kind == k]
  case (streamsOf KindUser, streamsOf KindClients) of
    ([u], [c]) | u /= c -> readerError "'consume-ack' needs the same STREAMS for 'user' and 'clients' (one identity per reader)"
    _ -> pure ts

maxAckEvery :: Integer
maxAckEvery = 10_000
```

In `Consume.hs` `runConsume`: `let label = experimentLabel opts.mode`; use `label` for `newMetrics`, `newConsumeMetrics` and every `printLine console "consume: …"` prefix (`label <> ": …"`), also in `loadSubscriptions` and `stopAndDrain` (pass `label` as an argument).

- [ ] **Step 4: Run all tool tests, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings.

- [ ] **Step 5: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Options.hs tools/fan-in-perf/src/FanInPerf/Consume.hs tools/fan-in-perf/test/FanInPerf/OptionsSpec.hs
git commit -m "fan-in-perf: consume-ack options"
```

---

### Task 5: Pure ack state (`FanInPerf.Acks`)

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Acks.hs`
- Create: `tools/fan-in-perf/test/FanInPerf/AcksSpec.hs`
- Modify: `tools/fan-in-perf/fan-in-perf.cabal` (library `exposed-modules`/`other-modules` list with `FanInPerf.Consume` etc.: add `FanInPerf.Acks`; test-suite `other-modules`: add `FanInPerf.AcksSpec`)

**Interfaces:**
- Consumes: `Key`, `streamKey` from `FanInPerf.Population` (Task 3).
- Produces:
  ```haskell
  data Pending = Pending {stream :: !Stream, maxId :: !UUID, count :: !Int} deriving (Eq, Show)
  data AckState  -- abstract, Eq, Show
  emptyAckState :: AckState
  unacked :: AckState -> Int
  deliver :: Int -> Stream -> UUID -> AckState -> ([Pending], AckState)
  restore :: [Pending] -> AckState -> AckState
  ```

- [ ] **Step 1: Write the failing tests**

`AcksSpec.hs`:

```haskell
module FanInPerf.AcksSpec (spec) where

import Data.Id
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import FanInPerf.Acks
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wire.FanInNotificationsStore (Stream (..))

user, team :: Stream
user = StreamUser (Id (UUID.fromWords 5 6 7 8))
team = StreamTeam (Id (UUID.fromWords 1 2 3 4))

nid :: Word32 -> UUID
nid = UUID.fromWords 0 0 0

-- | Delivers in order, ignoring due acks.
deliverAll :: Int -> [(Stream, Word32)] -> AckState -> AckState
deliverAll n xs st = foldl' (\s (str, i) -> snd (deliver n str (nid i) s)) st xs

byStream :: [Pending] -> [Pending]
byStream = sortOn (show . (.stream))

spec :: Spec
spec = do
  describe "deliver" $ do
    it "acks every notification with N = 1" $ do
      let (due, st) = deliver 1 user (nid 1) emptyAckState
      due `shouldBe` [Pending user (nid 1) 1]
      unacked st `shouldBe` 0

    it "is due nothing below N and counts over all streams" $ do
      let (d1, s1) = deliver 3 user (nid 1) emptyAckState
          (d2, s2) = deliver 3 team (nid 2) s1
      (d1, d2, unacked s2) `shouldBe` ([], [], 2)

    it "at the N-th returns every pending stream with its maximum id and clears the state" $ do
      let st = deliverAll 4 [(user, 1), (team, 5), (user, 3)] emptyAckState
          (due, st') = deliver 4 team (nid 2) st
      byStream due `shouldBe` byStream [Pending user (nid 3) 2, Pending team (nid 5) 2]
      st' `shouldBe` emptyAckState

  describe "restore" $
    it "keeps the higher id and the count of a failed ack, so the next ack is due earlier" $ do
      let st = restore [Pending user (nid 9) 3] (deliverAll 10 [(user, 4)] emptyAckState)
      unacked st `shouldBe` 4
      fst (deliver 6 user (nid 5) st) `shouldBe` []
      fst (deliver 5 user (nid 5) st) `shouldBe` [Pending user (nid 9) 5]

  prop "unacked equals the deliveries since the last ack point" $ \(Positive n0) (xs :: [Bool]) ->
    let n = 1 + n0 `mod` 10
        step (st, since) b =
          let (due, st') = deliver n (if b then user else team) (nid 1) st
           in (st', if null due then since + 1 else 0)
        (final, since) = foldl' step (emptyAckState, 0 :: Int) xs
     in unacked final == since
```

Add `FanInPerf.Acks` / `FanInPerf.AcksSpec` to the cabal file (alphabetical, next to existing modules).

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "deliver"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: module `FanInPerf.Acks` not found.

- [ ] **Step 3: Implement `Acks.hs`**

```haskell
-- | When a simulated client acknowledges. Like the RFC's delivery tags (per
-- websocket, across streams) with @multiple = true@: once the client received
-- N notifications since its last ack, it acks every stream it received
-- something on, each up to the highest id it got there.
module FanInPerf.Acks
  ( Pending (..),
    AckState,
    emptyAckState,
    unacked,
    deliver,
    restore,
  )
where

import Data.Map.Strict qualified as Map
import Data.UUID.Types (UUID)
import FanInPerf.Population (Key, streamKey)
import Imports
import Wire.FanInNotificationsStore (Stream)

-- | Received, not yet acknowledged notifications of one stream.
data Pending = Pending
  { stream :: !Stream,
    -- | highest delivered id
    maxId :: !UUID,
    -- | delivered since this stream's last ack
    count :: !Int
  }
  deriving (Eq, Show)

-- | Streams with unacknowledged notifications; the single source of truth
-- (no separate counter that could drift).
newtype AckState = AckState (Map Key Pending)
  deriving (Eq, Show)

emptyAckState :: AckState
emptyAckState = AckState mempty

unacked :: AckState -> Int
unacked (AckState m) = sum (map (.count) (Map.elems m))

-- | Merges entries (e.g. of failed acks) into the state: higher id, counts
-- added, so their notifications stay unacknowledged.
restore :: [Pending] -> AckState -> AckState
restore ps (AckState m) = AckState (foldl' (\acc p -> Map.insertWith merge (streamKey p.stream) p acc) m ps)
  where
    merge new old = old {maxId = max old.maxId new.maxId, count = old.count + new.count}

-- | @deliver n stream id state@: one received notification. Once 'unacked'
-- reaches @n@, returns every pending stream and clears the state.
deliver :: Int -> Stream -> UUID -> AckState -> ([Pending], AckState)
deliver n s i st =
  let AckState m = restore [Pending s i 1] st
   in if unacked (AckState m) >= n then (Map.elems m, emptyAckState) else ([], AckState m)
```

`Key` needs `Show` for the derived instance (it derives `Show` already).

- [ ] **Step 4: Run, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings.

- [ ] **Step 5: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Acks.hs tools/fan-in-perf/test/FanInPerf/AcksSpec.hs tools/fan-in-perf/fan-in-perf.cabal
git commit -m "fan-in-perf: pure ack state"
```

---

### Task 6: Ack stats

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/ConsumeStats.hs`
- Test: `tools/fan-in-perf/test/FanInPerf/ConsumeStatsSpec.hs`; adjust `tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs` (`creport` gets the new fields)

**Interfaces:**
- Produces:
  ```haskell
  recordAck :: ReaderStats -> TargetKind -> Word64 -> IO ()   -- latency ns
  recordAckError :: ReaderStats -> TargetKind -> IO ()
  acksOf, ackErrorsOf :: TargetKind -> ReaderSnapshot -> Int
  totalAcks, totalAckErrors :: ReaderSnapshot -> Int
  ackLatencyBuckets :: ReaderSnapshot -> VU.Vector Int
  -- ConsumeReport gains: ackRate, maxAckRate, ackErrorRate :: Double; ackP50, ackP99 :: Maybe Double
  ```

- [ ] **Step 1: Write the failing tests**

Add to `ConsumeStatsSpec.hs` (`describe "ReaderStats"` and `describe "consumeTick"`):

```haskell
    it "records, sums and diffs acks and ack errors without counting fetches" $ do
      a <- newReaderStats
      b <- newReaderStats
      recordAck a KindTeam 1000
      s0 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      recordAck a KindTeam 1000
      recordAck b KindEpoch 2000
      recordAckError b KindEpoch
      s1 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      let d = diffReaderSnapshot s1 s0
      (acksOf KindTeam s1, acksOf KindTeam d, acksOf KindEpoch d, ackErrorsOf KindEpoch d) `shouldBe` (2, 1, 1, 1)
      (totalAcks s1, totalAckErrors s1, VU.sum (ackLatencyBuckets d), totalFetches s1) `shouldBe` (3, 1, 2, 0)
```

```haskell
    it "computes the ack rate, its max after warmup and ack latency quantiles" $ do
      rs <- newReaderStats
      replicateM_ 20 (recordAck rs KindUser 1000)
      recordAckError rs KindUser
      t1 <- readReaderSnapshot rs
      let (r1, st1) = consumeTick 5 1 0 t1 (initialConsumeTickState 0)
      (r1.ackRate, r1.maxAckRate, r1.ackErrorRate) `shouldBe` (20, 0, 1)
      isJust r1.ackP99 `shouldBe` True
      replicateM_ 60 (recordAck rs KindUser 1000)
      t2 <- readReaderSnapshot rs
      let (r2, _) = consumeTick 5 6 0 t2 st1
      (r2.ackRate, r2.maxAckRate) `shouldBe` (12, 12)
      let (r0, _) = consumeTick 0 0 0 emptyReaderSnapshot (initialConsumeTickState 0)
      (r0.ackRate, r0.ackP50, r0.ackP99) `shouldBe` (0, Nothing, Nothing)
```

In `TerminalSpec.hs` `creport`, add `ackRate = 0, maxAckRate = 0, ackErrorRate = 0, ackP50 = Nothing, ackP99 = Nothing`.

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "ReaderStats"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: `recordAck` not in scope.

- [ ] **Step 3: Implement**

In `ConsumeStats.hs`:
- `ReaderSnapshot` gains `acks :: !(Map TargetKind Int)`, `ackErrors :: !(Map TargetKind Int)`, `ackLatency :: !(IntMap Int)`.
- `emptyReaderSnapshot` in record syntax with all nine fields `mempty`.
- `sumReaderSnapshots` / `diffReaderSnapshot`: same pattern for the three new fields (`Map.unionWith (+)` / `diffMap`; `IntMap.unionWith (+)` / negated old).
- New functions:

```haskell
-- | One ack upsert of a stream of the given kind and its latency (ns).
recordAck :: ReaderStats -> TargetKind -> Word64 -> IO ()
recordAck stats k latencyNs =
  modifyReader stats $ \s ->
    s
      { acks = bump k 1 s.acks,
        ackLatency = IntMap.insertWith (+) (bucketIndex latencyNs) 1 s.ackLatency
      }

recordAckError :: ReaderStats -> TargetKind -> IO ()
recordAckError stats k = modifyReader stats $ \s -> s {ackErrors = bump k 1 s.ackErrors}

acksOf, ackErrorsOf :: TargetKind -> ReaderSnapshot -> Int
acksOf k s = Map.findWithDefault 0 k s.acks
ackErrorsOf k s = Map.findWithDefault 0 k s.ackErrors

totalAcks, totalAckErrors :: ReaderSnapshot -> Int
totalAcks s = sum s.acks
totalAckErrors s = sum s.ackErrors

ackLatencyBuckets :: ReaderSnapshot -> VU.Vector Int
ackLatencyBuckets s = toBuckets s.ackLatency
```

- `ConsumeTickState` gains `peakAckRate :: Double` (`initialConsumeTickState now = ConsumeTickState now now emptyReaderSnapshot 0 0`, or record syntax).
- `ConsumeReport` gains `ackRate, maxAckRate, ackErrorRate :: Double` and `ackP50, ackP99 :: Maybe Double` (doc: "upserts per second; 0 / Nothing without acks").
- `consumeTick`:

```haskell
      afterWarmup = elapsed > warmup && dt >= minTickInterval
      peak = if afterWarmup then max st.peakRate consumeRate else st.peakRate
      ackRate = rate (totalAcks delta)
      peakAck = if afterWarmup then max st.peakAckRate ackRate else st.peakAckRate
```

  report fields `ackRate`, `maxAckRate = peakAck`, `ackErrorRate = rate (totalAckErrors delta)`, `ackP50 = quantileSeconds 0.5 (ackLatencyBuckets total)`, `ackP99 = quantileSeconds 0.99 (ackLatencyBuckets total)`; state `peakAckRate = peakAck`.
- Export all new names.

- [ ] **Step 4: Run, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings.

- [ ] **Step 5: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/ConsumeStats.hs tools/fan-in-perf/test/FanInPerf/ConsumeStatsSpec.hs tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs
git commit -m "fan-in-perf: ack stats"
```

---

### Task 7: Ack metrics

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Metrics.hs`
- Test: `tools/fan-in-perf/test/FanInPerf/MetricsSpec.hs`

**Interfaces:**
- Consumes: Task 6 accessors.
- Produces: `AckMetrics`, `newAckMetrics :: Text -> IO AckMetrics` (register once per process), `publishAcks :: AckMetrics -> ConsumeReport -> IO ()` (ticker thread only).

- [ ] **Step 1: Write the failing test**

```haskell
  describe "publishAcks" $
    it "exports ack counters, gauges and the ack latency histogram" $ do
      m <- newAckMetrics "speca"
      rs <- newReaderStats
      replicateM_ 3 (recordAck rs KindTeam 1000)
      recordAckError rs KindTeam
      total <- readReaderSnapshot rs
      let (r, _) = consumeTick 0 1 0 total (initialConsumeTickState 0)
      publishAcks m r
      out <- LBS8.unpack <$> P.exportMetricsAsText
      out `shouldContain` "fanin_perf_acks_total{experiment=\"speca\",kind=\"team\"} 3"
      out `shouldContain` "fanin_perf_ack_errors_total{experiment=\"speca\",kind=\"team\"} 1"
      out `shouldContain` "fanin_perf_ack_rate_current{experiment=\"speca\"} 3"
      out `shouldContain` "fanin_perf_ack_duration_seconds_count{experiment=\"speca\"} 3"
```

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "publishAcks"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: `newAckMetrics` not in scope.

- [ ] **Step 3: Implement**

In `Metrics.hs` (export `AckMetrics`, `newAckMetrics`, `publishAcks`):

```haskell
data AckMetrics = AckMetrics
  { experiment :: Text,
    ackCount :: P.Vector P.Label2 P.Counter,
    ackErrorCount :: P.Vector P.Label2 P.Counter,
    ackRateCurrent :: P.Vector P.Label1 P.Gauge,
    ackRateMax :: P.Vector P.Label1 P.Gauge,
    ackLatencyRef :: IORef (VU.Vector Int)
  }

-- | Registers the ack metrics; call once per process and only when readers
-- acknowledge, so B's /metrics stay unchanged.
newAckMetrics :: Text -> IO AckMetrics
newAckMetrics experiment = do
  ackCount <- counterVec "fanin_perf_acks_total" "Ack upserts (one per stream at an ack point)"
  ackErrorCount <- counterVec "fanin_perf_ack_errors_total" "Failed ack upserts"
  ackRateCurrent <- gaugeVec "fanin_perf_ack_rate_current" "Ack upserts per second during the last tick"
  ackRateMax <- gaugeVec "fanin_perf_ack_rate_max" "Maximal ack upserts per second after warmup"
  ackLatencyRef <- newIORef (VU.replicate numBuckets 0)
  _ <- P.register (histogramMetric "fanin_perf_ack_duration_seconds" "Ack upsert latency in seconds" experiment ackLatencyRef)
  pure AckMetrics {..}

-- | Only the ticker thread calls this.
publishAcks :: AckMetrics -> ConsumeReport -> IO ()
publishAcks m r = do
  for_ allKinds $ \k -> do
    let lbl = (m.experiment, kindName k)
    add m.ackCount lbl (acksOf k r.readerDelta)
    add m.ackErrorCount lbl (ackErrorsOf k r.readerDelta)
  set m.ackRateCurrent r.ackRate
  set m.ackRateMax r.maxAckRate
  writeIORef m.ackLatencyRef (ackLatencyBuckets r.readerTotal)
  where
    add v lbl n = when (n > 0) $ P.withLabel v lbl (void . (`P.addCounter` fromIntegral n))
    set g x = P.withLabel g m.experiment (`P.setGauge` x)
```

(`RecordWildCards` must be on for `AckMetrics {..}`; `newMetrics` already uses it. If the `add`/`set` helpers now exist three times, lift them to top level as `addCounter :: P.Vector P.Label2 P.Counter -> (Text, Text) -> Int -> IO ()` and `setGauge :: Text -> P.Vector P.Label1 P.Gauge -> Double -> IO ()` and use them in all three publish functions.)

- [ ] **Step 4: Run, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings.

- [ ] **Step 5: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Metrics.hs tools/fan-in-perf/test/FanInPerf/MetricsSpec.hs
git commit -m "fan-in-perf: ack metrics"
```

---

### Task 8: Mode-aware terminal output

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Terminal.hs`
- Modify: `tools/fan-in-perf/src/FanInPerf/Consume.hs` (call sites, `WriterPhase.acks`)
- Test: `tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs`

**Interfaces:**
- Consumes: `ReaderMode` (Task 4), Task 6 report fields.
- Produces:
  ```haskell
  formatConsumeStatus :: ReaderMode -> TickReport -> ConsumeReport -> Text
  formatConsumeSummary :: ReaderMode -> Int -> WriterPhase -> Drain -> TickReport -> ConsumeReport -> [Text]
  data WriterPhase = WriterPhase {duration :: Double, delivered :: Int, acks :: Int}
  ```

- [ ] **Step 1: Write the failing tests**

In `TerminalSpec.hs`: `phase = WriterPhase {duration = 30, delivered = 600, acks = 300}`; every existing `formatConsumeStatus report creport` → `formatConsumeStatus NoAcks report creport`, every `formatConsumeSummary x …` → `formatConsumeSummary NoAcks x …`; the empty-run `WriterPhase {…}` gets `acks = 0`. Import `FanInPerf.Options (ReaderMode (..))`. The existing NoAcks status string stays exactly as it is (Review Focus 5). Add:

```haskell
  describe "formatConsumeStatus with acks" $
    it "adds the ack segment and ack errors" $
      formatConsumeStatus (Acks 100) report creport {ackRate = 500.4, maxAckRate = 600, ackP99 = Just 0.002, ackErrorRate = 2}
        `shouldBe` "t=42s push/s cur=8,312 max=9,105 | cons/s cur=8,000 max=9,000 | ack/s cur=500 max=600 p99(cum.)=2.0ms | lag(cum.) p50=4.0ms p99=50.0ms | backlog=1,234 | err/s push=3 fetch=1 ack=2"

  describe "formatConsumeSummary with acks" $ do
    it "reports acks, ack errors, ack/s over the writer phase and ack latency" $ do
      rs <- newReaderStats
      replicateM_ 4 (recordAck rs KindUser 1000)
      recordAckError rs KindUser
      s <- readReaderSnapshot rs
      let out = formatConsumeSummary (Acks 100) 0 phase (DrainComplete 1) report creport {readerTotal = s, maxAckRate = 50, ackP50 = Just 0.001, ackP99 = Just 0.002}
      out `shouldSatisfy` any (T.isInfixOf "acks=4 errors=1 ack/s avg=10 max=50")
      out `shouldSatisfy` any (T.isInfixOf "ack latency (cumulative) p50=1.0ms p99=2.0ms")
    it "prints no ack line without acks" $
      formatConsumeSummary NoAcks 0 phase (DrainComplete 1) report creport
        `shouldSatisfy` (not . any (T.isInfixOf "acks="))
    it "survives a run without a single ack" $
      formatConsumeSummary (Acks 10000) 0 WriterPhase {duration = 0, delivered = 0, acks = 0} (DrainComplete 0) report {elapsed = 0} creport
        `shouldSatisfy` (\ls -> any (T.isInfixOf "acks=0") ls && not (any (T.isInfixOf "NaN") ls))
```

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "formatConsume"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: arity / missing field errors.

- [ ] **Step 3: Implement**

`Terminal.hs` (import `FanInPerf.Options (ReaderMode (..))`):

```haskell
formatConsumeStatus :: ReaderMode -> TickReport -> ConsumeReport -> Text
formatConsumeStatus mode r c =
  T.intercalate " | " $
    [ "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> rateText r.maxPushRate,
      "cons/s cur=" <> rateText c.consumeRate <> " max=" <> rateText c.maxConsumeRate
    ]
      <> ["ack/s cur=" <> rateText c.ackRate <> " max=" <> rateText c.maxAckRate <> " p99(cum.)=" <> formatLatency c.ackP99 | acking]
      <> [ "lag(cum.) p50=" <> formatLatency c.lagP50 <> " p99=" <> formatLatency c.lagP99,
           "backlog=" <> groupThousands c.backlog,
           "err/s push=" <> rateText r.errorRate <> " fetch=" <> rateText c.fetchErrorRate <> (if acking then " ack=" <> rateText c.ackErrorRate else "")
         ]
  where
    acking = mode /= NoAcks
```

`WriterPhase` gains `acks :: Int` (doc: "ack upserts by then"). `formatConsumeSummary mode expected phase drain r c`: same lines as today, plus before the drain line, only when `mode /= NoAcks`:

```haskell
      avgAckRate = if phase.duration > 0 then fromIntegral phase.acks / phase.duration else 0
      ackLines =
        [ "  acks=" <> groupThousands (totalAcks c.readerTotal) <> " errors=" <> groupThousands (totalAckErrors c.readerTotal) <> " ack/s avg=" <> rateText avgAckRate <> " max=" <> rateText c.maxAckRate,
          "  ack latency (cumulative) p50=" <> formatLatency c.ackP50 <> " p99=" <> formatLatency c.ackP99
        ]
```

`Consume.hs`:
- `consumeTickOnce` gets a `ReaderMode` argument and calls `formatConsumeStatus mode …`.
- `writerPhaseEnd` also reads the ack total:

```haskell
      writerPhaseEnd = do
        now <- getMonotonicTime
        readerTotal <- sumReaderSnapshots <$> traverse readReaderSnapshot readerStats
        pure WriterPhase {duration = now - start, delivered = totalDelivered readerTotal, acks = totalAcks readerTotal}
```

- Final summary: `formatConsumeSummary opts.mode …`.

- [ ] **Step 4: Run, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings.

- [ ] **Step 5: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Terminal.hs tools/fan-in-perf/src/FanInPerf/Consume.hs tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs
git commit -m "fan-in-perf: ack status line and summary"
```

---

### Task 9: Reader refactor — `PassResult`, `isolated`, mode-aware start positions

B behaviour stays the same; this prepares the loop for acks.

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Consume.hs`
- Test: `tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs`

**Interfaces:**
- Consumes: `Reader`, `readerStreams` (Task 3), `ReaderMode` (Task 4), `initAcks` (Task 1).
- Produces:
  ```haskell
  data PassResult = PassResult
    { subs :: [Subscription],
      more :: Bool,                   -- some page was full
      delivered :: [(Stream, UUID)],  -- newly delivered ids, page order
      errors :: [Text]
    }   -- Semigroup/Monoid: lists concatenated, more = (||)
  readerPass :: … -> Sem r PassResult          -- was a triple
  readerRound :: (e -> Text) -> ReaderStats -> (Subscription -> IO (Either e PassResult)) -> [Subscription] -> IO PassResult
  isolated :: (e -> Text) -> (Text -> IO b) -> IO (Either e b) -> IO b
  startPositions :: Member FanInNotificationsStore r => ReaderMode -> Set TargetKind -> Reader -> Sem r [Subscription]
  -- loadCursors removed
  ```

- [ ] **Step 1: Adapt the test helpers and write the failing tests**

In `ConsumeSpec.hs`:
- A call log for the fake store:

```haskell
data Call = InitCall UserId ClientId Stream | AckCall UserId ClientId Stream UUID
  deriving (Eq, Show)

-- | In-memory store; every fetch fails while the flag is set; ack calls are logged (newest first).
fakeStore :: (Member (Embed IO) r, Member (Error Text) r) => IORef [Call] -> Rows -> IORef Bool -> InterpreterFor FanInNotificationsStore r
fakeStore calls rows failing = interpret $ \case
  PushViaFanIn _ -> pure ()
  FetchNotifications s cursor limit -> do
    down <- readIORef failing
    when down (throw (T.pack "store down"))
    rs <- fromMaybe [] . lookup s <$> readIORef rows
    pure (take limit (filter ((> cursor) . fst) rs))
  GetLastNotificationId s -> lastId s
  InitAcks u c s -> modifyIORef calls (InitCall u c s :) >> lastId s
  AckNotification u c s i -> do
    down <- readIORef failing
    when down (throw (T.pack "store down"))
    modifyIORef calls (AckCall u c s i :)
  where
    lastId s = fmap fst . listToMaybe . reverse . fromMaybe [] . lookup s <$> readIORef rows

runFakeWith :: IORef [Call] -> Rows -> IORef Bool -> Sem '[FanInNotificationsStore, Error Text, Embed IO] a -> IO a
runFakeWith calls rows failing = runM . runError . fakeStore calls rows failing >=> either (fail . T.unpack) pure

runFake :: Rows -> IORef Bool -> Sem '[FanInNotificationsStore, Error Text, Embed IO] a -> IO a
runFake rows failing act = newIORef [] >>= \calls -> runFakeWith calls rows failing act
```

- `pass` / `passW` keep returning triples so the existing tests stay unchanged:

```haskell
passW rows failing limit window stats subs = do
  r <- runFake rows failing (readerPass @Text id limit window stats subs)
  pure (r.subs, r.more, r.errors)
```

- `readerRound` tests: `fetchOne` returns `Right <$> runFake rows failing (readerPass @Text id 10 0 stats [sub])` (a `PassResult`); results are read as `r.subs`, `r.more`, `r.errors` instead of the triple.
- A reader fixture:

```haskell
teamId :: TeamId
teamId = Id (UUID.fromWords 1 2 3 4)

team :: Stream
team = StreamTeam teamId

uid0 :: UserId
uid0 = Id (UUID.fromWords 9 9 9 9)

reader0 :: Reader
reader0 = Reader (Just teamId) (SimUser uid0 [] (ClientId 7 :| [])) (ClientId 7)
```

- Replace the `describe "loadCursors"` block:

```haskell
  describe "startPositions" $ do
    it "NoAcks: starts at the latest notification, without touching acks" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      calls <- newIORef []
      subs <- runFakeWith calls rows failing (startPositions NoAcks (Set.singleton KindTeam) reader0)
      map (.cursor) subs `shouldBe` [fst (last rs)]
      readIORef calls `shouldReturn` []
    it "Acks: resets the reader's acks to the latest notification and starts there" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      calls <- newIORef []
      subs <- runFakeWith calls rows failing (startPositions (Acks 10) (Set.singleton KindTeam) reader0)
      map (.cursor) subs `shouldBe` [fst (last rs)]
      readIORef calls `shouldReturn` [InitCall uid0 (ClientId 7) team]
    it "starts at the nil UUID for a stream nothing was written to" $ do
      (rows, failing) <- setup []
      subs <- runFake rows failing (startPositions (Acks 10) (Set.singleton KindTeam) reader0)
      map (.cursor) subs `shouldBe` [UUID.nil]
```

- The window test "never re-fetches below the start position" uses `startPositions NoAcks (Set.singleton KindTeam) reader0` instead of `loadCursors [team]`.
- New `readerPass` test:

```haskell
    it "reports the delivered ids with their stream in page order" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      stats <- newReaderStats
      r <- runFake rows failing (readerPass @Text id 10 0 stats [sub0])
      r.delivered `shouldBe` [(team, i) | (i, _) <- rs]
```

- New `isolated` tests:

```haskell
  describe "isolated" $ do
    it "passes a result through" $
      isolated @Text id (\_ -> pure 0) (pure (Right (1 :: Int))) `shouldReturn` 1
    it "turns an error result and a synchronous exception into the failure branch" $ do
      isolated @Text id (pure . T.length) (pure (Left "down")) `shouldReturn` 4
      isolated @Text id (pure . T.length) (throwIO (ErrorCall "boom")) `shouldReturn` 4
```

Imports: `Data.Set qualified as Set`, `Data.List.NonEmpty (NonEmpty (..))`, `FanInPerf.Options (ReaderMode (..))`, `FanInPerf.Population (Reader (..), SimUser (..))`.

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "startPositions"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: `startPositions`, `PassResult`, `isolated` not in scope.

- [ ] **Step 3: Implement in `Consume.hs`**

```haskell
-- | Result of fetching some streams of one client once.
data PassResult = PassResult
  { subs :: [Subscription],
    -- | some stream returned a full page: more is waiting
    more :: Bool,
    -- | newly delivered ids with their stream, in page order
    delivered :: [(Stream, UUID)],
    errors :: [Text]
  }

instance Semigroup PassResult where
  a <> b = PassResult (a.subs <> b.subs) (a.more || b.more) (a.delivered <> b.delivered) (a.errors <> b.errors)

instance Monoid PassResult where
  mempty = PassResult [] False [] []
```

- `readerPass` returns `PassResult`: per sub, `Left` → `PassResult [sub] False [] [kindName sub.kind <> ": fetch failed: " <> render err]`; `Right` → `PassResult [sub' {seen = seen'}] full [(sub.stream, i) | i <- new] []`; combine with `mconcat`. Update its doc comment ("Returns the advanced cursors, …" → "Returns a 'PassResult' …").
- `isolated` and the new `readerRound`:

```haskell
-- | Runs one store call; a synchronous exception or an error result goes to
-- @onFailure@ as text, so one failing stream never affects another. Async
-- exceptions (cancel) propagate.
isolated :: (e -> Text) -> (Text -> IO b) -> IO (Either e b) -> IO b
isolated render onFailure act =
  tryAny act >>= \case
    Left e -> onFailure (T.pack (displayException e))
    Right (Left e) -> onFailure (render e)
    Right (Right b) -> pure b

-- | One pass over the streams of a client, each fetched in isolation by
-- @fetchOne@ (a 'readerPass' over that stream alone): a failure only keeps
-- the cursor of its own stream (counted as fetch error).
readerRound ::
  (e -> Text) ->
  ReaderStats ->
  (Subscription -> IO (Either e PassResult)) ->
  [Subscription] ->
  IO PassResult
readerRound render stats fetchOne subs =
  mconcat <$> for subs (\sub -> isolated render (failed sub) (fetchOne sub))
  where
    -- constructor, not a record update: 'errors' is also a field of 'Metrics'
    failed sub msg = do
      recordFetchError stats sub.kind
      pure (PassResult [sub] False [] ["reader: " <> kindName sub.kind <> ": " <> msg])
```

  Remove the `-- REVIEW: Dependency injection is confusing` comment only if the code it refers to is gone; otherwise leave it.
- `startPositions` replaces `loadCursors`:

```haskell
-- | Start position of every stream of a reader: B reads the latest
-- notification id ("start at latest"); C resets the reader's acks to it (RFC
-- "when nothing is acknowledged"). The nil UUID for an empty stream.
startPositions :: (Member FanInNotificationsStore r) => ReaderMode -> Set TargetKind -> Reader -> Sem r [Subscription]
startPositions mode kinds r =
  for (readerStreams kinds r) $ \s -> do
    start <- case mode of
      NoAcks -> getLastNotificationId s
      Acks _ -> initAcks r.user.uid r.cid s
    pure (newSubscription (streamKind s) s (fromMaybe UUID.nil start))
```

- `readerLoop`: `pollOne` uses `r <- readerRound …; traverse_ reportError r.errors; pure (map (,r.more) r.subs)`.
- `loadSubscriptions console gate env mode population`:

```haskell
loadSubscriptions :: Console -> QSem -> Env -> ReaderMode -> Population -> IO [[Subscription]]
loadSubscriptions console gate env mode population = do
  let label = experimentLabel mode
  printLine console (label <> ": loading start positions")
  loaded <- forConcurrently (readers population) (throughGate gate . runStore env . startPositions mode population.kinds)
  subs <- either (\e -> die ("loading start positions failed: " <> show e)) pure (sequence loaded)
  case mode of
    Acks _ -> printLine console (label <> ": acks reset to latest (" <> tshow (length (concat subs)) <> " streams)")
    NoAcks -> pure ()
  pure subs
```

- `runConsume` calls `loadSubscriptions console gate readerEnv opts.mode population`.
- Export `PassResult (..)`, `isolated`, `startPositions`; drop `loadCursors`.

- [ ] **Step 4: Run, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass (old `readerPass`/`readerRound`/window tests unchanged in substance), no warnings.

- [ ] **Step 5: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Consume.hs tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs
git commit -m "fan-in-perf: PassResult, mode-aware start positions"
```

---

### Task 10: Ack writing in the reader loop, wiring

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Consume.hs`
- Test: `tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs`

**Interfaces:**
- Consumes: `deliver`, `restore`, `emptyAckState`, `Pending` (Task 5); `recordAck`, `recordAckError` (Task 6); `newAckMetrics`, `publishAcks` (Task 7); `isolated`, `PassResult` (Task 9); `ackNotification` (Task 1).
- Produces:
  ```haskell
  ackOne :: Members '[FanInNotificationsStore, Embed IO] r => ReaderStats -> Reader -> Pending -> Sem r ()
  ackPending :: (e -> Text) -> ReaderStats -> (Text -> IO ()) -> (Pending -> IO (Either e ())) -> [Pending] -> IO [Pending]  -- returns failed
  acknowledge :: Int -> ([Pending] -> IO [Pending]) -> AckState -> [(Stream, UUID)] -> IO AckState
  ```

- [ ] **Step 1: Write the failing tests**

```haskell
  describe "ackOne" $
    it "acks as the reader's (user, client) up to the pending id and records the latency" $ do
      [(i, _)] <- newIds 1
      (rows, failing) <- setup []
      calls <- newIORef []
      stats <- newReaderStats
      runFakeWith calls rows failing (ackOne stats reader0 (Pending team i 3))
      readIORef calls `shouldReturn` [AckCall uid0 (ClientId 7) team i]
      s <- readReaderSnapshot stats
      (acksOf KindTeam s, VU.sum (ackLatencyBuckets s)) `shouldBe` (1, 1)

  describe "ackPending" $ do
    it "returns nothing when every ack succeeds" $ do
      stats <- newReaderStats
      [(i, _)] <- newIds 1
      ackPending @Text id stats (\_ -> pure ()) (\_ -> pure (Right ())) [Pending team i 1] `shouldReturn` []

    it "returns, counts and reports failed acks (error result or exception), and keeps going" $ do
      stats <- newReaderStats
      reported <- newIORef []
      [(i, _), (j, _)] <- newIds 2
      let failing p
            | p.stream == team = pure (Left "down")
            | otherwise = throwIO (ErrorCall "boom")
      failed <- ackPending @Text id stats (\m -> modifyIORef reported (m :)) failing [Pending team i 1, Pending user j 2]
      failed `shouldBe` [Pending team i 1, Pending user j 2]
      s <- readReaderSnapshot stats
      (ackErrorsOf KindTeam s, ackErrorsOf KindUser s, totalAcks s) `shouldBe` (1, 1, 0)
      reverse <$> readIORef reported `shouldReturn` ["ack: team: down", "ack: user: boom"]

  describe "acknowledge" $ do
    it "writes acks every N deliveries over all streams" $ do
      written <- newIORef []
      ids <- map fst <$> newIds 5
      let write ps = modifyIORef written (ps :) >> pure []
          delivered = zip [team, user, team, user, team] ids
      st <- acknowledge 2 write emptyAckState delivered
      batches <- reverse <$> readIORef written
      -- a batch lists streams in 'Key' order: user before team
      map (map (\p -> (p.stream, p.count))) batches `shouldBe` [[(user, 1), (team, 1)], [(user, 1), (team, 1)]]
      unacked st `shouldBe` 1

    it "keeps failed acks pending, so they are retried at the next ack point" $ do
      ids <- map fst <$> newIds 3
      st <- acknowledge 1 pure emptyAckState [(team, ids !! 0)] -- write fails: everything comes back
      unacked st `shouldBe` 1
      written <- newIORef []
      _ <- acknowledge 1 (\ps -> modifyIORef written (ps :) >> pure []) st [(team, ids !! 1)]
      readIORef written `shouldReturn` [[Pending team (ids !! 1) 2]]
```

Imports: `FanInPerf.Acks`.

- [ ] **Step 2: Run, expect compile FAIL**

Run: `cabal test fan-in-perf-tests --test-options='-m "ack"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: `ackOne`, `ackPending`, `acknowledge` not in scope.

- [ ] **Step 3: Implement**

In `Consume.hs` (export `ackOne`, `ackPending`, `acknowledge`):

```haskell
-- | Writes one ack as the reader's (user, client) and records its latency
-- (pool wait included, like fetches). A failed write records nothing here
-- ('ackPending' counts it).
ackOne :: (Members '[FanInNotificationsStore, Embed IO] r) => ReaderStats -> Reader -> Pending -> Sem r ()
ackOne stats r p = do
  t0 <- embed getMonotonicTimeNSec
  ackNotification r.user.uid r.cid p.stream p.maxId
  t1 <- embed getMonotonicTimeNSec
  embed (recordAck stats (streamKind p.stream) (t1 - t0))

-- | Writes due acks one by one through @writeOne@; returns the failed ones
-- (counted and reported) so they stay pending.
ackPending :: (e -> Text) -> ReaderStats -> (Text -> IO ()) -> (Pending -> IO (Either e ())) -> [Pending] -> IO [Pending]
ackPending render stats reportError writeOne due =
  catMaybes <$> for due (\p -> isolated render (failed p) (fmap (Nothing <$) (writeOne p)))
  where
    failed p msg = do
      let k = streamKind p.stream
      recordAckError stats k
      reportError ("ack: " <> kindName k <> ": " <> msg)
      pure (Just p)

-- | Feeds delivered notifications into the ack state; writes acks when due
-- (via @writeAcks@, which returns the failed ones) and keeps failed ones
-- pending.
acknowledge :: Int -> ([Pending] -> IO [Pending]) -> AckState -> [(Stream, UUID)] -> IO AckState
acknowledge n writeAcks = foldM step
  where
    step st (s, i) = case deliver n s i st of
      ([], st') -> pure st'
      (due, st') -> (`restore` st') <$> writeAcks due
```

`readerLoop` takes the `Reader` and threads an `AckState`:

```haskell
-- | One reader client: fetch all streams, re-fetch only those whose page was
-- full (more is waiting) until none is, then sleep. In 'Acks' mode every
-- delivered notification goes through 'acknowledge'; due acks are written
-- before the next fetch. Failures are reported and slow the loop down, never
-- stop it.
readerLoop :: QSem -> Env -> ConsumeOptions -> Reader -> ReaderStats -> (Text -> IO ()) -> [Subscription] -> IO ()
readerLoop gate env opts reader stats reportError = go emptyAckState
  where
    render :: UsageError -> Text
    render = tshow
    gated :: Sem StoreEffects a -> IO (Either UsageError a)
    gated = throughGate gate . runStore env
    fetchOne sub = gated (readerPass @UsageError render opts.fetchLimit (fromIntegral opts.safetyWindowMs) stats [sub])
    writeAcks = ackPending render stats reportError (gated . ackOne stats reader)
    received acks delivered = case opts.mode of
      NoAcks -> pure acks
      Acks n -> acknowledge n writeAcks acks delivered
    -- @(subscription, poll again)@ pairs, collected in reverse
    pollOne (done, acks) (sub, again)
      | not again = pure ((sub, False) : done, acks)
      | otherwise = do
          r <- readerRound render stats fetchOne [sub]
          traverse_ reportError r.errors
          acks' <- received acks r.delivered
          pure (reverse (map (,r.more) r.subs) <> done, acks')
    catchUp acks pending = do
      (polledRev, acks') <- foldM pollOne ([], acks) pending
      let polled = reverse polledRev
      if any snd polled then catchUp acks' polled else pure (map fst polled, acks')
    go acks subs = do
      (subs', acks') <- catchUp acks (map (,True) subs)
      threadDelay (opts.pollIntervalMs * 1000)
      go acks' subs'
```

`runConsume` wiring:

```haskell
  ackMetrics <- case opts.mode of
    Acks _ -> Just <$> newAckMetrics label
    NoAcks -> pure Nothing
  …
  let rs = readers population
      readerClient (stats, (reader, subs)) = readerLoop gate readerEnv opts reader stats reportError subs
  …
      withAsync (mapConcurrently_ readerClient (zip readerStats (zip rs clientSubs))) $ \readersA -> …
```

`consumeTickOnce` gets `Maybe AckMetrics` and calls `traverse_ (`publishAcks` readerReport) ackMetrics` after `publishConsume`.

- [ ] **Step 4: Run, expect PASS**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all pass, no warnings.

- [ ] **Step 5: Smoke-check the CLI**

Run: `cabal run -v0 fan-in-perf -- --db x consume-ack --help | grep -E 'ack-every|clients'`
Expected: both flags listed.

- [ ] **Step 6: Format and commit**

```bash
make format
git add tools/fan-in-perf/src/FanInPerf/Consume.hs tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs
git commit -m "fan-in-perf: consume-ack readers write acks"
```

---

### Task 11: Docs, dashboard, B-spec drift, acceptance

**Files:**
- Modify: `tools/fan-in-perf/README.md`
- Modify: `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json`
- Modify: `docs/superpowers/specs/2026-10-12-fan-in-perf-consume-design.md`

- [ ] **Step 1: README**

After the B section add (adapt headings to the file's style):

````markdown
Experiment C (acknowledging readers): `consume-ack` runs `consume` and every
simulated client acknowledges what it received. A client acks after every
N-th notification (`--ack-every N`, default 100, counted over all its
streams): one upsert per stream it received something on, up to the highest
id there (RFC: delivery tags with `multiple = true`; N = 1 is the RFC's live
path). Start positions come from resetting the client's acks to the latest
notification (RFC "when nothing is acknowledged"); run `reset` first.

```
fan-in-perf --db "$DB" reset
fan-in-perf --db "$DB" consume-ack --targets user:1000,clients:1000,team:10,epoch:200 --clients-per-user 2 --clients 2000 --duration 30 --ack-every 1
```

Readers are clients of users (round-robin, client ids 1..k per user); every
user is in one team (with `team` in `--targets`) and its clients share the
user's epoch streams. A reader fetches and acks as its (user, client).
`consume-ack` therefore needs equal STREAMS for `user` and `clients`.
````

- [ ] **Step 2: Dashboard panels**

Append to `panels` (ids 22–24, below the pool panels):

```json
    {
      "id": 22, "type": "timeseries", "title": "Ack upserts/s vs consumed/s", "datasource": null, "interval": "1s",
      "gridPos": { "x": 0, "y": 88, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "fanin_perf_ack_rate_current", "legendFormat": "acks/s {{experiment}}" },
        { "refId": "B", "expr": "fanin_perf_consume_rate_current", "legendFormat": "consumed/s {{experiment}}" }
      ]
    },
    {
      "id": 23, "type": "timeseries", "title": "Ack latency", "datasource": null, "interval": "1s",
      "gridPos": { "x": 12, "y": 88, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "histogram_quantile(0.5, sum by (le) (rate(fanin_perf_ack_duration_seconds_bucket[30s])))", "legendFormat": "p50" },
        { "refId": "B", "expr": "histogram_quantile(0.99, sum by (le) (rate(fanin_perf_ack_duration_seconds_bucket[30s])))", "legendFormat": "p99" }
      ]
    },
    {
      "id": 24, "type": "timeseries", "title": "Ack errors/s", "datasource": null, "interval": "1s",
      "gridPos": { "x": 0, "y": 96, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "sum by (kind) (rate(fanin_perf_ack_errors_total[5s]))", "legendFormat": "{{kind}}" }
      ]
    }
```

Check: `jq '[.panels[].id] | (length == (unique | length))' deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json` → `true`.

- [ ] **Step 3: B-spec drift note**

First run `git status --short docs/superpowers/specs/2026-10-12-fan-in-perf-consume-design.md`. If it shows uncommitted changes that are not yours, stop and ask the user whether to commit them along with the note.

Append to the B spec:

```markdown
## Drift (experiment C)

The reader population is superseded by
`2026-10-14-fan-in-perf-consume-ack-design.md`: readers are clients of users
(teams → users → clients), user and clients pools share their uids, team and
epochs follow the user. Subscriber counts per stream differ from the model
above when there are more readers than users.
```

- [ ] **Step 4: Quality gate**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Run: `make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Run: `make format && git status --short`
Expected: green, no warnings, no formatting diff beyond this task's files. (No new dependencies, so no `make regen-local-nix-derivations`.)

- [ ] **Step 5: Commit**

```bash
git add tools/fan-in-perf/README.md deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json docs/superpowers/specs/2026-10-12-fan-in-perf-consume-design.md
git commit -m "fan-in-perf: consume-ack docs, dashboard, B spec drift"
```

- [ ] **Step 6: Manual acceptance (user's DB; `export DB=…` with the connection string the user gave — never commit it)**

```bash
cabal run -v0 fan-in-perf -- --db "$DB" reset
cabal run -v0 fan-in-perf -- --db "$DB" consume-ack --targets user:1000,clients:1000,team:10,epoch:200 --clients-per-user 2 --clients 2000 --duration 30 --ack-every 1 | tail -20
```

Expected:
- a line `consume-ack: acks reset to latest (… streams)` before `consume-ack: running`;
- status lines with an `ack/s` segment;
- a summary with `acks=` > 0 and `errors=0`, plus `missing=0` (or a non-zero `missing`, which is a finding about the RFC's cursor model and must be reported, not hidden);
- while running: `curl -s localhost:9400/metrics | grep -c fanin_perf_acks_total` > 0.

Then the read-only DB checks (all `0` unless stated):

```bash
psql "$DB" -Atc "select count(*) from user_notification_acks"     # <= 2000
psql "$DB" -Atc "select count(*) from client_notification_acks"   # <= 2000
psql "$DB" -Atc "select count(*) from client_notification_acks where client_id not between 1 and 2"
psql "$DB" -Atc "select count(*) from client_notification_acks c left join user_notification_acks u using (user_id, client_id) where u.user_id is null"
psql "$DB" -Atc "select count(*) from (select user_id from team_notification_acks group by 1 having count(distinct team_id) > 1) x"
psql "$DB" -Atc "select count(*) from (select user_id from epoch_notification_acks group by user_id having count(distinct (group_id, epoch)) > 3) x"
psql "$DB" -Atc "select count(*) from user_notification_acks a join last_user_notifications l using (user_id) where a.last_notification_id > l.notification_id"
```

Repeat the run with `--ack-every 100` (after `reset`) and compare ack/s, push/s and consume rate with the `--ack-every 1` run. Report all numbers and query results to the user.
