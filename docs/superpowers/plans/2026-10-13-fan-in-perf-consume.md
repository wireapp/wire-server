# fan-in-perf `consume` (experiment B) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `consume` sub-command to `tools/fan-in-perf` that runs unthrottled writers plus ~2,000 simulated reader clients against the fan-in PostgreSQL store and reports consume rate, delivery lag, backlog and lost/duplicate notifications.

**Architecture:** Two new store operations (`FetchNotifications`, `GetLastNotificationId`) in `Wire.FanInNotificationsStore`. Readers are green threads sharing a separate connection pool; they hold cursors in memory, fetch per stream, and are derived from the same stream pools as the writers (`FanInPerf.Population`). Writers record the deliveries they expect; readers record what they got; the difference is reported after a drain phase. Reader logic is a Polysemy program over the store effect, so it is unit-tested with an in-memory fake store.

**Tech Stack:** Haskell (GHC2021), Polysemy, hasql + hasql-th, optparse-applicative, prometheus-client, async, hspec/QuickCheck.

**Spec:** `docs/superpowers/specs/2026-10-12-fan-in-perf-consume-design.md` (experiment A: `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md`; where it disagrees with the code, the code wins).

## Global Constraints

- Build/test commands always end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` (AGENTS.md). Compile package: `make c package=<PKG> | …`. Tests of a package: `make c package=<PKG> test=1 | …`. Single spec: `cabal test fan-in-perf-tests --test-options='-m "<describe text>"' | …`.
- No compiler warnings. All unit tests of changed packages (`fan-in-perf`, `wire-subsystems`, `extended` if touched) green. Format with `make format` before every commit.
- Do NOT run integration tests (`make ci`, `*-integration`). Do not start/stop docker. The only DB use is the manual acceptance run in Task 10, and only with the user's consent.
- DB access of the tool only through `Wire.FanInNotificationsStore` (and the existing `Wire.FanInNotificationsAdmin`). No SQL in the tool.
- Prefer record dot syntax over lenses.
- `--targets` for `consume` rejects `connections`. `consume` defaults: `--clients 2000`, `--epochs-per-client 3`, `--poll-interval 100` (ms), `--fetch-limit 100`, `--reader-pool-size 32`, `--drain-timeout 10` (s).
- Initial reader cursor = `GetLastNotificationId`, `Nothing` = nil UUID; all cursors loaded **before writers start**.
- Metrics prefix `fanin_perf_`, label `experiment="consume"`. Register each metric exactly once (a second `P.register` of the same name duplicates the series in `/metrics`).
- Commit messages: extremely concise; end with `Co-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>`.
- Out of scope (do not touch): the three open `REVIEW:` comments in `Produce.hs`, `Targets.hs`, `Metrics.hs`; `connections` kind; `epoch_history`; acks; writer rate cap.

## Review Focus

Most likely to bite, each pinned by a test in the owning task:

1. A notification committed *after* the reader's cursor passed its (smaller) id is never delivered — must be visible as `missing`, not hidden. (Task 9: cursor-hazard test; Task 8: summary prints `missing`.)
2. Fetch error must not advance the cursor and must not spin hot (counted, reported, then `pause`). (Task 9.)
3. Population mismatches: fewer streams than clients (shared user streams), epochs per client > pool, `clients` kind with too few client ids, coverage < 100%. (Task 5.)
4. Empty / never-written streams: `GetLastNotificationId = Nothing` → nil cursor, first fetch returns everything newer. (Task 9.)
5. Backlog can transiently go negative (reader sees a push before the writer records `expected`) → clamp to 0 for display; summary computes `missing`/`duplicates` with `max 0`. (Tasks 6, 8.)
6. `--poll-interval 0` and bounds of every new flag are accepted/rejected correctly. (Task 4.)

## File Structure

| File | Change |
|---|---|
| `libs/wire-subsystems/src/Wire/FanInNotificationsStore.hs` | `Stream`, `FetchNotifications`, `GetLastNotificationId` |
| `libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs` | interpreter for both ops, `notificationIdMillis` |
| `libs/wire-subsystems/test/unit/Wire/FanInNotificationsStore/PostgresSpec.hs` | test `notificationIdMillis` |
| `libs/extended/src/Hasql/Pool/Extended.hs` | `initPostgresPoolSharingMetrics` |
| `tools/fan-in-perf/src/FanInPerf/Options.hs` | `consume` command, `ConsumeOptions` |
| `tools/fan-in-perf/src/FanInPerf/Population.hs` (new) | reader→stream mapping, subscriber counts, expected deliveries, coverage (pure) |
| `tools/fan-in-perf/src/FanInPerf/Stats.hs` | `expected` counter, `toBuckets`, export `minTickInterval` |
| `tools/fan-in-perf/src/FanInPerf/ConsumeStats.hs` (new) | reader snapshots, consume tick |
| `tools/fan-in-perf/src/FanInPerf/Metrics.hs` | histogram generalisation, `ConsumeMetrics` |
| `tools/fan-in-perf/src/FanInPerf/Terminal.hs` | consume status/summary/coverage formatting |
| `tools/fan-in-perf/src/FanInPerf/Produce.hs` | export `storePush`, `waitForStop`, `printQueuedErrors` |
| `tools/fan-in-perf/src/FanInPerf/Consume.hs` (new) | `loadCursors`, `readerPass`, `runConsume` |
| `tools/fan-in-perf/src/FanInPerf/Run.hs` | `consume` wiring, reader pool |
| `tools/fan-in-perf/fan-in-perf.cabal`, `default.nix` | modules, deps |
| `tools/fan-in-perf/test/FanInPerf/*Spec.hs` | tests per task |
| `tools/fan-in-perf/README.md`, `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json`, A spec | docs, panels, drift |

---

### Task 1: Fix stale `TerminalSpec` (pre-existing failure)

Commit `d222b8fcd` made `groupThousands` emit `,` but `TerminalSpec` still expects spaces. Fix so the baseline is green before building on it.

**Files:**
- Modify: `tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs:29` and the `formatStatus` expectation

**Interfaces:** none.

- [ ] **Step 1: Confirm the failure**

Run: `cabal test fan-in-perf-tests --test-options='-m "FanInPerf.Terminal"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: FAIL in `groupThousands` and `formatStatus` (expected `"1 000"`, got `"1,000"`).

- [ ] **Step 2: Update expectations**

In `TerminalSpec.hs` replace

```haskell
        `shouldBe` ["0", "999", "1 000", "1 234 567", "-9 105"]
```
with
```haskell
        `shouldBe` ["0", "999", "1,000", "1,234,567", "-9,105"]
```
and replace the `formatStatus` expectation string with
```haskell
        `shouldBe` "t=42s push/s cur=8,312 max=9,105 | err/s cur=3 (0.04%) | targets/s cur=41,560 | p50(cum.)=3.1ms p99(cum.)=12.4ms"
```

- [ ] **Step 3: Run**

Run: same command as Step 1. Expected: all examples pass.

- [ ] **Step 4: Commit**

```bash
make format
git add tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs
git commit -m "fan-in-perf: fix TerminalSpec for ',' thousands separator"
```

---

### Task 2: Store ops `FetchNotifications`, `GetLastNotificationId`, `notificationIdMillis`

**Files:**
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsStore.hs`
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs`
- Test: `libs/wire-subsystems/test/unit/Wire/FanInNotificationsStore/PostgresSpec.hs`

**Interfaces:**
- Produces (module `Wire.FanInNotificationsStore`):
  ```haskell
  data Stream
    = StreamUser !UserId
    | StreamClient !UserId !ClientId
    | StreamTeam !TeamId
    | StreamEpoch !GroupId !Epoch
    deriving (Eq, Show)

  FetchNotifications :: Stream -> UUID -> Int -> FanInNotificationsStore m [(UUID, Value)]
  GetLastNotificationId :: Stream -> FanInNotificationsStore m (Maybe UUID)
  -- makeSem: fetchNotifications, getLastNotificationId
  ```
  `FetchNotifications s cursor limit`: notifications of `s` with id `> cursor`, oldest first, at most `limit`.
- Produces (module `Wire.FanInNotificationsStore.Postgres`): `notificationIdMillis :: UUID -> Word64` (Unix ms encoded in the UUIDv7).

No DB-free test is possible for the SQL; it is covered by Task 10's acceptance run. `notificationIdMillis` is unit-tested.

- [ ] **Step 1: Write the failing test**

In `PostgresSpec.hs` add imports `Data.Time.Clock.POSIX (getPOSIXTime)` and extend the import of `Wire.FanInNotificationsStore.Postgres` to `(genNotificationId, notificationIdMillis)`, then append to `spec`:

```haskell
  describe "notificationIdMillis" $
    it "returns the generation time in milliseconds" $ do
      before <- nowMillis
      u <- (.toUUID) <$> genNotificationId @()
      after <- nowMillis
      let t = notificationIdMillis u
      -- one ms of slack: the id generator may round its clock reading up
      (t >= before && t <= after + 1) `shouldBe` True
  where
    nowMillis :: IO Word64
    nowMillis = floor . (* 1000) <$> getPOSIXTime
```
(`where` attaches to `spec`; if the file's `spec` already has a `where`, merge.)

- [ ] **Step 2: Run to verify it fails**

Run: `make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error "Variable not in scope: notificationIdMillis".

- [ ] **Step 3: Extend the effect**

In `Wire/FanInNotificationsStore.hs` add `import Data.UUID (UUID)` and replace the effect declaration with:

```haskell
-- | One notification stream, i.e. one key of a notification table.
data Stream
  = StreamUser !UserId
  | StreamClient !UserId !ClientId
  | StreamTeam !TeamId
  | StreamEpoch !GroupId !Epoch
  deriving (Eq, Show)

data FanInNotificationsStore m a where
  PushViaFanIn :: FanInPush -> FanInNotificationsStore m ()
  -- | Notifications of a stream with an id greater than the given one, oldest
  -- first, at most the given number.
  FetchNotifications :: Stream -> UUID -> Int -> FanInNotificationsStore m [(UUID, Value)]
  -- | Id of the latest notification ever pushed to the stream.
  GetLastNotificationId :: Stream -> FanInNotificationsStore m (Maybe UUID)

makeSem ''FanInNotificationsStore
```
(`Value` comes from the existing `Data.Aeson` import.)

- [ ] **Step 4: Interpreter**

In `Wire/FanInNotificationsStore/Postgres.hs` add imports:
```haskell
import Data.Bits (shiftL, shiftR, (.|.))
import Data.UUID qualified as UUID
```
Replace the `interpret` body:
```haskell
interpretFanInNotificationsStoreToPostgres isolationLevel = interpret $ \case
  PushViaFanIn push -> pushViaFanInImpl isolationLevel push
  FetchNotifications stream cursor limit -> fetchNotificationsImpl stream cursor limit
  GetLastNotificationId stream -> getLastNotificationIdImpl stream
```
Add after `genNotificationId`:

```haskell
-- | Milliseconds since the Unix epoch encoded in the first 48 bits of a UUIDv7.
notificationIdMillis :: UUID -> Word64
notificationIdMillis u =
  let (w1, w2, _, _) = UUID.toWords u
   in (fromIntegral w1 `shiftL` 16) .|. (fromIntegral w2 `shiftR` 16)

fetchNotificationsImpl ::
  (PGConstraints r) =>
  Stream ->
  UUID ->
  Int ->
  Sem r [(UUID, Value)]
fetchNotificationsImpl stream cursor limit =
  V.toList <$> case stream of
    StreamUser uid -> runStatement (uid.toUUID, cursor, limit') fetchUserStatement
    StreamClient uid cid ->
      runStatement (uid.toUUID, fromIntegral cid.clientToWord64, cursor, limit') fetchClientStatement
    StreamTeam tid -> runStatement (tid.toUUID, cursor, limit') fetchTeamStatement
    StreamEpoch gid epoch ->
      runStatement (gid.unGroupId, fromIntegral epoch.epochNumber, cursor, limit') fetchEpochStatement
  where
    limit' = fromIntegral limit :: Int64

fetchUserStatement :: Statement (UUID, UUID, Int64) (V.Vector (UUID, Value))
fetchUserStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from user_notifications
    where user_id = ($1 :: uuid) and notification_id > ($2 :: uuid)
    order by notification_id
    limit ($3 :: int8)
  |]

fetchClientStatement :: Statement (UUID, Int64, UUID, Int64) (V.Vector (UUID, Value))
fetchClientStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from client_notifications
    where user_id = ($1 :: uuid) and client_id = ($2 :: int8) and notification_id > ($3 :: uuid)
    order by notification_id
    limit ($4 :: int8)
  |]

fetchTeamStatement :: Statement (UUID, UUID, Int64) (V.Vector (UUID, Value))
fetchTeamStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from team_notifications
    where team_id = ($1 :: uuid) and notification_id > ($2 :: uuid)
    order by notification_id
    limit ($3 :: int8)
  |]

fetchEpochStatement :: Statement (ByteString, Int64, UUID, Int64) (V.Vector (UUID, Value))
fetchEpochStatement =
  [vectorStatement|
    select notification_id :: uuid, payload :: jsonb
    from epoch_notifications
    where group_id = ($1 :: bytea) and epoch = ($2 :: int8) and notification_id > ($3 :: uuid)
    order by notification_id
    limit ($4 :: int8)
  |]

getLastNotificationIdImpl ::
  (PGConstraints r) =>
  Stream ->
  Sem r (Maybe UUID)
getLastNotificationIdImpl = \case
  StreamUser uid -> runStatement uid.toUUID lastUserStatement
  StreamClient uid cid -> runStatement (uid.toUUID, fromIntegral cid.clientToWord64) lastClientStatement
  StreamTeam tid -> runStatement tid.toUUID lastTeamStatement
  StreamEpoch gid epoch -> runStatement (gid.unGroupId, fromIntegral epoch.epochNumber) lastEpochStatement

lastUserStatement :: Statement UUID (Maybe UUID)
lastUserStatement =
  [maybeStatement|
    select notification_id :: uuid from last_user_notifications where user_id = ($1 :: uuid)
  |]

lastClientStatement :: Statement (UUID, Int64) (Maybe UUID)
lastClientStatement =
  [maybeStatement|
    select notification_id :: uuid from last_client_notifications
    where user_id = ($1 :: uuid) and client_id = ($2 :: int8)
  |]

lastTeamStatement :: Statement UUID (Maybe UUID)
lastTeamStatement =
  [maybeStatement|
    select notification_id :: uuid from last_team_notifications where team_id = ($1 :: uuid)
  |]

lastEpochStatement :: Statement (ByteString, Int64) (Maybe UUID)
lastEpochStatement =
  [maybeStatement|
    select notification_id :: uuid from last_epoch_notifications
    where group_id = ($1 :: bytea) and epoch = ($2 :: int8)
  |]
```
Place the new top-level statements anywhere after the interpreter; keep the module's existing style.

- [ ] **Step 5: Build and test**

Run: `make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings; `notificationIdMillis` and `genNotificationId` examples pass. If `UUID.toWords` / `Data.Aeson.Value` imports are flagged unused/missing, fix per the compiler.

- [ ] **Step 6: Commit**

```bash
make format
git add libs/wire-subsystems
git commit -m "FanInNotificationsStore: FetchNotifications, GetLastNotificationId"
```

---

### Task 3: Second connection pool without duplicate metrics

`initPostgresPoolFromConnString` registers `wire_hasql_pool_*` on every call; a second (reader) pool would duplicate those series. Add a variant sharing the first pool's metrics.

**Files:**
- Modify: `libs/extended/src/Hasql/Pool/Extended.hs`
- Create: `tools/fan-in-perf/test/FanInPerf/PoolSpec.hs`
- Modify: `tools/fan-in-perf/fan-in-perf.cabal` (test `other-modules`: `FanInPerf.PoolSpec`; test `build-depends`: `extended`, `postgresql-connection-string`)

**Interfaces:**
- Produces: `initPostgresPoolSharingMetrics :: Pool -> PoolConfig -> PostgresqlConnectionString.ConnectionString -> Maybe Text -> IO Pool`. Counters/histograms aggregate both pools; the `ready_for_use`/`in_use` gauges keep reporting the first pool only (no second stats reporter).

- [ ] **Step 1: Write the failing test**

`tools/fan-in-perf/test/FanInPerf/PoolSpec.hs`:
```haskell
module FanInPerf.PoolSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Misc (Duration (..))
import Hasql.Pool.Extended
import Imports
import PostgresqlConnectionString qualified
import Prometheus qualified as P
import Test.Hspec

spec :: Spec
spec = describe "initPostgresPoolSharingMetrics" $
  it "does not register the pool metrics a second time" $ do
    connStr <- either (const (fail "connection string")) pure (PostgresqlConnectionString.parse "postgresql://u:p@localhost:1/db")
    let cfg = PoolConfig {size = 1, acquisitionTimeout = Duration 1, idlenessTimeout = Duration 1}
    first <- initPostgresPoolFromConnString cfg connStr Nothing
    _ <- initPostgresPoolSharingMetrics first cfg connStr Nothing
    out <- LBS8.unpack <$> P.exportMetricsAsText
    length (filter (isPrefixOf "# TYPE wire_hasql_pool_in_use ") (lines out)) `shouldBe` 1
```
Add the cabal entries listed above.

- [ ] **Step 2: Run to verify it fails**

Run: `cabal test fan-in-perf-tests --test-options='-m "initPostgresPoolSharingMetrics"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error "not in scope: initPostgresPoolSharingMetrics".

- [ ] **Step 3: Implement**

In `Hasql/Pool/Extended.hs`, replace `initPostgresPoolFromConnString` and its `where` block with:

```haskell
-- | Creates a pool from a parsed connection string and an optional password.
initPostgresPoolFromConnString :: PoolConfig -> PostgresqlConnectionString.ConnectionString -> Maybe Text -> IO Pool
initPostgresPoolFromConnString config connStr mPw = do
  metrics <- mkHasqlPoolMetrics
  pool <- acquirePool metrics config connStr mPw
  startHasqlPoolStatsReporter pool
  pure pool

-- | A further pool that reports into the metrics of an existing pool:
-- registering them again would duplicate the series. Counters and histograms
-- aggregate both pools; the pool gauges keep describing the first pool only.
initPostgresPoolSharingMetrics :: Pool -> PoolConfig -> PostgresqlConnectionString.ConnectionString -> Maybe Text -> IO Pool
initPostgresPoolSharingMetrics existing = acquirePool existing.metrics

acquirePool :: HasqlPoolMetrics -> PoolConfig -> PostgresqlConnectionString.ConnectionString -> Maybe Text -> IO Pool
acquirePool metrics config connStr mPw = do
  let pgSettings =
        HasqlConnSettings.connectionString (PostgresqlConnectionString.toUrl connStr)
          <> foldMap HasqlConnSettings.password mPw
  rawPool <-
    HasqlPool.acquireWith
      (instrumentedConnectionGetter (Hasql.Connection.acquire pgSettings))
      ( config.size,
        realToFrac config.idlenessTimeout.duration,
        unusedSettings
      )
  pure Pool {rawPool, metrics, poolAcquisitionTimeout = config.acquisitionTimeout}
  where
    instrumentedConnectionGetter getter = do
      started <- getMonotonicTime
      res <- getter
      ended <- getMonotonicTime
      recordHasqlPoolConnectionAcquisition metrics (ended - started)
      case res of
        Right _ -> recordHasqlPoolConnectionEstablished metrics
        Left _ -> recordHasqlPoolConnectionFailure metrics
      pure res

    unusedSettings =
      -- The custom getter above performs the actual connection establishment.
      -- The API forces us to pass this record, but it is actually not used in acquireWith
      HasqlPool.ConnectionSettings
        { host = "",
          port = 5432,
          user = "",
          password = "",
          dbName = "",
          connAcqTimeout = 0,
          txIdleTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          stmtTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          sslMode = "prefer",
          sslRootCert = ""
        }

mkHasqlPoolMetrics :: IO HasqlPoolMetrics
mkHasqlPoolMetrics =
  HasqlPoolMetrics
    <$> register (gauge $ Info "wire_hasql_pool_ready_for_use" "Number of hasql pool connections ready for use")
    <*> register (gauge $ Info "wire_hasql_pool_in_use" "Number of hasql pool connections in use")
    <*> register (counter $ Info "wire_hasql_pool_connection_established_count" "Number of established connections")
    <*> register (counter $ Info "wire_hasql_pool_connection_failure_count" "Number of failed connection acquisition attempts")
    <*> register (counter $ Info "wire_hasql_pool_acquisition_timeout_count" "Number of pool acquisition timeouts")
    <*> register (counter $ Info "wire_hasql_pool_session_failure_count" "Number of times a session has failed")
    <*> register (counter $ Info "wire_hasql_pool_session_count" "Number of times a session was created")
    <*> register (histogram (Info "wire_hasql_pool_connection_acquisition_seconds" "Time spent establishing new PostgreSQL connections") defaultBuckets)
    <*> register (histogram (Info "wire_hasql_pool_session_seconds" "Time spent using PostgreSQL sessions") defaultBuckets)
```
Keep the existing `initPostgresPool` untouched.

- [ ] **Step 4: Build and run**

Run: `make c package=extended | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` then the Step 2 command.
Expected: no warnings; example passes. If pool creation turns out to connect eagerly and the test errors with a connection failure, replace the test body's pool creation by registering via `mkHasqlPoolMetrics` is NOT an option (it is the thing under test) — instead delete `PoolSpec` and rely on the Task 10 manual check that `/metrics` has one `# TYPE wire_hasql_pool_in_use` line; note that in the commit message.

- [ ] **Step 5: Commit**

```bash
make format
git add libs/extended tools/fan-in-perf
git commit -m "Hasql.Pool.Extended: pool sharing another pool's metrics"
```

---

### Task 4: `consume` options

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Options.hs`
- Modify: `tools/fan-in-perf/src/FanInPerf/Run.hs` (only `defaultPoolSize`, to keep it compiling)
- Test: `tools/fan-in-perf/test/FanInPerf/OptionsSpec.hs`

**Interfaces:**
- Produces:
  ```haskell
  data ConsumeOptions = ConsumeOptions
    { writerOptions :: ProduceOptions,  -- all `produce` flags
      clients :: Int,
      epochsPerClient :: Int,
      pollIntervalMs :: Int,
      fetchLimit :: Int,
      readerPoolSize :: Int,
      drainTimeout :: Int  -- seconds
    } deriving (Eq, Show)
  data Command = Migrate | Reset | Produce ProduceOptions | Consume ConsumeOptions
  ```

- [ ] **Step 1: Write the failing tests**

Append to `OptionsSpec.hs` `spec` (imports `Data.Maybe (isNothing)` if `Imports` lacks it):

```haskell
  it "parses consume with defaults" $ do
    o <- parseOk ["--db", "postgresql://u:p@localhost/db", "consume", "--targets", "user:100,team:1"]
    case o.command of
      Consume c -> do
        (c.clients, c.epochsPerClient, c.pollIntervalMs, c.fetchLimit, c.readerPoolSize, c.drainTimeout)
          `shouldBe` (2000, 3, 100, 100, 32, 10)
        c.writerOptions.writers `shouldBe` 16
        c.writerOptions.targets `shouldBe` (TargetConfig KindUser 100 1 :| [TargetConfig KindTeam 1 1])
      _ -> expectationFailure "not a consume command"

  it "parses all consume flags" $ do
    o <-
      parseOk
        [ "--db", "postgresql://u:p@localhost/db", "consume", "--targets", "epoch:50",
          "--clients", "10", "--epochs-per-client", "2", "--poll-interval", "0",
          "--fetch-limit", "5", "--reader-pool-size", "4", "--drain-timeout", "0",
          "--writers", "3", "--duration", "9"
        ]
    case o.command of
      Consume c -> do
        (c.clients, c.epochsPerClient, c.pollIntervalMs, c.fetchLimit, c.readerPoolSize, c.drainTimeout)
          `shouldBe` (10, 2, 0, 5, 4, 0)
        (c.writerOptions.writers, c.writerOptions.duration) `shouldBe` (3, Just 9)
      _ -> expectationFailure "not a consume command"

  it "rejects consume flags out of bounds and the connections kind" $ do
    let consume extra = ["--db", "x", "consume", "--targets", "user:10"] <> extra
    isNothing (parse (consume ["--clients", "0"])) `shouldBe` True
    isNothing (parse (consume ["--fetch-limit", "0"])) `shouldBe` True
    isNothing (parse (consume ["--poll-interval", "-1"])) `shouldBe` True
    isNothing (parse (consume ["--reader-pool-size", "0"])) `shouldBe` True
    isNothing (parse ["--db", "x", "consume", "--targets", "connections:5"]) `shouldBe` True
    isNothing (parse ["--db", "x", "consume", "--targets", "user:10,connections:5"]) `shouldBe` True
    -- `produce` still accepts connections
    isNothing (parse ["--db", "x", "produce", "--targets", "connections:5"]) `shouldBe` False
```

- [ ] **Step 2: Run to verify failure**

Run: `cabal test fan-in-perf-tests --test-options='-m "FanInPerf.Options"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error (`Consume` not in scope).

- [ ] **Step 3: Implement**

In `Options.hs`: export `ConsumeOptions (..)`; add the type and constructor from the Interfaces block; add the sub-command and parsers. Make `produceParser` take its targets reader:

```haskell
produceParser :: Parser ProduceOptions
produceParser = produceParserWith targetsReader

produceParserWith :: ReadM (NonEmpty TargetConfig) -> Parser ProduceOptions
produceParserWith targetsRdr =
  ProduceOptions
    <$> option (positive maxWriters) (long "writers" <> metavar "W" <> value 16 <> showDefault <> help "Concurrent writer threads")
    <*> option targetsRdr (long "targets" <> metavar "SPEC" <> helpDoc (Just targetsHelp))
    -- … remaining fields exactly as before …
```
(only the `targetsReader` occurrence changes to `targetsRdr`). Add:

```haskell
consumeParser :: Parser ConsumeOptions
consumeParser =
  ConsumeOptions
    <$> produceParserWith consumeTargetsReader
    <*> option (positive maxClients) (long "clients" <> metavar "N" <> value 2000 <> showDefault <> help "Simulated reader clients")
    <*> option (positive maxEpochsPerClient) (long "epochs-per-client" <> metavar "E" <> value 3 <> showDefault <> help "Epoch streams per client (needs an 'epoch' entry in --targets)")
    <*> option (nonNegative maxPollIntervalMs) (long "poll-interval" <> metavar "MS" <> value 100 <> showDefault <> help "Sleep in milliseconds after a fetch pass without a full page")
    <*> option (positive maxFetchLimit) (long "fetch-limit" <> metavar "L" <> value 100 <> showDefault <> help "Notifications per fetch")
    <*> option (positive maxPoolSize) (long "reader-pool-size" <> metavar "P" <> value 32 <> showDefault <> help "Connection pool size of the readers (writers: --pool-size)")
    <*> option (nonNegative maxDuration) (long "drain-timeout" <> metavar "SECS" <> value 10 <> showDefault <> help "Seconds to wait for readers to catch up after the writers stopped")

-- | Readers have no 'connections' streams (see the design spec's follow-ups).
consumeTargetsReader :: ReadM (NonEmpty TargetConfig)
consumeTargetsReader = do
  ts <- targetsReader
  when (any ((== KindConnections) . (.kind)) ts) $
    readerError "the 'connections' kind is not supported by 'consume'"
  pure ts

maxClients, maxEpochsPerClient, maxPollIntervalMs, maxFetchLimit :: Integer
maxClients = 100_000
maxEpochsPerClient = 10_000
maxPollIntervalMs = 3_600_000
maxFetchLimit = 10_000
```
Add to the `hsubparser`:
```haskell
          <> O.command "consume" (info (Consume <$> consumeParser) (progDesc "Experiment B: maximal rate of adding and consuming notifications by simulated clients"))
```
In `Run.hs` `defaultPoolSize` add `Consume c -> c.writerOptions.writers`. In `run`'s `case opts.command of` add a temporary arm so it compiles: `Consume _ -> abort "consume: not implemented yet"` (replaced in Task 9).

- [ ] **Step 4: Run tests**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings; all Options examples pass.

- [ ] **Step 5: Commit**

```bash
make format
git add tools/fan-in-perf
git commit -m "fan-in-perf: consume options"
```

---

### Task 5: `Population` (pure reader→stream mapping)

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Population.hs`
- Create: `tools/fan-in-perf/test/FanInPerf/PopulationSpec.hs`
- Modify: `tools/fan-in-perf/fan-in-perf.cabal` (`exposed-modules`: `FanInPerf.Population`; test `other-modules`: `FanInPerf.PopulationSpec`)

**Interfaces:**
- Consumes: `FanInPerf.Targets` (`Entry (..)`, `StreamPool (..)`, `TargetConfig (..)`, `TargetKind (..)`, `targetAt`), `Wire.FanInNotificationsStore` (`Stream (..)`, `Target (..)`, `FanInPush (..)`).
- Produces:
  ```haskell
  newtype Client = Client {streams :: [Stream]}
  data Key  -- opaque subscriber-map key (Eq, Ord, Show)
  data Population = Population {simClients :: [Client], subscribers :: Map Key Int}
  mkPopulation :: Int -> Int -> V.Vector Entry -> Either Text Population  -- clients, epochsPerClient, pools
  streamKind :: Stream -> TargetKind
  expectedDeliveries :: Map Key Int -> FanInPush -> Int
  coverage :: Map Key Int -> V.Vector Entry -> [(TargetKind, Double)]
  ```
  Mapping: client `i`, entry pool size `S`: `user`/`team` → stream `i mod S`; `clients` → pool entry `i mod S`, client id = element `i div S` of its client ids (error if clients > S × ids per user); `epoch` → `E` consecutive streams from `(i × E) mod S` (error if `E > S`); `connections` → error.

- [ ] **Step 1: Write the failing tests**

`PopulationSpec.hs`:
```haskell
module FanInPerf.PopulationSpec (spec) where

import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Population
import FanInPerf.Targets
import Imports
import System.Random (mkStdGen)
import Test.Hspec
import Wire.FanInNotificationsStore (FanInPush (..), Stream (..))

entriesOf :: Int -> [TargetConfig] -> V.Vector Entry
entriesOf clientsPerUser cfgs = fst (mkEntries clientsPerUser (NE.fromList cfgs) (mkStdGen 7))

population :: Int -> Int -> V.Vector Entry -> Population
population n e es = either (error . T.unpack) id (mkPopulation n e es)

-- the only push a single-entry spec can produce, for any seed
pushOf :: V.Vector Entry -> FanInPush
pushOf es = fst (genPush (mkPayload 0) es (mkStdGen 1))

spec :: Spec
spec = do
  describe "mkPopulation" $ do
    it "spreads clients over a user pool; shared streams are allowed" $ do
      let p = population 25 3 (entriesOf 1 [TargetConfig KindUser 10 1])
      all (\c -> length c.streams == 1 && all ((== KindUser) . streamKind) c.streams) p.simClients `shouldBe` True
      length p.simClients `shouldBe` 25
      sum (Map.elems p.subscribers) `shouldBe` 25
      all (`elem` [2, 3]) (Map.elems p.subscribers) `shouldBe` True

    it "puts every client on the single team of a one-team pool" $ do
      let es = entriesOf 1 [TargetConfig KindTeam 1 1]
          p = population 25 3 es
      expectedDeliveries p.subscribers (pushOf es) `shouldBe` 25

    it "gives each client E distinct consecutive epoch streams" $ do
      let p = population 10 3 (entriesOf 1 [TargetConfig KindEpoch 10 1])
      all (\c -> length (nubOrd (map show c.streams)) == 3) p.simClients `shouldBe` True
      sum (Map.elems p.subscribers) `shouldBe` 30

    it "rejects more epochs per client than epoch streams" $
      isLeft (mkPopulation 10 11 (entriesOf 1 [TargetConfig KindEpoch 10 1])) `shouldBe` True

    it "assigns client ids of a clients pool by i div S, and rejects too many clients" $ do
      let es = entriesOf 2 [TargetConfig KindClients 4 1]
      -- 4 clients: all client id 1 of their user; a push to client ids [1,2] reaches 1 subscriber
      expectedDeliveries (population 4 3 es).subscribers (pushOf es) `shouldBe` 1
      -- 8 clients: both client ids subscribed
      expectedDeliveries (population 8 3 es).subscribers (pushOf es) `shouldBe` 2
      isLeft (mkPopulation 9 3 es) `shouldBe` True

    it "rejects the connections kind" $
      isLeft (mkPopulation 5 3 (entriesOf 1 [TargetConfig KindConnections 5 1])) `shouldBe` True

    it "combines kinds per client" $ do
      let p = population 6 2 (entriesOf 1 [TargetConfig KindUser 6 1, TargetConfig KindTeam 2 1, TargetConfig KindEpoch 4 1])
      all (\c -> map streamKind c.streams == [KindUser, KindTeam, KindEpoch, KindEpoch]) p.simClients `shouldBe` True

  describe "expectedDeliveries" $
    it "is 0 for a push to a stream nobody reads" $ do
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
          p = population 20 3 es
          -- stream pools are fixed, so scan pushes until one hits an unread stream
          counts = [expectedDeliveries p.subscribers (fst (genPush (mkPayload 0) es (mkStdGen s))) | s <- [1 .. 200]]
      (0 `elem` counts, all (`elem` [0, 1]) counts) `shouldBe` (True, True)

  describe "coverage" $ do
    it "is the share of pool streams with a reader" $
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
       in coverage (population 20 3 es).subscribers es `shouldBe` [(KindUser, 0.2)]
    it "is 1 when every stream has a reader" $
      let es = entriesOf 1 [TargetConfig KindTeam 1 1, TargetConfig KindUser 10 1]
       in map snd (coverage (population 10 3 es).subscribers es) `shouldBe` [1, 1]
```
(`isLeft` from `Imports`; if absent import `Data.Either (isLeft)`.)

- [ ] **Step 2: Run to verify failure**

Run: `cabal test fan-in-perf-tests --test-options='-m "FanInPerf.Population"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error, module `FanInPerf.Population` not found.

- [ ] **Step 3: Implement**

`Population.hs`:
```haskell
-- | Which streams each simulated reader client subscribes to. Derived from the
-- same stream pools the writers use, so readers poll streams that get written.
module FanInPerf.Population
  ( Client (..),
    Key,
    Population (..),
    mkPopulation,
    streamKind,
    expectedDeliveries,
    coverage,
  )
where

import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Targets
import Imports
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.FanInNotificationsStore

newtype Client = Client {streams :: [Stream]}

-- | Identifies one stream. 'Epoch' has no 'Ord', hence the plain number.
data Key
  = KUser !UserId
  | KClient !UserId !ClientId
  | KTeam !TeamId
  | KEpoch !GroupId !Word64
  deriving (Eq, Ord, Show)

data Population = Population
  { simClients :: [Client],
    -- | Number of clients reading each stream (streams nobody reads are absent).
    subscribers :: Map Key Int
  }

streamKey :: Stream -> Key
streamKey = \case
  StreamUser u -> KUser u
  StreamClient u c -> KClient u c
  StreamTeam t -> KTeam t
  StreamEpoch g e -> KEpoch g e.epochNumber

streamKind :: Stream -> TargetKind
streamKind = \case
  StreamUser _ -> KindUser
  StreamClient _ _ -> KindClients
  StreamTeam _ -> KindTeam
  StreamEpoch _ _ -> KindEpoch

-- | The streams (as 'Key's) a push 'Target' writes to.
targetKeys :: Target -> [Key]
targetKeys = \case
  TargetUser u -> [KUser u]
  TargetUserClients (u, cs) -> [KClient u c | c <- NE.toList cs]
  TargetTeam t -> [KTeam t]
  TargetEpoch (g, e) -> [KEpoch g e.epochNumber]
  TargetConnections _ -> []

-- | @mkPopulation clients epochsPerClient entries@. 'entries' is never empty
-- (the CLI requires at least one target entry).
mkPopulation :: Int -> Int -> V.Vector Entry -> Either Text Population
mkPopulation clients epochsPerClient entries = do
  perEntry <- traverse (clientStreams clients epochsPerClient) (V.toList entries)
  let simClients = map (Client . concat) (transpose perEntry)
      subscribers = Map.fromListWith (+) [(streamKey s, 1) | c <- simClients, s <- c.streams]
  pure Population {simClients, subscribers}

-- | For one entry: the streams it gives to each client (index = client number).
clientStreams :: Int -> Int -> Entry -> Either Text [[Stream]]
clientStreams clients epochs e = case e.pool of
  UserPool v -> Right [[StreamUser (v V.! (i `mod` s))] | i <- idxs]
  TeamPool v -> Right [[StreamTeam (v V.! (i `mod` s))] | i <- idxs]
  ClientsPool v ->
    let perUser = maybe 0 (NE.length . snd) (v V.!? 0)
     in if clients > s * perUser
          then Left ("clients: " <> n clients <> " clients need at least that many (user, client id) pairs, the pool has " <> n (s * perUser))
          else Right [[let (uid, cids) = v V.! (i `mod` s) in StreamClient uid (cids NE.!! (i `div` s))] | i <- idxs]
  EpochPool v
    | epochs > s -> Left ("epoch: --epochs-per-client " <> n epochs <> " exceeds the " <> n s <> " epoch streams")
    | otherwise ->
        Right [[uncurry StreamEpoch (v V.! ((i * epochs + j) `mod` s)) | j <- [0 .. epochs - 1]] | i <- idxs]
  ConnectionsPool _ -> Left "connections targets are not supported by consume"
  where
    s = e.spec.streams
    idxs = [0 .. clients - 1]
    n = T.pack . show

-- | Notifications that reach readers when this push succeeds.
expectedDeliveries :: Map Key Int -> FanInPush -> Int
expectedDeliveries subs push =
  sum [Map.findWithDefault 0 k subs | t <- push.targets, k <- targetKeys t]

-- | Per entry: share of its pool streams that have at least one reader.
coverage :: Map Key Int -> V.Vector Entry -> [(TargetKind, Double)]
coverage subs = map one . V.toList
  where
    one e =
      let covered = length [() | i <- [0 .. e.spec.streams - 1], any (`Map.member` subs) (targetKeys (targetAt e.pool i))]
       in (e.spec.kind, fromIntegral covered / fromIntegral e.spec.streams)
```

- [ ] **Step 4: Run tests**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings; Population examples pass. If `transpose` or `isLeft` are not exported by `Imports`, import them from `Data.List` / `Data.Either`.

- [ ] **Step 5: Commit**

```bash
make format
git add tools/fan-in-perf
git commit -m "fan-in-perf: reader population"
```

---

### Task 6: Stats — `expected` counter and reader stats

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Stats.hs`
- Create: `tools/fan-in-perf/src/FanInPerf/ConsumeStats.hs`
- Create: `tools/fan-in-perf/test/FanInPerf/ConsumeStatsSpec.hs`
- Modify: `tools/fan-in-perf/test/FanInPerf/StatsSpec.hs`
- Modify: `tools/fan-in-perf/fan-in-perf.cabal` (`exposed-modules`: `FanInPerf.ConsumeStats`; test `other-modules`: `FanInPerf.ConsumeStatsSpec`)

**Interfaces:**
- Produces from `FanInPerf.Stats`: `recordExpected :: WriterStats -> Int -> IO ()`, `totalExpected :: Snapshot -> Int`, `toBuckets :: IntMap Int -> VU.Vector Int`, `minTickInterval :: Double` (now exported). `Snapshot` gains a strict `expected :: !Int` (summed by `sumSnapshots`, subtracted by `diffSnapshot`, 0 in `emptySnapshot`). `TickReport` and `recordSuccess` are unchanged.
- Produces from `FanInPerf.ConsumeStats`:
  ```haskell
  newReaderStats :: IO ReaderStats
  recordFetch :: ReaderStats -> TargetKind -> Word64 -> [Word64] -> IO ()  -- kind, fetch latency ns, lag ns of each returned notification
  recordFetchError :: ReaderStats -> TargetKind -> IO ()
  data ReaderSnapshot; emptyReaderSnapshot; readReaderSnapshot :: ReaderStats -> IO ReaderSnapshot
  sumReaderSnapshots :: [ReaderSnapshot] -> ReaderSnapshot; diffReaderSnapshot :: ReaderSnapshot -> ReaderSnapshot -> ReaderSnapshot
  deliveredOf, fetchesOf, emptyFetchesOf, fetchErrorsOf :: TargetKind -> ReaderSnapshot -> Int
  totalDelivered, totalFetches, totalEmptyFetches, totalFetchErrors :: ReaderSnapshot -> Int
  fetchLatencyBuckets, lagBuckets :: ReaderSnapshot -> VU.Vector Int
  data ConsumeTickState; initialConsumeTickState :: Double -> ConsumeTickState
  data ConsumeReport = ConsumeReport
    { consumeRate, maxConsumeRate, fetchRate, emptyFetchRatio, fetchErrorRate :: Double
    , lagP50, lagP99, fetchP50, fetchP99 :: Maybe Double
    , backlog :: Int, readerDelta, readerTotal :: ReaderSnapshot }
  consumeTick :: Double -> Double -> Int -> ReaderSnapshot -> ConsumeTickState -> (ConsumeReport, ConsumeTickState)
  --              warmup    now       expectedTotal  readerTotal
  ```

- [ ] **Step 1: Write the failing tests**

Append to `StatsSpec.hs` (inside `spec`):
```haskell
  describe "expected deliveries" $
    it "are summed over writers and diffed like the other counters" $ do
      a <- newWriterStats
      b <- newWriterStats
      recordExpected a 5
      s0 <- sumSnapshots <$> traverse readSnapshot [a, b]
      recordExpected a 2
      recordExpected b 10
      s1 <- sumSnapshots <$> traverse readSnapshot [a, b]
      (totalExpected s0, totalExpected s1, totalExpected (diffSnapshot s1 s0)) `shouldBe` (5, 17, 12)
```
`ConsumeStatsSpec.hs`:
```haskell
module FanInPerf.ConsumeStatsSpec (spec) where

import Data.Vector.Unboxed qualified as VU
import FanInPerf.ConsumeStats
import FanInPerf.Stats (bucketIndex)
import FanInPerf.Targets (TargetKind (..))
import Imports
import Test.Hspec

snapshotAfter :: Int -> Int -> IO ReaderSnapshot
snapshotAfter fetchesWith emptyFetches = do
  rs <- newReaderStats
  replicateM_ fetchesWith (recordFetch rs KindTeam 1000 [500, 500, 500])
  replicateM_ emptyFetches (recordFetch rs KindTeam 1000 [])
  readReaderSnapshot rs

spec :: Spec
spec = do
  describe "ReaderStats" $ do
    it "records fetches, deliveries, empty fetches, errors and histograms" $ do
      rs <- newReaderStats
      recordFetch rs KindUser 1000 [500, 500]
      recordFetch rs KindUser 2000 []
      recordFetchError rs KindEpoch
      s <- readReaderSnapshot rs
      (deliveredOf KindUser s, fetchesOf KindUser s, emptyFetchesOf KindUser s, fetchErrorsOf KindEpoch s)
        `shouldBe` (2, 2, 1, 1)
      (totalDelivered s, totalFetches s, totalEmptyFetches s, totalFetchErrors s) `shouldBe` (2, 2, 1, 1)
      VU.sum (fetchLatencyBuckets s) `shouldBe` 2
      lagBuckets s VU.! bucketIndex 500 `shouldBe` 2

    it "sums readers and diffs snapshots, including kinds absent from one side" $ do
      a <- newReaderStats
      b <- newReaderStats
      recordFetch a KindUser 1000 [1]
      s0 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      recordFetch a KindUser 1000 [1, 1]
      recordFetch b KindTeam 1000 [1]
      s1 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      let d = diffReaderSnapshot s1 s0
      (deliveredOf KindUser d, deliveredOf KindTeam d, totalFetches d, VU.sum (lagBuckets d)) `shouldBe` (2, 1, 2, 3)

  describe "consumeTick" $ do
    it "computes rates from the delta and ignores the max during warmup" $ do
      t1 <- snapshotAfter 10 0 -- 30 delivered
      let (r1, st1) = consumeTick 5 1 100 t1 (initialConsumeTickState 0)
      (r1.consumeRate, r1.maxConsumeRate, r1.fetchRate) `shouldBe` (30, 0, 10)
      t2 <- snapshotAfter 30 0 -- 90 delivered
      let (r2, _) = consumeTick 5 6 100 t2 st1
      (r2.consumeRate, r2.maxConsumeRate) `shouldBe` (12, 12)

    it "computes the empty-fetch ratio over the last tick and survives no traffic" $ do
      t <- snapshotAfter 1 3
      let (r, _) = consumeTick 0 1 10 t (initialConsumeTickState 0)
      r.emptyFetchRatio `shouldBe` 0.75
      let (r0, _) = consumeTick 0 0 0 emptyReaderSnapshot (initialConsumeTickState 0)
      [r0.consumeRate, r0.fetchRate, r0.emptyFetchRatio, r0.fetchErrorRate, r0.maxConsumeRate] `shouldBe` [0, 0, 0, 0, 0]
      (r0.lagP50, r0.lagP99, r0.fetchP50, r0.fetchP99) `shouldBe` (Nothing, Nothing, Nothing, Nothing)

    it "reports backlog = expected - delivered, never negative" $ do
      t <- snapshotAfter 10 0 -- 30 delivered
      fst (consumeTick 0 1 100 t (initialConsumeTickState 0)) `shouldSatisfy` ((== 70) . (.backlog))
      fst (consumeTick 0 1 10 t (initialConsumeTickState 0)) `shouldSatisfy` ((== 0) . (.backlog))

    it "a tiny final tick counts in totals but does not raise the max rate" $ do
      t1 <- snapshotAfter 10 0
      let (r1, st1) = consumeTick 5 10 100 t1 (initialConsumeTickState 0)
      r1.maxConsumeRate `shouldBe` 3
      t2 <- snapshotAfter 1000 0
      let (r2, _) = consumeTick 5 10.001 100 t2 st1
      r2.maxConsumeRate `shouldBe` 3
      totalDelivered r2.readerTotal `shouldBe` 3000
```

- [ ] **Step 2: Run to verify failure**

Run: `cabal test fan-in-perf-tests --test-options='-m "FanInPerf.Stats"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` (and `-m "FanInPerf.ConsumeStats"`)
Expected: compile errors (`recordExpected`, `FanInPerf.ConsumeStats` not found).

- [ ] **Step 3: Extend `Stats.hs`**

- Export list: add `recordExpected`, `totalExpected`, `toBuckets`, `minTickInterval`.
- `Snapshot` gets a last field `expected :: !Int`.
- `emptySnapshot = Snapshot mempty mempty mempty mempty 0`.
- `sumSnapshots`' `Snapshot {…}` gets `expected = x.expected + y.expected`; `diffSnapshot`'s gets `expected = new.expected - old.expected`.
- Add:
```haskell
-- | Notification deliveries readers should see because of the pushes recorded
-- so far (only used by experiment B).
recordExpected :: WriterStats -> Int -> IO ()
recordExpected stats n = modifyStats stats $ \s -> s {expected = s.expected + n}

totalExpected :: Snapshot -> Int
totalExpected s = s.expected

toBuckets :: IntMap Int -> VU.Vector Int
toBuckets m = VU.accum (+) (VU.replicate numBuckets 0) (IntMap.toList m)
```
- `latencyBuckets s = toBuckets s.latency`.

- [ ] **Step 4: Create `ConsumeStats.hs`**

```haskell
-- | Reader-side statistics, same scheme as "FanInPerf.Stats": one 'IORef' per
-- reader client written only by it, summed by the ticker.
module FanInPerf.ConsumeStats
  ( ReaderStats,
    newReaderStats,
    recordFetch,
    recordFetchError,
    ReaderSnapshot,
    emptyReaderSnapshot,
    readReaderSnapshot,
    sumReaderSnapshots,
    diffReaderSnapshot,
    deliveredOf,
    fetchesOf,
    emptyFetchesOf,
    fetchErrorsOf,
    totalDelivered,
    totalFetches,
    totalEmptyFetches,
    totalFetchErrors,
    fetchLatencyBuckets,
    lagBuckets,
    ConsumeTickState,
    initialConsumeTickState,
    ConsumeReport (..),
    consumeTick,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Stats (bucketIndex, minTickInterval, quantileSeconds, toBuckets)
import FanInPerf.Targets (TargetKind)
import Imports

data ReaderSnapshot = ReaderSnapshot
  { delivered :: !(Map TargetKind Int),
    fetches :: !(Map TargetKind Int),
    emptyFetches :: !(Map TargetKind Int),
    fetchErrors :: !(Map TargetKind Int),
    fetchLatency :: !(IntMap Int),
    lag :: !(IntMap Int)
  }
  deriving (Eq, Show)

newtype ReaderStats = ReaderStats (IORef ReaderSnapshot)

emptyReaderSnapshot :: ReaderSnapshot
emptyReaderSnapshot = ReaderSnapshot mempty mempty mempty mempty mempty mempty

newReaderStats :: IO ReaderStats
newReaderStats = ReaderStats <$> newIORef emptyReaderSnapshot

-- | See the memory-barrier note in "FanInPerf.Stats".
modifyReader :: ReaderStats -> (ReaderSnapshot -> ReaderSnapshot) -> IO ()
modifyReader (ReaderStats r) f = atomicModifyIORef' r (\s -> (f s, ()))

bump :: TargetKind -> Int -> Map TargetKind Int -> Map TargetKind Int
bump = Map.insertWith (+)

-- | One fetch of a stream of the given kind: its latency (ns) and the lag (ns)
-- of every notification it returned.
recordFetch :: ReaderStats -> TargetKind -> Word64 -> [Word64] -> IO ()
recordFetch stats k latencyNs lagsNs =
  modifyReader stats $ \s ->
    s
      { delivered = bump k (length lagsNs) s.delivered,
        fetches = bump k 1 s.fetches,
        emptyFetches = if null lagsNs then bump k 1 s.emptyFetches else s.emptyFetches,
        fetchLatency = IntMap.insertWith (+) (bucketIndex latencyNs) 1 s.fetchLatency,
        lag = foldl' (\m ns -> IntMap.insertWith (+) (bucketIndex ns) 1 m) s.lag lagsNs
      }

recordFetchError :: ReaderStats -> TargetKind -> IO ()
recordFetchError stats k = modifyReader stats $ \s -> s {fetchErrors = bump k 1 s.fetchErrors}

readReaderSnapshot :: ReaderStats -> IO ReaderSnapshot
readReaderSnapshot (ReaderStats r) = atomicModifyIORef' r (\s -> (s, s))

sumReaderSnapshots :: [ReaderSnapshot] -> ReaderSnapshot
sumReaderSnapshots = foldl' add emptyReaderSnapshot
  where
    add x y =
      ReaderSnapshot
        { delivered = Map.unionWith (+) x.delivered y.delivered,
          fetches = Map.unionWith (+) x.fetches y.fetches,
          emptyFetches = Map.unionWith (+) x.emptyFetches y.emptyFetches,
          fetchErrors = Map.unionWith (+) x.fetchErrors y.fetchErrors,
          fetchLatency = IntMap.unionWith (+) x.fetchLatency y.fetchLatency,
          lag = IntMap.unionWith (+) x.lag y.lag
        }

diffReaderSnapshot :: ReaderSnapshot -> ReaderSnapshot -> ReaderSnapshot
diffReaderSnapshot new old =
  ReaderSnapshot
    { delivered = diffMap new.delivered old.delivered,
      fetches = diffMap new.fetches old.fetches,
      emptyFetches = diffMap new.emptyFetches old.emptyFetches,
      fetchErrors = diffMap new.fetchErrors old.fetchErrors,
      fetchLatency = IntMap.unionWith (+) new.fetchLatency (negate <$> old.fetchLatency),
      lag = IntMap.unionWith (+) new.lag (negate <$> old.lag)
    }
  where
    diffMap n o = Map.unionWith (+) n (negate <$> o)

deliveredOf, fetchesOf, emptyFetchesOf, fetchErrorsOf :: TargetKind -> ReaderSnapshot -> Int
deliveredOf k s = Map.findWithDefault 0 k s.delivered
fetchesOf k s = Map.findWithDefault 0 k s.fetches
emptyFetchesOf k s = Map.findWithDefault 0 k s.emptyFetches
fetchErrorsOf k s = Map.findWithDefault 0 k s.fetchErrors

totalDelivered, totalFetches, totalEmptyFetches, totalFetchErrors :: ReaderSnapshot -> Int
totalDelivered s = sum s.delivered
totalFetches s = sum s.fetches
totalEmptyFetches s = sum s.emptyFetches
totalFetchErrors s = sum s.fetchErrors

fetchLatencyBuckets, lagBuckets :: ReaderSnapshot -> VU.Vector Int
fetchLatencyBuckets s = toBuckets s.fetchLatency
lagBuckets s = toBuckets s.lag

data ConsumeTickState = ConsumeTickState
  { startedAt :: Double,
    lastTickAt :: Double,
    previousSnapshot :: ReaderSnapshot,
    peakRate :: Double
  }

initialConsumeTickState :: Double -> ConsumeTickState
initialConsumeTickState now = ConsumeTickState now now emptyReaderSnapshot 0

data ConsumeReport = ConsumeReport
  { consumeRate :: Double,
    maxConsumeRate :: Double,
    fetchRate :: Double,
    -- | empty fetches / fetches during the last tick
    emptyFetchRatio :: Double,
    fetchErrorRate :: Double,
    lagP50 :: Maybe Double,
    lagP99 :: Maybe Double,
    fetchP50 :: Maybe Double,
    fetchP99 :: Maybe Double,
    -- | expected - delivered, clamped at 0: readers may see a push before the
    -- writer has recorded its expected deliveries.
    backlog :: Int,
    readerDelta :: ReaderSnapshot,
    readerTotal :: ReaderSnapshot
  }
  deriving (Eq, Show)

-- | @consumeTick warmup now expectedTotal readerTotal state@.
consumeTick :: Double -> Double -> Int -> ReaderSnapshot -> ConsumeTickState -> (ConsumeReport, ConsumeTickState)
consumeTick warmup now expectedTotal total st =
  let dt = now - st.lastTickAt
      delta = diffReaderSnapshot total st.previousSnapshot
      rate :: Int -> Double
      rate n = if dt > 0 then fromIntegral n / dt else 0
      elapsed = now - st.startedAt
      consumeRate = rate (totalDelivered delta)
      peak = if elapsed > warmup && dt >= minTickInterval then max st.peakRate consumeRate else st.peakRate
      fetchesInTick = totalFetches delta
      report =
        ConsumeReport
          { consumeRate,
            maxConsumeRate = peak,
            fetchRate = rate fetchesInTick,
            emptyFetchRatio = if fetchesInTick > 0 then fromIntegral (totalEmptyFetches delta) / fromIntegral fetchesInTick else 0,
            fetchErrorRate = rate (totalFetchErrors delta),
            lagP50 = quantileSeconds 0.5 (lagBuckets total),
            lagP99 = quantileSeconds 0.99 (lagBuckets total),
            fetchP50 = quantileSeconds 0.5 (fetchLatencyBuckets total),
            fetchP99 = quantileSeconds 0.99 (fetchLatencyBuckets total),
            backlog = max 0 (expectedTotal - totalDelivered total),
            readerDelta = delta,
            readerTotal = total
          }
   in (report, st {lastTickAt = now, previousSnapshot = total, peakRate = peak})
```

- [ ] **Step 5: Run tests**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings; all Stats / ConsumeStats / existing examples pass.

- [ ] **Step 6: Commit**

```bash
make format
git add tools/fan-in-perf
git commit -m "fan-in-perf: reader stats, expected deliveries"
```

---

### Task 7: Metrics for `consume`

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Metrics.hs`
- Modify: `tools/fan-in-perf/test/FanInPerf/MetricsSpec.hs`

**Interfaces:**
- Consumes: `FanInPerf.ConsumeStats` (`ConsumeReport (..)`, `deliveredOf`, `fetchesOf`, `emptyFetchesOf`, `fetchErrorsOf`, `fetchLatencyBuckets`, `lagBuckets`).
- Produces:
  ```haskell
  histogramSampleGroup :: Text -> Text -> Text -> VU.Vector Int -> P.SampleGroup  -- metric name, help, experiment, buckets
  data ConsumeMetrics
  newConsumeMetrics :: Text -> Int -> IO ConsumeMetrics   -- experiment, clients; call once
  publishConsume :: ConsumeMetrics -> ConsumeReport -> IO ()
  ```
  `latencySampleGroup`, `newMetrics`, `publish` keep their signatures.

Metric names: `fanin_perf_consumed_total{experiment,kind}`, `…_fetches_total`, `…_empty_fetches_total`, `…_fetch_errors_total` (counters); gauges `fanin_perf_consume_rate_current`, `fanin_perf_consume_rate_max`, `fanin_perf_backlog`, `fanin_perf_clients`; histograms `fanin_perf_fetch_duration_seconds`, `fanin_perf_delivery_lag_seconds`.

- [ ] **Step 1: Write the failing test**

Add imports to `MetricsSpec.hs`: `FanInPerf.ConsumeStats`. Append inside `spec`:
```haskell
  describe "publishConsume" $
    it "exports consume counters, gauges and histograms" $ do
      m <- newConsumeMetrics "specc" 7
      rs <- newReaderStats
      replicateM_ 3 (recordFetch rs KindTeam 1000 [500])
      recordFetch rs KindTeam 1000 []
      total <- readReaderSnapshot rs
      let (r, _) = consumeTick 0 1 10 total (initialConsumeTickState 0)
      publishConsume m r
      out <- LBS8.unpack <$> P.exportMetricsAsText
      out `shouldContain` "fanin_perf_consumed_total{experiment=\"specc\",kind=\"team\"} 3"
      out `shouldContain` "fanin_perf_empty_fetches_total{experiment=\"specc\",kind=\"team\"} 1"
      out `shouldContain` "fanin_perf_consume_rate_current{experiment=\"specc\"} 3"
      out `shouldContain` "fanin_perf_backlog{experiment=\"specc\"} 7"
      out `shouldContain` "fanin_perf_clients{experiment=\"specc\"} 7"
      out `shouldContain` "fanin_perf_delivery_lag_seconds_count{experiment=\"specc\"} 3"
      out `shouldContain` "fanin_perf_fetch_duration_seconds_count{experiment=\"specc\"} 4"
```

- [ ] **Step 2: Run to verify failure**

Run: `cabal test fan-in-perf-tests --test-options='-m "FanInPerf.Metrics"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error (`newConsumeMetrics` not in scope).

- [ ] **Step 3: Implement**

In `Metrics.hs`:
- Export `histogramSampleGroup`, `ConsumeMetrics`, `newConsumeMetrics`, `publishConsume`. Import `FanInPerf.ConsumeStats`.
- Lift the two local helpers of `newMetrics` to top level and use them there (remove its `let`):
```haskell
counterVec :: Text -> Text -> IO (P.Vector P.Label2 P.Counter)
counterVec name help = P.register $ P.vector ("experiment", "kind") $ P.counter (P.Info name help)

gaugeVec :: Text -> Text -> IO (P.Vector P.Label1 P.Gauge)
gaugeVec name help = P.register $ P.vector "experiment" $ P.gauge (P.Info name help)
```
- Generalise the histogram code (replace `latencyMetric` and the head of `latencySampleGroup`):
```haskell
-- | Histogram served from the ticker's latest bucket snapshot (see 'histogramSampleGroup').
histogramMetric :: Text -> Text -> Text -> IORef (VU.Vector Int) -> P.Metric ()
histogramMetric name help experiment ref =
  P.Metric $ pure ((), (: []) . histogramSampleGroup name help experiment <$> readIORef ref)

latencySampleGroup :: Text -> VU.Vector Int -> P.SampleGroup
latencySampleGroup = histogramSampleGroup "fanin_perf_push_duration_seconds" "Push latency in seconds"

histogramSampleGroup :: Text -> Text -> Text -> VU.Vector Int -> P.SampleGroup
histogramSampleGroup name help experiment buckets =
  P.SampleGroup info P.HistogramType (bucketSamples <> [sumSample, countSample])
  where
    info = P.Info name help
    -- … the rest of the existing `latencySampleGroup` where-block, unchanged
    -- (lbl, count, cumulative, bucketSamples, approxSum, sumSample, countSample, bshow),
    -- minus its local `name` and `info` bindings …
```
  and in `newMetrics` register with `histogramMetric "fanin_perf_push_duration_seconds" "Push latency in seconds" experimentName latency`. Keep the existing `-- REVIEW:` comment on `approxSum` as is.
- Add:
```haskell
data ConsumeMetrics = ConsumeMetrics
  { experiment :: Text,
    consumed :: P.Vector P.Label2 P.Counter,
    fetchCount :: P.Vector P.Label2 P.Counter,
    emptyFetchCount :: P.Vector P.Label2 P.Counter,
    fetchErrorCount :: P.Vector P.Label2 P.Counter,
    consumeRateCurrent :: P.Vector P.Label1 P.Gauge,
    consumeRateMax :: P.Vector P.Label1 P.Gauge,
    backlogGauge :: P.Vector P.Label1 P.Gauge,
    fetchLatencyRef :: IORef (VU.Vector Int),
    lagRef :: IORef (VU.Vector Int)
  }

-- | Registers the consume metrics; call once per process.
newConsumeMetrics :: Text -> Int -> IO ConsumeMetrics
newConsumeMetrics experiment clients = do
  consumed <- counterVec "fanin_perf_consumed_total" "Notifications delivered to readers"
  fetchCount <- counterVec "fanin_perf_fetches_total" "Fetches of a stream"
  emptyFetchCount <- counterVec "fanin_perf_empty_fetches_total" "Fetches that returned no notification"
  fetchErrorCount <- counterVec "fanin_perf_fetch_errors_total" "Failed fetches"
  consumeRateCurrent <- gaugeVec "fanin_perf_consume_rate_current" "Delivered notifications per second during the last tick"
  consumeRateMax <- gaugeVec "fanin_perf_consume_rate_max" "Maximal delivered notifications per second after warmup"
  backlogGauge <- gaugeVec "fanin_perf_backlog" "Expected minus delivered notifications (clamped at 0)"
  clientsGauge <- gaugeVec "fanin_perf_clients" "Simulated reader clients"
  P.withLabel clientsGauge experiment (`P.setGauge` fromIntegral clients)
  fetchLatencyRef <- newIORef (VU.replicate numBuckets 0)
  lagRef <- newIORef (VU.replicate numBuckets 0)
  _ <- P.register (histogramMetric "fanin_perf_fetch_duration_seconds" "Fetch latency in seconds" experiment fetchLatencyRef)
  _ <- P.register (histogramMetric "fanin_perf_delivery_lag_seconds" "Time from a notification's creation to its delivery to a reader" experiment lagRef)
  pure ConsumeMetrics {experiment, consumed, fetchCount, emptyFetchCount, fetchErrorCount, consumeRateCurrent, consumeRateMax, backlogGauge, fetchLatencyRef, lagRef}

-- | Only the ticker thread calls this.
publishConsume :: ConsumeMetrics -> ConsumeReport -> IO ()
publishConsume m r = do
  for_ allKinds $ \k -> do
    let lbl = (m.experiment, kindName k)
    add m.consumed lbl (deliveredOf k r.readerDelta)
    add m.fetchCount lbl (fetchesOf k r.readerDelta)
    add m.emptyFetchCount lbl (emptyFetchesOf k r.readerDelta)
    add m.fetchErrorCount lbl (fetchErrorsOf k r.readerDelta)
  set m.consumeRateCurrent r.consumeRate
  set m.consumeRateMax r.maxConsumeRate
  set m.backlogGauge (fromIntegral r.backlog)
  writeIORef m.fetchLatencyRef (fetchLatencyBuckets r.readerTotal)
  writeIORef m.lagRef (lagBuckets r.readerTotal)
  where
    add v lbl n = when (n > 0) $ P.withLabel v lbl (void . (`P.addCounter` fromIntegral n))
    set g x = P.withLabel g m.experiment (`P.setGauge` x)
```

- [ ] **Step 4: Run tests**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings; new and existing Metrics examples pass (the existing `latencySampleGroup`/`publish` examples prove the refactor kept behaviour).

- [ ] **Step 5: Commit**

```bash
make format
git add tools/fan-in-perf
git commit -m "fan-in-perf: consume metrics"
```

---

### Task 8: Terminal output for `consume`

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Terminal.hs`
- Modify: `tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs`

**Interfaces:**
- Consumes: `ConsumeReport (..)`, `totalDelivered`, `totalFetches`, `totalEmptyFetches` from `FanInPerf.ConsumeStats`; `TargetKind`, `kindName` from `FanInPerf.Targets`.
- Produces:
  ```haskell
  formatConsumeStatus :: TickReport -> ConsumeReport -> Text
  formatConsumeSummary :: Int -> Bool -> TickReport -> ConsumeReport -> [Text]  -- expected deliveries, drained?
  formatCoverage :: [(TargetKind, Double)] -> [Text]
  ```

- [ ] **Step 1: Write the failing tests**

In `TerminalSpec.hs` add imports `FanInPerf.ConsumeStats`, `FanInPerf.Targets (TargetKind (..))`, and:
```haskell
creport :: ConsumeReport
creport =
  ConsumeReport
    { consumeRate = 8000.2,
      maxConsumeRate = 9000,
      fetchRate = 20000,
      emptyFetchRatio = 0.5,
      fetchErrorRate = 1,
      lagP50 = Just 0.004,
      lagP99 = Just 0.05,
      fetchP50 = Just 0.001,
      fetchP99 = Just 0.01,
      backlog = 1234,
      readerDelta = emptyReaderSnapshot,
      readerTotal = emptyReaderSnapshot
    }

deliveredSnapshot :: IO ReaderSnapshot
deliveredSnapshot = do
  rs <- newReaderStats
  recordFetch rs KindUser 1000 [1000, 1000, 1000]
  recordFetch rs KindUser 1000 []
  readReaderSnapshot rs
```
Append to `spec`:
```haskell
  describe "formatConsumeStatus" $
    it "renders the consume status line" $
      formatConsumeStatus report creport
        `shouldBe` "t=42s push/s cur=8,312 max=9,105 | cons/s cur=8,000 max=9,000 | lag(cum.) p50=4.0ms p99=50.0ms | backlog=1,234 | err/s push=3 fetch=1"

  describe "formatConsumeSummary" $ do
    it "reports missing notifications and a timed-out drain" $ do
      s <- deliveredSnapshot
      let out = formatConsumeSummary 5 False report creport {readerTotal = s}
      out `shouldSatisfy` any (T.isInfixOf "delivered=3 expected=5 missing=2 duplicates=0")
      out `shouldSatisfy` any (T.isInfixOf "drain: timed out")
    it "reports duplicates and a complete drain" $ do
      s <- deliveredSnapshot
      let out = formatConsumeSummary 2 True report creport {readerTotal = s}
      out `shouldSatisfy` any (T.isInfixOf "missing=0 duplicates=1")
      out `shouldSatisfy` any (T.isInfixOf "drain: complete")
    it "survives an empty run" $
      formatConsumeSummary 0 True report {elapsed = 0} creport {lagP50 = Nothing, lagP99 = Nothing}
        `shouldSatisfy` (not . any (T.isInfixOf "NaN"))

  describe "formatCoverage" $
    it "warns below 100%" $
      formatCoverage [(KindUser, 0.02), (KindTeam, 1)]
        `shouldBe` [ "coverage user: 2.00% of streams have a reader (warning: consumed/s will stay below targets/s)",
                     "coverage team: 100.00% of streams have a reader"
                   ]
```

- [ ] **Step 2: Run to verify failure**

Run: `cabal test fan-in-perf-tests --test-options='-m "FanInPerf.Terminal"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error (`formatConsumeStatus` not in scope).

- [ ] **Step 3: Implement**

In `Terminal.hs` export the three functions, add `import FanInPerf.ConsumeStats` and `import FanInPerf.Targets (TargetKind, kindName)`, and add:
```haskell
formatConsumeStatus :: TickReport -> ConsumeReport -> Text
formatConsumeStatus r c =
  T.intercalate
    " | "
    [ "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> rateText r.maxPushRate,
      "cons/s cur=" <> rateText c.consumeRate <> " max=" <> rateText c.maxConsumeRate,
      "lag(cum.) p50=" <> formatLatency c.lagP50 <> " p99=" <> formatLatency c.lagP99,
      "backlog=" <> groupThousands c.backlog,
      "err/s push=" <> rateText r.errorRate <> " fetch=" <> rateText c.fetchErrorRate
    ]

-- | @formatConsumeSummary expectedDeliveries drained pushReport consumeReport@.
formatConsumeSummary :: Int -> Bool -> TickReport -> ConsumeReport -> [Text]
formatConsumeSummary expected drained r c =
  let delivered = totalDelivered c.readerTotal
      fetches = totalFetches c.readerTotal
      emptyRatio = if fetches > 0 then fromIntegral (totalEmptyFetches c.readerTotal) / fromIntegral fetches else 0
      avgRate = if r.elapsed > 0 then fromIntegral delivered / r.elapsed else 0
   in formatSummary r
        <> [ "consume: delivered=" <> groupThousands delivered <> " expected=" <> groupThousands expected
               <> " missing=" <> groupThousands (max 0 (expected - delivered))
               <> " duplicates=" <> groupThousands (max 0 (delivered - expected)),
             "  cons/s avg=" <> rateText avgRate <> " max=" <> rateText c.maxConsumeRate,
             "  lag (cumulative) p50=" <> formatLatency c.lagP50 <> " p99=" <> formatLatency c.lagP99,
             "  fetch (cumulative) p50=" <> formatLatency c.fetchP50 <> " p99=" <> formatLatency c.fetchP99 <> " empty=" <> percent emptyRatio,
             "  drain: " <> if drained then "complete" else "timed out"
           ]

formatCoverage :: [(TargetKind, Double)] -> [Text]
formatCoverage = map line
  where
    line (k, c) =
      "coverage " <> kindName k <> ": " <> percent c <> " of streams have a reader"
        <> if c < 1 then " (warning: consumed/s will stay below targets/s)" else ""
```

- [ ] **Step 3b: Run tests**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings; all Terminal examples pass.

- [ ] **Step 4: Commit**

```bash
make format
git add tools/fan-in-perf
git commit -m "fan-in-perf: consume terminal output"
```

---

### Task 9: Reader loop, `runConsume`, wiring

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Consume.hs`
- Create: `tools/fan-in-perf/test/FanInPerf/ConsumeSpec.hs`
- Modify: `tools/fan-in-perf/src/FanInPerf/Produce.hs` (exports, extract `printQueuedErrors`)
- Modify: `tools/fan-in-perf/src/FanInPerf/Run.hs`
- Modify: `tools/fan-in-perf/fan-in-perf.cabal` (`exposed-modules`: `FanInPerf.Consume`; library `build-depends`: `time`; test `other-modules`: `FanInPerf.ConsumeSpec`, test `build-depends`: `polysemy`, `uuid-types`), then `make regen-local-nix-derivations`.

**Interfaces:**
- Consumes: everything from Tasks 2–8 (`fetchNotifications`, `getLastNotificationId`, `notificationIdMillis`, `Population`, `ConsumeStats`, `ConsumeMetrics`, `formatConsume*`, `Store.Env`, `runStore`, `initPostgresPoolSharingMetrics`).
- Produces:
  ```haskell
  data Sub = Sub {kind :: !TargetKind, stream :: !Stream, cursor :: !UUID}
  loadCursors :: Member FanInNotificationsStore r => Client -> Sem r [Sub]
  readerPass :: forall e r. (Show e, Members '[FanInNotificationsStore, Error e, Embed IO] r)
             => Int -> ReaderStats -> [Sub] -> Sem r ([Sub], Bool, [Text])
             -- limit; returns advanced cursors, "some stream returned a full page", error messages
  runConsume :: Console -> Env -> Env -> ConsumeOptions -> IO ()  -- writer env, reader env
  -- Produce additionally exports: storePush, waitForStop, printQueuedErrors
  ```
  `readerPass` semantics: for each sub, `fetchNotifications stream cursor limit` inside `Polysemy.Error.try`; on success record fetch (latency, lag of each notification = now − UUIDv7 ms timestamp, clamped to 0), advance cursor to the last returned id; on error record a fetch error, keep the cursor, return a message, never signal "full page".

- [ ] **Step 1: Write the failing tests**

`ConsumeSpec.hs`:
```haskell
module FanInPerf.ConsumeSpec (spec) where

import Data.Aeson qualified as A
import Data.Id
import Data.Text qualified as T
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Consume
import FanInPerf.ConsumeStats
import FanInPerf.Population (Client (..))
import FanInPerf.Targets (TargetKind (..))
import Imports
import Polysemy
import Polysemy.Error
import Test.Hspec
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres (genNotificationId)

type Rows = IORef [(Stream, [(UUID, A.Value)])]

-- | In-memory store; every fetch fails while the flag is set.
fakeStore :: (Member (Embed IO) r, Member (Error Text) r) => Rows -> IORef Bool -> InterpreterFor FanInNotificationsStore r
fakeStore rows failing = interpret $ \case
  PushViaFanIn _ -> pure ()
  FetchNotifications s cursor limit -> do
    down <- embed (readIORef failing)
    when down (throw "store down")
    rs <- embed (fromMaybe [] . lookup s <$> readIORef rows)
    pure (take limit (filter ((> cursor) . fst) rs))
  GetLastNotificationId s -> do
    rs <- embed (fromMaybe [] . lookup s <$> readIORef rows)
    pure (fst <$> listToMaybe (reverse rs))

runFake :: Rows -> IORef Bool -> Sem '[FanInNotificationsStore, Error Text, Embed IO] a -> IO a
runFake rows failing = runM . runError . fakeStore rows failing >=> either (fail . T.unpack) pure

newIds :: Int -> IO [(UUID, A.Value)]
newIds n = replicateM n ((\i -> (i.toUUID, A.Null)) <$> genNotificationId @())

team :: Stream
team = StreamTeam (Id (UUID.fromWords 1 2 3 4))

setup :: [(UUID, A.Value)] -> IO (Rows, IORef Bool)
setup rs = (,) <$> newIORef [(team, rs)] <*> newIORef False

sub0 :: Sub
sub0 = Sub {kind = KindTeam, stream = team, cursor = UUID.nil}

pass :: Rows -> IORef Bool -> Int -> ReaderStats -> [Sub] -> IO ([Sub], Bool, [Text])
pass rows failing limit stats subs = runFake rows failing (readerPass @Text limit stats subs)

spec :: Spec
spec = do
  describe "loadCursors" $ do
    it "starts at the latest notification of a stream" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      subs <- runFake rows failing (loadCursors (Client [team]))
      map (.cursor) subs `shouldBe` [fst (last rs)]
    it "starts at the nil UUID for a stream nothing was written to" $ do
      (rows, failing) <- setup []
      subs <- runFake rows failing (loadCursors (Client [team]))
      map (.cursor) subs `shouldBe` [UUID.nil]

  describe "readerPass" $ do
    it "delivers a page, advances the cursor and records lag" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      stats <- newReaderStats
      (subs, more, errs) <- pass rows failing 10 stats [sub0]
      map (.cursor) subs `shouldBe` [fst (last rs)]
      (more, errs) `shouldBe` (False, [])
      s <- readReaderSnapshot stats
      (totalDelivered s, totalFetches s, VU.sum (lagBuckets s)) `shouldBe` (3, 1, 3)

    it "signals a full page, and pages through a backlog without duplicates or gaps" $ do
      rs <- newIds 5
      (rows, failing) <- setup rs
      stats <- newReaderStats
      (s1, more1, _) <- pass rows failing 2 stats [sub0]
      (s2, more2, _) <- pass rows failing 2 stats s1
      (s3, more3, _) <- pass rows failing 2 stats s2
      (more1, more2, more3) `shouldBe` (True, True, False)
      map (.cursor) s3 `shouldBe` [fst (last rs)]
      s <- readReaderSnapshot stats
      totalDelivered s `shouldBe` 5

    it "delivers only newer notifications on the next pass" $ do
      rs <- newIds 2
      (rows, failing) <- setup rs
      stats <- newReaderStats
      (s1, _, _) <- pass rows failing 10 stats [sub0]
      more <- newIds 1
      modifyIORef rows (map (\(k, v) -> (k, v <> more)))
      _ <- pass rows failing 10 stats s1
      s <- readReaderSnapshot stats
      (totalDelivered s, totalEmptyFetches s) `shouldBe` (3, 0)

    it "counts a failed fetch, keeps the cursor and does not claim a full page" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      writeIORef failing True
      stats <- newReaderStats
      (subs, more, errs) <- pass rows failing 10 stats [sub0]
      map (.cursor) subs `shouldBe` [UUID.nil]
      more `shouldBe` False
      length errs `shouldBe` 1
      s <- readReaderSnapshot stats
      (totalFetchErrors s, totalDelivered s) `shouldBe` (1, 0)

    it "never delivers a notification older than the cursor (RFC cursor-model hazard)" $ do
      -- A takes the older id, B the newer one; B commits and is read first, A commits late.
      [(a, _), (b, _)] <- newIds 2
      (rows, failing) <- setup [(b, A.Null)]
      stats <- newReaderStats
      (s1, _, _) <- pass rows failing 10 stats [sub0]
      writeIORef rows [(team, [(a, A.Null), (b, A.Null)])]
      _ <- pass rows failing 10 stats s1
      s <- readReaderSnapshot stats
      totalDelivered s `shouldBe` 1 -- 'a' is lost; the run reports it as `missing`
```
Imports needed beyond those above: `Control.Monad ((>=>))` (if not in `Imports`).

- [ ] **Step 2: Run to verify failure**

Run: `cabal test fan-in-perf-tests --test-options='-m "FanInPerf.Consume"' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile error, module `FanInPerf.Consume` not found.

- [ ] **Step 3: Produce.hs refactor**

Export `storePush`, `waitForStop`, `printQueuedErrors` (add to the export list). Replace the error-printing block of `tickOnce` and its `where maxErrorLines = 10` with a call, and add the function:
```haskell
-- | Prints queued error messages (at most 'maxErrorLines' per call).
printQueuedErrors :: Console -> TBQueue Text -> IO ()
printQueuedErrors console errors = do
  msgs <- atomically (flushTBQueue errors)
  traverse_ (printLine console) (take maxErrorLines msgs)
  when (length msgs > maxErrorLines) $
    printLine console ("... " <> T.pack (show (length msgs - maxErrorLines)) <> " more errors suppressed")

maxErrorLines :: Int
maxErrorLines = 10
```
`tickOnce` then reads: `… (report, st') = tick …; writeIORef ref st'; printQueuedErrors console errors; publish metrics report; drawStatus …; pure report`.

- [ ] **Step 4: Create `Consume.hs`**

```haskell
-- | Experiment B: writers as in "FanInPerf.Produce" plus simulated reader
-- clients that fetch the streams they subscribed to and keep their cursors in
-- memory (no acknowledgements).
module FanInPerf.Consume
  ( Sub (..),
    loadCursors,
    readerPass,
    runConsume,
  )
where

import Control.Concurrent.Async
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import FanInPerf.ConsumeStats
import FanInPerf.Metrics
import FanInPerf.Options (ConsumeOptions (..), ProduceOptions (..))
import FanInPerf.Population
import FanInPerf.Produce
import FanInPerf.Stats
import FanInPerf.Store
import FanInPerf.Targets
import FanInPerf.Terminal
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Hasql.Pool (UsageError)
import Imports
import Polysemy
import Polysemy.Error qualified as PE
import System.Exit (die)
import System.Random
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres (notificationIdMillis)

-- | A stream a client reads and its cursor: the id of the last notification seen.
data Sub = Sub {kind :: !TargetKind, stream :: !Stream, cursor :: !UUID}

-- | Start position of every stream of a client: the latest notification id
-- ("start at latest"), the nil UUID for an empty stream.
loadCursors :: (Member FanInNotificationsStore r) => Client -> Sem r [Sub]
loadCursors c =
  for c.streams $ \s -> do
    lastId <- getLastNotificationId s
    pure Sub {kind = streamKind s, stream = s, cursor = fromMaybe UUID.nil lastId}

-- | Fetches every stream of one client once. Returns the advanced cursors,
-- whether some stream returned a full page (more is waiting) and the messages
-- of failed fetches. A failed fetch leaves its cursor unchanged.
readerPass ::
  forall e r.
  (Show e, Members '[FanInNotificationsStore, PE.Error e, Embed IO] r) =>
  Int ->
  ReaderStats ->
  [Sub] ->
  Sem r ([Sub], Bool, [Text])
readerPass limit stats subs = do
  results <- for subs $ \sub -> do
    t0 <- embed getMonotonicTimeNSec
    fetched <- PE.try @e (fetchNotifications sub.stream sub.cursor limit)
    t1 <- embed getMonotonicTimeNSec
    case fetched of
      Left err -> do
        embed (recordFetchError stats sub.kind)
        pure (sub, False, [kindName sub.kind <> ": fetch failed: " <> T.pack (show err)])
      Right page -> do
        nowMs <- embed currentMillis
        embed (recordFetch stats sub.kind (t1 - t0) (map (lagNs nowMs . fst) page))
        let cursor' = maybe sub.cursor fst (listToMaybe (reverse page))
        pure (sub {cursor = cursor'}, length page >= limit, [])
  pure ([s | (s, _, _) <- results], or [full | (_, full, _) <- results], concat [errs | (_, _, errs) <- results])

currentMillis :: IO Word64
currentMillis = floor . (* 1000) <$> getPOSIXTime

-- | Time since the notification was created (ms timestamp of its UUIDv7) in ns.
lagNs :: Word64 -> UUID -> Word64
lagNs nowMs notifId =
  let created = notificationIdMillis notifId
   in if nowMs > created then (nowMs - created) * 1_000_000 else 0

-- | One reader client: fetch all streams, then sleep unless a full page says
-- more is waiting. Failures are reported and slow the loop down, never stop it.
readerLoop :: Env -> ConsumeOptions -> ReaderStats -> (Text -> IO ()) -> [Sub] -> IO ()
readerLoop env opts stats reportError = go
  where
    pause = threadDelay (opts.pollIntervalMs * 1000)
    go subs =
      runStore env (readerPass @UsageError opts.fetchLimit stats subs) >>= \case
        Left e -> reportError ("reader: " <> T.pack (show e)) >> pause >> go subs
        Right (subs', more, errs) -> do
          traverse_ reportError errs
          unless more pause
          go subs'

-- | @runConsume console writerEnv readerEnv options@. The environments differ
-- only in their connection pool.
runConsume :: Console -> Env -> Env -> ConsumeOptions -> IO ()
runConsume console writerEnv readerEnv opts = do
  let p = opts.writerOptions
  seed <- maybe randomIO pure p.seed
  let (entries, gen') = mkEntries p.clientsPerUser p.targets (mkStdGen seed)
      payload = mkPayload p.payloadBytes
      writerGens = take p.writers (unfoldr (Just . split) gen')
  population <- either (die . T.unpack) pure (mkPopulation opts.clients opts.epochsPerClient entries)
  printLine console $
    "consume: writers=" <> tshow p.writers <> " clients=" <> tshow opts.clients
      <> " targets=" <> renderTargetSpec p.targets <> " seed=" <> tshow seed
  traverse_ (printLine console) (formatCoverage (coverage population.subscribers entries))
  -- Start positions are read before any writer runs: a push committed in
  -- between could otherwise sit below a cursor and show up as a false `missing`.
  printLine console "consume: loading start positions"
  loaded <- forConcurrently population.simClients (runStore readerEnv . loadCursors)
  clientSubs <- either (\e -> die ("loading start positions failed: " <> show e)) pure (sequence loaded)

  metrics <- newMetrics "consume" p.writers
  consumeMetrics <- newConsumeMetrics "consume" opts.clients
  writerStats <- replicateM p.writers newWriterStats
  readerStats <- replicateM opts.clients newReaderStats
  errors <- newTBQueueIO 1000
  stopWriters <- newIORef False
  let -- error path only; drops messages when the ticker falls behind
      reportError msg = atomically $ do
        full <- isFullTBQueue errors
        unless full (writeTBQueue errors msg)
      -- A writer stops between pushes, so every committed push has its
      -- expected deliveries recorded before the drain starts.
      writer (ws, g0) =
        let doPush push = do
              outcome <- storePush writerEnv push
              case outcome of
                PushOk -> recordExpected ws (expectedDeliveries population.subscribers push)
                PushFailed _ -> pure ()
              pure outcome
            loop !g = do
              stop <- readIORef stopWriters
              unless stop (writerStep doPush payload entries ws reportError g >>= loop)
         in loop g0
      reader (stats, subs) = readerLoop readerEnv opts stats reportError subs
      totals = do
        expected <- totalExpected . sumSnapshots <$> traverse readSnapshot writerStats
        delivered <- totalDelivered . sumReaderSnapshots <$> traverse readReaderSnapshot readerStats
        pure (expected, delivered)
      -- True when readers caught up before the timeout
      drain = do
        deadline <- (+ fromIntegral opts.drainTimeout) <$> getMonotonicTime
        let go = do
              (expected, delivered) <- totals
              now <- getMonotonicTime
              if delivered >= expected
                then pure True
                else
                  if now >= deadline
                    then pure False
                    else threadDelay 100_000 >> go
        go
  start <- getMonotonicTime
  stateRef <- newIORef (initialTickState start, initialConsumeTickState start)
  let warmup = fromIntegral p.warmup
      doTick = do
        now <- getMonotonicTime
        writerTotal <- sumSnapshots <$> traverse readSnapshot writerStats
        readerTotal <- sumReaderSnapshots <$> traverse readReaderSnapshot readerStats
        (writerState, readerState) <- readIORef stateRef
        let (writerReport, writerState') = tick warmup now writerTotal writerState
            (readerReport, readerState') = consumeTick warmup now (totalExpected writerTotal) readerTotal readerState
        writeIORef stateRef (writerState', readerState')
        printQueuedErrors console errors
        publish metrics writerReport
        publishConsume consumeMetrics readerReport
        drawStatus console (formatConsumeStatus writerReport readerReport)
        pure (writerReport, readerReport)
  printLine console "consume: running"
  drained <-
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      withAsync (mapConcurrently_ reader (zip readerStats clientSubs)) $ \readersA -> do
        link readersA
        withAsync (mapConcurrently_ writer (zip writerStats writerGens)) $ \writersA -> do
          link writersA
          waitForStop p.duration
          writeIORef stopWriters True
          wait writersA
          printLine console "consume: writers stopped, draining"
          drain
  -- ticker and readers are cancelled now; the final tick and summary run here only
  (writerReport, readerReport) <- doTick
  traverse_ (printLine console) (formatConsumeSummary (totalExpected writerReport.total) drained writerReport readerReport)
  where
    tshow :: (Show a) => a -> Text
    tshow = T.pack . show
```
(`for`, `forever`, `void`, `unfoldr`, `newTBQueueIO` etc. come from `Imports`/`Control.Concurrent.Async`; if the compiler reports a missing name, import it from the module the existing `Produce.hs` uses for it. `renderTargetSpec` is exported by `FanInPerf.Targets`.)

- [ ] **Step 5: Wire into `Run.hs`**

Add imports `FanInPerf.Consume (runConsume)` and extend the `Hasql.Pool.Extended` import with `initPostgresPoolSharingMetrics`. Replace the temporary arm from Task 4 with:
```haskell
    Consume c ->
      withAsync (runMetricsServer opts.global.metricsPort) $ \server -> do
        link server
        -- readers get their own pool so they cannot starve the writers
        readerPool <- initPostgresPoolSharingMetrics pool (poolConfig c.readerPoolSize) connStr Nothing
        let readerEnv = Env {pool = readerPool, local = env.local, isolation = env.isolation}
        runConsume console env readerEnv c
```

- [ ] **Step 6: Cabal and Nix**

Add the cabal entries listed under **Files**. Run `make regen-local-nix-derivations | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` and check `git diff --stat tools/fan-in-perf/default.nix` shows only the new deps.

- [ ] **Step 7: Build and test**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no warnings; all examples pass, including the cursor-hazard one (documents the limitation) and the Produce examples (refactor is behaviour-preserving). If polysemy cannot solve `Member (PE.Error e) r` in `readerPass` calls, make the `e` application explicit (`@Text` in the test, `@UsageError` in `readerLoop`; both already are).

- [ ] **Step 8: Commit**

```bash
make format
git add tools/fan-in-perf
git commit -m "fan-in-perf: consume sub-command"
```

---

### Task 10: Docs, dashboard, A-spec drift, acceptance

**Files:**
- Modify: `tools/fan-in-perf/README.md`
- Modify: `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json`
- Modify: `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md`

**Interfaces:** none.

- [ ] **Step 1: README**

Append after the `produce` example in `README.md`:
```markdown
Experiment B (readers): `consume` runs the writers of `produce` plus simulated
reader clients (`--clients`, default 2000; `--epochs-per-client`,
`--poll-interval`, `--fetch-limit`, `--reader-pool-size`, `--drain-timeout`).

```sh
fan-in-perf --db "$DB" consume --targets user:2000,team:1,epoch:200 --duration 30
```

Readers subscribe to the same stream pools as the writers (client `i` reads
stream `i mod STREAMS` of each kind, `--epochs-per-client` consecutive epoch
streams); startup prints how many pool streams have a reader. `connections`
targets are not supported. Readers keep their cursors in memory and start at
the latest notification of each stream (`GetLastNotificationId`), taken before
the writers start. After the writers stop, readers get `--drain-timeout`
seconds to catch up; the summary reports `missing`/`duplicates`. A non-zero
`missing` means a notification committed after a reader's cursor had already
passed its (smaller, pre-transaction generated) UUIDv7 — a finding about the
RFC's `> lastId` model, not a tool bug. Delivery lag is measured from the
millisecond timestamp inside the UUIDv7 (writers and readers share a clock).
```
(Use a real fenced block for the sh example inside the file; the nested fence above is only escaping for this plan.)

- [ ] **Step 2: Grafana panels**

From `deploy/dockerephemeral/docker/grafana-dashboards/`:
```bash
jq --indent 2 '
  def panel($id; $title; $x; $y; $targets):
    {id: $id, type: "timeseries", title: $title, datasource: null, interval: "1s",
     gridPos: {x: $x, y: $y, w: 12, h: 8},
     targets: [$targets | to_entries[] | {refId: ("ABCDE"[.key:.key+1]), expr: .value[0], legendFormat: .value[1]}]};
  .panels += [
    panel(8; "Consume rate (notifications/s)"; 0; 32;
      [["sum(rate(fanin_perf_consumed_total[5s]))", "delivered/s (5s avg)"],
       ["fanin_perf_consume_rate_current", "delivered/s (last tick)"],
       ["fanin_perf_consume_rate_max", "peak delivered/s (after warmup)"]]),
    panel(9; "Delivery lag"; 12; 32;
      [["histogram_quantile(0.5, sum by (le) (rate(fanin_perf_delivery_lag_seconds_bucket[30s])))", "p50"],
       ["histogram_quantile(0.99, sum by (le) (rate(fanin_perf_delivery_lag_seconds_bucket[30s])))", "p99"]]),
    panel(10; "Backlog (expected - delivered)"; 0; 40;
      [["fanin_perf_backlog", "backlog"]]),
    panel(11; "Fetch latency"; 12; 40;
      [["histogram_quantile(0.5, sum by (le) (rate(fanin_perf_fetch_duration_seconds_bucket[30s])))", "p50"],
       ["histogram_quantile(0.99, sum by (le) (rate(fanin_perf_fetch_duration_seconds_bucket[30s])))", "p99"]]),
    panel(12; "Empty fetch ratio / fetch errors"; 0; 48;
      [["sum(rate(fanin_perf_empty_fetches_total[5s])) / sum(rate(fanin_perf_fetches_total[5s]))", "empty fetch ratio"],
       ["sum(rate(fanin_perf_fetch_errors_total[5s]))", "fetch errors/s"]])
  ]' fan-in-perf.json > fan-in-perf.json.new && mv fan-in-perf.json.new fan-in-perf.json
git diff --stat fan-in-perf.json
jq '.panels | length' fan-in-perf.json
```
Expected: diff only adds lines (no deletions of existing panels; if jq reformatting changes existing lines, check `jq --indent 2 . <(git show HEAD:deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json) | diff - <(git show HEAD:…/fan-in-perf.json)` is empty and, if not, restore the original and append the panels by hand); panel count 12.

- [ ] **Step 3: Fix drift in the A spec**

Edit `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md`:
- CLI table: delete the `--domain` row; change the `--isolation` row to `read-committed\|repeatable-read\|serializable`.
- Sub-commands: add `- migrate — create the fan-in tables (idempotent) via FanInNotificationsAdmin.Migrate.`; add a `--seed` row (`unset = random, printed at start`) to the `produce` table.
- Section "Stats: non-blocking by construction": replace the `MutablePrimArray`/`fetchAddIntArray`/`atomicReadIntArray`/padding/log2-bucket bullets with: each writer owns an `IORef Snapshot` (per-kind `Map` counters + sparse `IntMap` latency histogram, buckets `[2^b, 2^(b+1))` ns, 1 ns .. ~18 min) it alone updates with `atomicModifyIORef'`; the ticker reads with an identity `atomicModifyIORef'` (memory barrier) and sums. Keep the "only the ticker touches prometheus-client and stdout" and "`/metrics` serves ticker-published state" bullets.
- Add one line under "Ticker": p50/p99 are cumulative over the run including warmup.
- Add at the end of the "Goal" section: `Experiment B: see 2026-10-12-fan-in-perf-consume-design.md.`

- [ ] **Step 4: Full verification**

Run, each with the standard filter:
```
make c | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
make format
make lint-all | tail -20
git status --short
```
Expected: no warnings, all tests green, formatter makes no further changes (commit any it makes), lint clean, only intended files changed.

- [ ] **Step 5: Manual acceptance (needs the user's consent and the running DB)**

Ask the user before running. With the connection string they provide (never echo it):
```
fan-in-perf --db "$DB" reset
fan-in-perf --db "$DB" consume --targets user:2000,team:1,epoch:200 --duration 30
```
Expected: startup prints coverage (user 100%, team 100%, epoch 100% for 2000 clients × 3 epochs over 200 streams), `loading start positions`, a live status line with `cons/s`, `lag(cum.)`, `backlog`; after 30 s `writers stopped, draining`; the summary shows `delivered`, `expected`, `missing`, `duplicates`, `drain: complete|timed out`. Also: `curl -s localhost:9400/metrics | grep -E '^# TYPE (fanin_perf_consumed_total|wire_hasql_pool_in_use)'` prints exactly one line per name; Grafana "fan-in-perf" shows the 5 new panels. Report `missing`/`duplicates` and the rates to the user as the experiment-B result.

- [ ] **Step 6: Commit**

```bash
git add tools/fan-in-perf/README.md deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json docs/superpowers/specs/2026-10-08-fan-in-perf-design.md
git commit -m "fan-in-perf: consume docs, dashboard panels, spec A drift"
```
