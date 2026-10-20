# fan-in-perf (Experiment A) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build CLI `fan-in-perf` (sub-commands `reset`, `produce`) that measures the maximal rate of adding fan-in notifications through `Wire.FanInNotificationsStore`, shows current/max rates on the terminal and exposes them on `/metrics` for Grafana LGTM.

**Architecture:** New package `tools/fan-in-perf`. Closed-loop writer threads generate one-kind pushes from pre-generated in-memory stream keys and call the store's Polysemy interpreter per push. Writers record into private cache-line-aligned `MutablePrimArray` counters; one ticker thread aggregates every second and is the only thread touching prometheus-client and stdout. Store fixes (bytea group ids, isolation param, UUIDv7 gen) and a test-only admin effect land in `wire-subsystems`.

**Tech Stack:** GHC 9.10.3, Polysemy, hasql (+ hasql-resource-pool, hasql-transaction 1.2.2, hasql-th), prometheus-client 1.1.1, warp 3.4 / wai 3.2, optparse-applicative 0.18, primitive 0.9.1, random 1.2.1, vector 0.13, async, stm, hspec 2.11 + QuickCheck 2.15.

**Spec:** `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md`

## Global Constraints

- DB access only via `Wire.FanInNotificationsStore` / `Wire.FanInNotificationsAdmin` effects; the tool never runs SQL itself.
- Exactly one `Target` constructor kind per push.
- `--isolation` default `read-committed` (RFC intent); `serializable` selectable.
- Defaults: `--writers 16`, `--payload-bytes 512`, `--metrics-port 9400`, `--domain example.com`, `--warmup 5`, `--clients-per-user 1`, `--pool-size` = `--writers` (produce) / 1 (reset).
- `--targets` grammar `KIND:STREAMS[xK]`, kinds `user|clients|team|epoch|connections`, `1 <= K <= STREAMS <= 10_000_000`, no duplicate kinds.
- Writers never block on metrics/terminal; only the ticker touches prometheus-client and stdout.
- Connection string / password never printed; `GlobalOptions` has no `Show` instance.
- Metric prefix `fanin_perf_`, label `experiment="produce"`, `kind` label on counters.
- Status line redraws in place on a TTY (`\r` + `ESC[2K`); one line per tick otherwise.
- Build commands end with `| grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`.
- Do not run integration test suites. Do not run docker commands (agent has no docker access).
- Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.
- Prefer record dot syntax for simple field access.

## Review Focus

1. **Absurd `--targets` numbers** (`team:99999999999999999999`, `user:50000000`): must be rejected with a clear message, not overflow to negative or allocate GBs → parser test in Task 4.
2. **Non-`UsageError` exceptions inside a push** (e.g. a decoding exception from the interpreter): must count as an error and the writer must keep running, not die silently → `writerStep` test in Task 10.
3. **Output piped to a file / non-TTY**: no escape codes, one line per tick → `renderStatusLine False` test in Task 7.
4. **Run shorter than warmup or Ctrl-C before the first tick** (`dt = 0`, zero pushes): no NaN/Infinity, max rate 0, summary still printed → `tick` tests in Task 6.
5. **DB errors leaking connection details** (connection error text containing host/password): terminal shows a generic message → `describeUsageError` test in Task 10; invalid `--db` never echoed → generic message in `run` (Task 10).

---

## File Structure

| Path | Action | Responsibility |
|---|---|---|
| `libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql` | modify | `group_id` → `bytea` |
| `libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs` | modify | isolation param, bytea epoch statements, `genNotificationId` |
| `libs/wire-subsystems/src/Wire/FanInNotificationsAdmin.hs` | create | test/perf-only effect `TruncateAll`, `Ping` |
| `libs/wire-subsystems/src/Wire/FanInNotificationsAdmin/Postgres.hs` | create | Postgres interpreter for admin effect |
| `libs/wire-subsystems/test/unit/Wire/FanInNotificationsStore/PostgresSpec.hs` | create | `genNotificationId` tests |
| `libs/wire-subsystems/wire-subsystems.cabal` | modify | new modules |
| `libs/extended/src/Hasql/Pool/Extended.hs` | modify | factor out `initPostgresPoolFromConnString` |
| `tools/fan-in-perf/fan-in-perf.cabal` | create | package |
| `tools/fan-in-perf/default.nix` | generate | via `make regen-local-nix-derivations` |
| `tools/fan-in-perf/README.md` | create | usage |
| `tools/fan-in-perf/app/Main.hs` | create | entry point |
| `tools/fan-in-perf/src/FanInPerf/Targets.hs` | create | `--targets` parser, stream pools, push generation |
| `tools/fan-in-perf/src/FanInPerf/Stats.hs` | create | per-writer counters, snapshots, tick math |
| `tools/fan-in-perf/src/FanInPerf/Terminal.hs` | create | formatting + redraw |
| `tools/fan-in-perf/src/FanInPerf/Metrics.hs` | create | prometheus metrics + `/metrics` server |
| `tools/fan-in-perf/src/FanInPerf/Options.hs` | create | optparse-applicative |
| `tools/fan-in-perf/src/FanInPerf/Store.hs` | create | `Env`, `runStore`, error description |
| `tools/fan-in-perf/src/FanInPerf/Produce.hs` | create | experiment A loop |
| `tools/fan-in-perf/src/FanInPerf/Run.hs` | create | top-level dispatch |
| `tools/fan-in-perf/test/Main.hs` + `test/FanInPerf/*Spec.hs` | create | unit tests |
| `cabal.project`, `nix/local-haskell-packages.nix`, `nix/wire-server.nix` | modify | register package |
| `deploy/dockerephemeral/docker-compose.yaml` | modify | `extra_hosts`, dashboard mount |
| `deploy/dockerephemeral/docker/otel-collector-config.yaml` | modify | scrape job |
| `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json` | create | dashboard |

Note vs spec module table: `FanInPerf.Store` is split out of `FanInPerf.Run` to avoid an import cycle between `Run` and `Produce`.

---

### Task 1: Store fixes in `wire-subsystems`

**Files:**
- Modify: `libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql`
- Modify: `libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs`
- Create: `libs/wire-subsystems/test/unit/Wire/FanInNotificationsStore/PostgresSpec.hs`
- Modify: `libs/wire-subsystems/wire-subsystems.cabal` (test-suite `other-modules`)

**Interfaces:**
- Produces: `interpretFanInNotificationsStoreToPostgres :: (PGConstraints r, Member (Input (Local ())) r) => TxSessions.IsolationLevel -> InterpreterFor FanInNotificationsStore r`; `genNotificationId :: IO (Id a)` (unchanged type).

- [ ] **Step 1: Write the failing/pinning test for `genNotificationId`**

Create `libs/wire-subsystems/test/unit/Wire/FanInNotificationsStore/PostgresSpec.hs`:

```haskell
module Wire.FanInNotificationsStore.PostgresSpec (spec) where

import Data.Bits (shiftR, (.&.))
import Data.Id
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.FanInNotificationsStore.Postgres (genNotificationId)

spec :: Spec
spec = describe "genNotificationId" $ do
  it "generates version 7 UUIDs" $ do
    ids <- replicateM 100 (genNotificationId @())
    forM_ ids $ \i -> do
      let (_, w2, _, _) = UUID.toWords i.toUUID
      (w2 `shiftR` 12) .&. 0xF `shouldBe` 7

  it "generates strictly increasing ids within the process" $ do
    uuids <- replicateM 10000 ((.toUUID) <$> genNotificationId @())
    and (zipWith (<) uuids (drop 1 uuids)) `shouldBe` True
```

Add `Wire.FanInNotificationsStore.PostgresSpec` to `other-modules` of `test-suite wire-subsystems-tests` in `libs/wire-subsystems/wire-subsystems.cabal` (keep alphabetical order).

- [ ] **Step 2: Run the test against the current implementation**

Run: `cabal test wire-subsystems-tests --test-options='-m genNotificationId' | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: PASS (it pins today's semantics before the refactor). If "strictly increasing" fails, STOP and report: the RFC relies on time-ordered ids, this would be a finding.

- [ ] **Step 3: Change migration to `bytea`**

In `libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql` replace `group_id text NOT NULL` with `group_id bytea NOT NULL` in exactly these four tables: `epoch_notifications`, `epoch_history`, `last_epoch_notifications`, `epoch_notification_acks`.

Verify: `grep -n "group_id" libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql` → four lines, all `bytea`.

- [ ] **Step 4: Update the Postgres interpreter**

In `libs/wire-subsystems/src/Wire/FanInNotificationsStore/Postgres.hs`:

1. Interpreter takes the isolation level:

```haskell
interpretFanInNotificationsStoreToPostgres ::
  (FanInNotificationsStorePostgresEffectConstraints r, Member (Input (Local ())) r) =>
  TxSessions.IsolationLevel ->
  InterpreterFor FanInNotificationsStore r
interpretFanInNotificationsStoreToPostgres isolationLevel = interpret $ \case
  PushViaFanIn push -> pushViaFanInImpl isolationLevel push

pushViaFanInImpl ::
  (FanInNotificationsStorePostgresEffectConstraints r, Member (Input (Local ())) r) =>
  TxSessions.IsolationLevel ->
  FanInPush ->
  Sem r ()
pushViaFanInImpl isolationLevel push = do
  notifId <- embed @IO genNotificationId
  loc <- inputQualifyLocal ()
  let payload = Aeson.toJSON push.json
  runTransactionWithRetry isolationLevel TxSessions.Write do
    -- body unchanged
```

2. Replace `genNotificationId`:

```haskell
-- | Time-ordered (UUIDv7) notification id.
genNotificationId :: IO (Id a)
genNotificationId = Id <$> UUIDv7.genUUID
```

Remove the now unused `Data.UUID qualified as UUID` import if nothing else uses it.

3. `pushEpochNotification` passes the raw bytes:

```haskell
pushEpochNotification gid epoch notifId payload origin = do
  let epoch' = fromIntegral epoch :: Int64
  Tx.statement
    (gid, epoch', notifId.toUUID, payload, fmap (.toUUID) origin)
    insertEpochNotificationStatement
  Tx.statement
    (gid, epoch', notifId.toUUID)
    upsertLastEpochNotificationStatement
  where
    insertEpochNotificationStatement :: Statement (ByteString, Int64, UUID, Value, Maybe UUID) ()
    insertEpochNotificationStatement =
      [resultlessStatement|
        insert into epoch_notifications (group_id, epoch, notification_id, payload, origin)
          values ($1 :: bytea, $2 :: bigint, $3 :: uuid, $4 :: jsonb, $5 :: uuid?)
      |]

    upsertLastEpochNotificationStatement :: Statement (ByteString, Int64, UUID) ()
    upsertLastEpochNotificationStatement =
      [resultlessStatement|
        insert into last_epoch_notifications (group_id, epoch, notification_id)
          values ($1 :: bytea, $2 :: bigint, $3 :: uuid)
          on conflict (group_id, epoch) do update
            set notification_id = greatest(last_epoch_notifications.notification_id, excluded.notification_id)
      |]
```

Remove the `Data.Text.Encoding qualified as TE` import if unused.

- [ ] **Step 5: Build + tests**

Run: `make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no errors, no warnings in changed modules, all tests PASS (including `genNotificationId`).

- [ ] **Step 6: Commit**

```bash
git add libs/wire-subsystems
git commit -m "FanInNotificationsStore: bytea group ids, isolation param, simpler UUIDv7

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

- [ ] **Step 7: Hand-off note for the user (do not run)**

Tell the user: schema changed → re-create the DB schema (`make postgres-reset`) and regenerate `postgres-schema.sql` (`make postgres-schema`); both need docker.

---

### Task 2: `FanInNotificationsAdmin` effect

**Files:**
- Create: `libs/wire-subsystems/src/Wire/FanInNotificationsAdmin.hs`
- Create: `libs/wire-subsystems/src/Wire/FanInNotificationsAdmin/Postgres.hs`
- Modify: `libs/wire-subsystems/wire-subsystems.cabal` (library `exposed-modules`)

**Interfaces:**
- Produces: `truncateAll :: Member FanInNotificationsAdmin r => Sem r ()`, `ping :: Member FanInNotificationsAdmin r => Sem r ()`, `interpretFanInNotificationsAdminToPostgres :: PGConstraints r => InterpreterFor FanInNotificationsAdmin r`.

- [ ] **Step 1: Effect**

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsAdmin where

import Imports
import Polysemy

-- | Administrative operations on the fan-in notification tables for tests and
-- performance tooling. Never wire this into services.
data FanInNotificationsAdmin m a where
  TruncateAll :: FanInNotificationsAdmin m ()
  Ping :: FanInNotificationsAdmin m ()

makeSem ''FanInNotificationsAdmin
```

- [ ] **Step 2: Postgres interpreter**

```haskell
module Wire.FanInNotificationsAdmin.Postgres where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Imports
import Polysemy
import Wire.FanInNotificationsAdmin
import Wire.Postgres

interpretFanInNotificationsAdminToPostgres :: (PGConstraints r) => InterpreterFor FanInNotificationsAdmin r
interpretFanInNotificationsAdminToPostgres = interpret $ \case
  TruncateAll -> runStatement () truncateAllStatement
  Ping -> runStatement () pingStatement

-- | Keep the table list in sync with
-- @postgres-migrations/20260729073800-fan-in-notifications.sql@.
truncateAllStatement :: Statement.Statement () ()
truncateAllStatement =
  Statement.unpreparable
    "TRUNCATE TABLE \
    \user_notifications, client_notifications, team_notifications, \
    \epoch_notifications, local_connection_notifications, remote_connection_notifications, \
    \epoch_history, \
    \last_user_notifications, last_client_notifications, last_team_notifications, \
    \last_epoch_notifications, last_local_connection_notifications, last_remote_connection_notifications, \
    \user_notification_acks, client_notification_acks, team_notification_acks, \
    \epoch_notification_acks, local_connection_acks, remote_connection_acks"
    Encoders.noParams
    Decoders.noResult

pingStatement :: Statement.Statement () ()
pingStatement = Statement.unpreparable "SELECT 1" Encoders.noParams Decoders.noResult
```

If `Statement.unpreparable` takes `Text` (as in `Wire.JobSubsystem.ArbiterAdapter.runRawSql`), the string literals work via `OverloadedStrings`; check the module's extensions. `Decoders.noResult` on `SELECT 1` is accepted by hasql (result rows ignored); if the build or a runtime check complains, use `Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4))` and `void`.

Add both modules to `exposed-modules` in `libs/wire-subsystems/wire-subsystems.cabal`.

- [ ] **Step 3: Build**

Run: `make c package=wire-subsystems | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no errors/warnings.

Cross-check the table list: `grep -o "CREATE TABLE [a-z_]*" libs/wire-subsystems/postgres-migrations/20260729073800-fan-in-notifications.sql | wc -l` → 19, and every name appears in `truncateAllStatement`.

- [ ] **Step 4: Commit**

```bash
git add libs/wire-subsystems
git commit -m "Add FanInNotificationsAdmin effect (truncate, ping) for perf tooling

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Pool from connection string

**Files:**
- Modify: `libs/extended/src/Hasql/Pool/Extended.hs:134-200`

**Interfaces:**
- Produces: `initPostgresPoolFromConnString :: PoolConfig -> PostgresqlConnectionString.ConnectionString -> Maybe Text -> IO Pool` (third arg: optional password).

- [ ] **Step 1: Refactor**

Split `initPostgresPool` so the existing function delegates; behaviour for existing callers is unchanged:

```haskell
initPostgresPool :: PoolConfig -> Map Text Text -> Maybe FilePathSecrets -> IO Pool
initPostgresPool config pgConfig mFpSecrets = do
  mPw <- for mFpSecrets initCredentials
  connStr <- runConnStrParser $ PostgresqlConnectionString.fromKeyValueParams pgConfig
  initPostgresPoolFromConnString config connStr mPw

-- | Creates a pool from a parsed connection string and an optional password.
initPostgresPoolFromConnString :: PoolConfig -> PostgresqlConnectionString.ConnectionString -> Maybe Text -> IO Pool
initPostgresPoolFromConnString config connStr mPw = do
  let pgSettings =
        HasqlConnSettings.connectionString (PostgresqlConnectionString.toUrl connStr)
          <> foldMap HasqlConnSettings.password mPw
  metrics <- mkHasqlPoolMetrics
  rawPool <-
    HasqlPool.acquireWith
      (instrumentedConnectionGetter metrics (Hasql.Connection.acquire pgSettings))
      ( config.size,
        realToFrac config.idlenessTimeout.duration,
        unusedSettings
      )
  let pool = Pool {rawPool, metrics, poolAcquisitionTimeout = config.acquisitionTimeout}
  startHasqlPoolStatsReporter pool
  pure pool
  where
    -- move the existing where-bindings (instrumentedConnectionGetter,
    -- mkHasqlPoolMetrics, unusedSettings) here verbatim
```

If `HasqlConnSettings.password` does not take `Text`, use its actual argument type in the signature (check with LSP hover); `initCredentials` is polymorphic in its result.

- [ ] **Step 2: Build all dependants**

Run: `make c | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: no errors/warnings.

- [ ] **Step 3: Commit**

```bash
git add libs/extended
git commit -m "Hasql.Pool.Extended: add initPostgresPoolFromConnString

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: Package scaffold + `--targets` parser

**Files:**
- Create: `tools/fan-in-perf/fan-in-perf.cabal`, `tools/fan-in-perf/app/Main.hs`, `tools/fan-in-perf/test/Main.hs`, `tools/fan-in-perf/src/FanInPerf/Targets.hs`, `tools/fan-in-perf/test/FanInPerf/TargetsSpec.hs`, `tools/fan-in-perf/README.md`
- Modify: `cabal.project` (add `, tools/fan-in-perf` next to `tools/rabbitmq-consumer`), `nix/wire-server.nix:88` area (add `fan-in-perf = [ "fan-in-perf" ];`)
- Generate: `tools/fan-in-perf/default.nix`, `nix/local-haskell-packages.nix` via `make regen-local-nix-derivations`

**Interfaces:**
- Produces: `data TargetKind = KindUser | KindClients | KindTeam | KindEpoch | KindConnections` (`Eq, Ord, Show, Enum, Bounded`); `allKinds :: [TargetKind]`; `kindName :: TargetKind -> Text`; `data TargetEntry = TargetEntry {kind :: TargetKind, streams :: Int, perPush :: Int}` (`Eq, Show`); `maxStreams :: Int`; `parseTargetSpec :: Text -> Either String (NonEmpty TargetEntry)`; `renderTargetSpec :: NonEmpty TargetEntry -> Text`.

- [ ] **Step 1: Cabal file**

Copy the header fields (license, author, maintainer, copyright, `default-extensions` list) from `tools/rabbitmq-consumer/rabbitmq-consumer.cabal` into a `common common-all` stanza:

```cabal
cabal-version: 3.0
name:          fan-in-perf
version:       1.0.0
synopsis:      Benchmark the notification fan-in PostgreSQL store (WPB-26288)
category:      Tools
license:       AGPL-3.0-only
build-type:    Simple
-- author/maintainer/copyright: copy from rabbitmq-consumer.cabal

common common-all
  default-language:   GHC2021
  ghc-options:
    -O2 -Wall -Wpartial-fields -fwarn-tabs
    -optP-Wno-nonportable-include-path
  default-extensions:
    -- copy verbatim from rabbitmq-consumer.cabal (includes OverloadedRecordDot,
    -- DuplicateRecordFields, RecordWildCards, LambdaCase, NoImplicitPrelude)
    OverloadedStrings

library
  import:          common-all
  hs-source-dirs:  src
  exposed-modules:
    FanInPerf.Metrics
    FanInPerf.Options
    FanInPerf.Produce
    FanInPerf.Run
    FanInPerf.Stats
    FanInPerf.Store
    FanInPerf.Targets
    FanInPerf.Terminal
  build-depends:
    , aeson
    , async
    , base
    , bytestring
    , containers
    , extended
    , hasql
    , hasql-resource-pool
    , hasql-transaction
    , http-types
    , imports
    , optparse-applicative
    , polysemy
    , postgresql-connection-string
    , primitive
    , prometheus-client
    , random
    , stm
    , text
    , types-common
    , unliftio
    , uuid-types
    , vector
    , wai
    , warp
    , wire-api
    , wire-subsystems

executable fan-in-perf
  import:         common-all
  main-is:        Main.hs
  hs-source-dirs: app
  ghc-options:    -threaded -rtsopts "-with-rtsopts=-N -T"
  build-depends:
    , base
    , fan-in-perf
    , imports
    , optparse-applicative

test-suite fan-in-perf-tests
  import:             common-all
  type:               exitcode-stdio-1.0
  main-is:            Main.hs
  hs-source-dirs:     test
  ghc-options:        -threaded
  build-tool-depends: hspec-discover:hspec-discover
  other-modules:
    FanInPerf.MetricsSpec
    FanInPerf.OptionsSpec
    FanInPerf.ProduceSpec
    FanInPerf.StatsSpec
    FanInPerf.TargetsSpec
    FanInPerf.TerminalSpec
  build-depends:
    , aeson
    , base
    , bytestring
    , containers
    , fan-in-perf
    , hasql
    , hasql-resource-pool
    , hspec
    , imports
    , optparse-applicative
    , prometheus-client
    , QuickCheck
    , random
    , text
    , types-common
    , vector
    , wai
    , wire-api
    , wire-subsystems
```

Until later tasks exist, list only the modules that exist so far in `exposed-modules` / `other-modules` and add the others in their tasks (cabal fails on missing modules).

`test/Main.hs`:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

`app/Main.hs` (temporary until Task 10):

```haskell
module Main (main) where

import Imports

main :: IO ()
main = pure ()
```

- [ ] **Step 2: Register package**

- `cabal.project`: add `  , tools/fan-in-perf` in the packages list next to `tools/rabbitmq-consumer`.
- `nix/wire-server.nix`: add `fan-in-perf = [ "fan-in-perf" ];` next to `mlsstats = [ "mlsstats" ];`.
- Run: `make regen-local-nix-derivations 2>&1 | tail -5` → creates `tools/fan-in-perf/default.nix` and the entry in `nix/local-haskell-packages.nix`.

- [ ] **Step 3: Write the failing parser tests**

`tools/fan-in-perf/test/FanInPerf/TargetsSpec.hs`:

```haskell
module FanInPerf.TargetsSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import FanInPerf.Targets
import Imports
import Test.Hspec

spec :: Spec
spec = do
  describe "parseTargetSpec" $ do
    it "parses a single entry with default targets per push" $
      parseTargetSpec "team:10" `shouldBe` Right (TargetEntry KindTeam 10 1 :| [])

    it "parses several entries with targets per push" $
      parseTargetSpec "user:1000x20,team:10"
        `shouldBe` Right (TargetEntry KindUser 1000 20 :| [TargetEntry KindTeam 10 1])

    it "parses every kind" $
      fmap (fmap (.kind)) (parseTargetSpec "user:1,clients:1,team:1,epoch:1,connections:1")
        `shouldBe` Right (KindUser :| [KindClients, KindTeam, KindEpoch, KindConnections])

    it "accepts K == STREAMS" $
      parseTargetSpec "user:5x5" `shouldBe` Right (TargetEntry KindUser 5 5 :| [])

    forM_
      [ "",
        "team",
        "team:",
        "team:0",
        "team:10x0",
        "team:10x11",
        "team:-1",
        "team:1x2x3",
        "bogus:10",
        "team:10,team:5",
        "team:abc",
        "team:10,",
        "team:10 ",
        "team:99999999999999999999999",
        "team:10000001"
      ]
      $ \bad ->
        it ("rejects " <> show bad) $
          parseTargetSpec bad `shouldSatisfy` isLeft

  describe "renderTargetSpec" $
    it "round-trips" $
      let spec' = TargetEntry KindUser 1000 20 :| [TargetEntry KindTeam 10 1]
       in parseTargetSpec (renderTargetSpec spec') `shouldBe` Right spec'

  describe "kindName" $
    it "is unique per kind" $
      length (nubOrd (map kindName allKinds)) `shouldBe` length allKinds
```

(If `nubOrd` is not exported by `Imports`, import `Data.Containers.ListUtils (nubOrd)`. `T` import is used in Task 5's additions; drop it here if unused to keep `-Wall` clean.)

- [ ] **Step 4: Run tests to see them fail**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile FAIL (`FanInPerf.Targets` missing).

- [ ] **Step 5: Implement parser part of `FanInPerf.Targets`**

```haskell
module FanInPerf.Targets
  ( TargetKind (..),
    allKinds,
    kindName,
    TargetEntry (..),
    maxStreams,
    parseTargetSpec,
    renderTargetSpec,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Imports

-- | One kind per 'Wire.FanInNotificationsStore.Target' constructor.
data TargetKind = KindUser | KindClients | KindTeam | KindEpoch | KindConnections
  deriving (Eq, Ord, Show, Enum, Bounded)

allKinds :: [TargetKind]
allKinds = [minBound .. maxBound]

kindName :: TargetKind -> Text
kindName = \case
  KindUser -> "user"
  KindClients -> "clients"
  KindTeam -> "team"
  KindEpoch -> "epoch"
  KindConnections -> "connections"

-- | @KIND:STREAMS[xK]@: @streams@ distinct stream keys, @perPush@ targets per push.
data TargetEntry = TargetEntry
  { kind :: TargetKind,
    streams :: Int,
    perPush :: Int
  }
  deriving (Eq, Show)

-- | Stream keys are kept in memory, so their number is bounded.
maxStreams :: Int
maxStreams = 10_000_000

parseTargetSpec :: Text -> Either String (NonEmpty TargetEntry)
parseTargetSpec spec = do
  entries <- traverse parseEntry (T.splitOn "," spec)
  let kinds = map (.kind) entries
  when (Set.size (Set.fromList kinds) /= length kinds) $
    Left "duplicate target kind"
  maybe (Left "empty target spec") Right (nonEmpty entries)

parseEntry :: Text -> Either String TargetEntry
parseEntry entry = case T.splitOn ":" entry of
  [k, counts] -> do
    kind <- parseKind k
    (streams, perPush) <- case T.splitOn "x" counts of
      [s] -> (,1) <$> parseCount s
      [s, p] -> (,) <$> parseCount s <*> parseCount p
      _ -> malformed
    when (perPush > streams) $
      Left ("targets per push exceed streams: " <> T.unpack entry)
    pure TargetEntry {..}
  _ -> malformed
  where
    malformed = Left ("malformed target entry (expected KIND:STREAMS[xK]): " <> T.unpack entry)

parseKind :: Text -> Either String TargetKind
parseKind t =
  maybe (Left ("unknown target kind: " <> T.unpack t)) Right $
    find ((== t) . kindName) allKinds

-- | Parsed as 'Integer' first so huge inputs cannot overflow 'Int'.
parseCount :: Text -> Either String Int
parseCount t = case T.decimal @Integer t of
  Right (n, rest)
    | T.null rest && n > 0 && n <= toInteger maxStreams -> Right (fromInteger n)
  _ -> Left ("expected a number between 1 and " <> show maxStreams <> ", got: " <> T.unpack t)

renderTargetSpec :: NonEmpty TargetEntry -> Text
renderTargetSpec =
  T.intercalate ","
    . map (\e -> kindName e.kind <> ":" <> T.pack (show e.streams) <> "x" <> T.pack (show e.perPush))
    . NE.toList
```

- [ ] **Step 6: Run tests**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all PASS, no warnings.

- [ ] **Step 7: Commit**

```bash
git add cabal.project nix tools/fan-in-perf
git commit -m "fan-in-perf: package scaffold and --targets parser

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: Stream pools + push generation

**Files:**
- Modify: `tools/fan-in-perf/src/FanInPerf/Targets.hs`
- Modify: `tools/fan-in-perf/test/FanInPerf/TargetsSpec.hs`

**Interfaces:**
- Consumes: Task 4 types.
- Produces:
  - `data StreamPool = UserPool (V.Vector UserId) | ClientsPool (V.Vector (UserId, NonEmpty ClientId)) | TeamPool (V.Vector TeamId) | EpochPool (V.Vector (GroupId, Epoch)) | ConnectionsPool (V.Vector UserId)`
  - `data Entry = Entry {spec :: TargetEntry, pool :: StreamPool}`
  - `mkEntries :: RandomGen g => Int -> NonEmpty TargetEntry -> g -> (V.Vector Entry, g)` (first arg: clients per user)
  - `sampleDistinct :: RandomGen g => Int -> Int -> g -> ([Int], g)` (`n`, `k`)
  - `targetAt :: Domain -> StreamPool -> Int -> Target`
  - `genTargets :: RandomGen g => Domain -> V.Vector Entry -> g -> ((TargetKind, NonEmpty Target), g)`
  - `targetKind :: Target -> TargetKind`, `targetKey :: Target -> Text`
  - `mkPayload :: Int -> Aeson.Object`, `mkPush :: Aeson.Object -> NonEmpty Target -> FanInPush`

- [ ] **Step 1: Write failing tests** (append to `TargetsSpec`; add imports `Data.Domain (Domain (..))`, `Data.List.NonEmpty qualified as NE`, `Data.Vector qualified as V`, `System.Random (mkStdGen)`, `Test.QuickCheck`, `Test.Hspec.QuickCheck (prop)`, `Wire.FanInNotificationsStore (Target (..))`, `Data.Aeson qualified as A`, `Data.Aeson.KeyMap qualified as KM`)

```haskell
  describe "sampleDistinct" $
    prop "returns k distinct indices in [0, n)" $ \(Positive n0) (Positive k0) seed ->
      let n = min 500 n0
          k = 1 + (k0 - 1) `mod` n
          (xs, _) = sampleDistinct n k (mkStdGen seed)
       in length xs == k
            && length (nubOrd xs) == k
            && all (\x -> x >= 0 && x < n) xs

  describe "genTargets" $ do
    let dom = Domain "example.com"
        specs =
          TargetEntry KindUser 50 5
            :| [ TargetEntry KindClients 20 3,
                 TargetEntry KindTeam 4 1,
                 TargetEntry KindEpoch 10 2,
                 TargetEntry KindConnections 30 7
               ]
        perPushOf k = maybe 0 (.perPush) (find ((== k) . (.kind)) specs)

    prop "pushes have one kind, K distinct targets, keys from the entry's pool" $ \seed ->
      let (entries, g0) = mkEntries 2 specs (mkStdGen seed)
          poolKeys k =
            case V.find ((== k) . (.spec.kind)) entries of
              Nothing -> []
              Just e -> [targetKey (targetAt dom e.pool i) | i <- [0 .. e.spec.streams - 1]]
          pushes = take 200 (unfoldr (Just . genTargets dom entries) g0)
          ok (kind, ts) =
            let keys = map targetKey (NE.toList ts)
             in all ((== kind) . targetKind) ts
                  && length ts == perPushOf kind
                  && length (nubOrd keys) == length keys
                  && all (`elem` poolKeys kind) keys
       in all ok pushes

    it "uses every entry eventually" $
      let (entries, g0) = mkEntries 1 specs (mkStdGen 42)
          kinds = map fst (take 500 (unfoldr (Just . genTargets dom entries) g0))
       in nubOrd kinds `shouldMatchList` allKinds

    it "gives clients targets the configured number of client ids" $
      let (entries, g0) = mkEntries 3 (TargetEntry KindClients 5 2 :| []) (mkStdGen 7)
          ((_, ts), _) = genTargets dom entries g0
       in [length cs | TargetUserClients (_, cs) <- NE.toList ts] `shouldBe` [3, 3]

    it "generates 32-byte binary group ids" $
      let (entries, g0) = mkEntries 1 (TargetEntry KindEpoch 5 1 :| []) (mkStdGen 9)
          ((_, ts), _) = genTargets dom entries g0
       in [BS.length gid.unGroupId | TargetEpoch (gid, _) <- NE.toList ts] `shouldBe` [32]

    it "qualifies connection targets with the local domain" $
      let (entries, g0) = mkEntries 1 (TargetEntry KindConnections 5 1 :| []) (mkStdGen 3)
          ((_, ts), _) = genTargets dom entries g0
       in [qDomain q | TargetConnections q <- NE.toList ts] `shouldBe` [dom]

  describe "mkPayload" $
    it "has roughly the requested size" $
      let size = LBS.length (A.encode (mkPayload 512))
       in size `shouldSatisfy` (\s -> s >= 512 && s < 600)
```

(Extra imports for these: `Data.ByteString qualified as BS`, `Data.ByteString.Lazy qualified as LBS`, `Data.Qualified (qDomain)`, `Wire.API.MLS.Group (GroupId (..))`.)

- [ ] **Step 2: Run tests to see them fail**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile FAIL (missing `mkEntries`, …).

- [ ] **Step 3: Implement** (append to `FanInPerf.Targets`, extend export list with the Produces names)

```haskell
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Domain
import Data.Id
import Data.IntSet qualified as IntSet
import Data.Qualified
import Data.UUID.Types qualified as UUID
import Data.Vector qualified as V
import System.Random
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.Push.V2 (Route (RouteAny))
import Wire.FanInNotificationsStore

-- | Pre-generated stream keys of one '--targets' entry.
data StreamPool
  = UserPool (V.Vector UserId)
  | ClientsPool (V.Vector (UserId, NonEmpty ClientId))
  | TeamPool (V.Vector TeamId)
  | EpochPool (V.Vector (GroupId, Epoch))
  | ConnectionsPool (V.Vector UserId)

data Entry = Entry
  { spec :: TargetEntry,
    pool :: StreamPool
  }

mkEntries :: (RandomGen g) => Int -> NonEmpty TargetEntry -> g -> (V.Vector Entry, g)
mkEntries clientsPerUser specs g0 =
  let step (acc, g) s =
        let (p, g') = mkPool clientsPerUser s g
         in (Entry s p : acc, g')
      (entries, g1) = foldl' step ([], g0) (NE.toList specs)
   in (V.fromList (reverse entries), g1)

mkPool :: (RandomGen g) => Int -> TargetEntry -> g -> (StreamPool, g)
mkPool clientsPerUser s g0 =
  let (g1, g2) = split g0
      gen :: (forall h. (RandomGen h) => h -> (a, h)) -> V.Vector a
      gen f = V.unfoldrExactN s.streams f g1
   in ( case s.kind of
          KindUser -> UserPool (gen genId)
          KindClients -> ClientsPool (gen genUserClients)
          KindTeam -> TeamPool (gen genId)
          KindEpoch -> EpochPool (gen genGroupEpoch)
          KindConnections -> ConnectionsPool (gen genId),
        g2
      )
  where
    -- Client ids 1..C per user: distinct, so one push never inserts the same
    -- (user, client, notification) row twice.
    clientIds = ClientId 1 :| map ClientId [2 .. fromIntegral clientsPerUser]
    genUserClients g = first (,clientIds) (genId g)

genId :: (RandomGen g) => g -> (Id a, g)
genId g0 =
  let (w1, g1) = uniform g0
      (w2, g2) = uniform g1
   in (Id (UUID.fromWords64 w1 w2), g2)

-- | MLS group ids are arbitrary bytes; 32 random bytes like real ones.
genGroupEpoch :: (RandomGen g) => g -> ((GroupId, Epoch), g)
genGroupEpoch g0 =
  let (bytes, g1) = genByteString 32 g0
      (epoch, g2) = uniformR (0, 1000) g1
   in ((GroupId bytes, Epoch epoch), g2)

-- | @k@ distinct indices from @[0, n)@ using Floyd's algorithm: exactly @k@
-- random draws regardless of how close @k@ is to @n@. Requires @1 <= k <= n@.
sampleDistinct :: (RandomGen g) => Int -> Int -> g -> ([Int], g)
sampleDistinct n k = go (n - k) IntSet.empty []
  where
    go j seen acc g
      | j >= n = (acc, g)
      | otherwise =
          let (t, g') = uniformR (0, j) g
              pick = if IntSet.member t seen then j else t
           in go (j + 1) (IntSet.insert pick seen) (pick : acc) g'

targetAt :: Domain -> StreamPool -> Int -> Target
targetAt dom pool i = case pool of
  UserPool v -> TargetUser (v V.! i)
  ClientsPool v -> TargetUserClients (v V.! i)
  TeamPool v -> TargetTeam (v V.! i)
  EpochPool v -> TargetEpoch (v V.! i)
  ConnectionsPool v -> TargetConnections (Qualified (v V.! i) dom)

-- | Picks an entry uniformly, then 'perPush' distinct stream keys of it. All
-- targets of one push therefore share one constructor.
genTargets :: (RandomGen g) => Domain -> V.Vector Entry -> g -> ((TargetKind, NonEmpty Target), g)
genTargets dom entries g0 =
  let (ei, g1) = uniformR (0, V.length entries - 1) g0
      e = entries V.! ei
      (idxs, g2) = sampleDistinct e.spec.streams e.spec.perPush g1
      -- perPush >= 1, so idxs is never empty; the fallback only satisfies the type
      targets = targetAt dom e.pool <$> fromMaybe (0 :| []) (nonEmpty idxs)
   in ((e.spec.kind, targets), g2)

targetKind :: Target -> TargetKind
targetKind = \case
  TargetUser _ -> KindUser
  TargetUserClients _ -> KindClients
  TargetTeam _ -> KindTeam
  TargetEpoch _ -> KindEpoch
  TargetConnections _ -> KindConnections

-- | Stream key as text, for tests and diagnostics.
targetKey :: Target -> Text
targetKey = \case
  TargetUser uid -> idToText uid
  TargetUserClients (uid, cids) -> idToText uid <> ":" <> T.intercalate "," (map clientToText (NE.toList cids))
  TargetTeam tid -> idToText tid
  TargetEpoch (gid, epoch) -> T.pack (show gid.unGroupId) <> "/" <> T.pack (show epoch.epochNumber)
  TargetConnections q -> idToText (qUnqualified q) <> "@" <> domainText (qDomain q)

-- | JSON payload of roughly @n@ bytes.
mkPayload :: Int -> A.Object
mkPayload n =
  KM.fromList
    [ ("type", A.String "fanin-perf"),
      ("pad", A.String (T.replicate n "x"))
    ]

mkPush :: A.Object -> NonEmpty Target -> FanInPush
mkPush payload targets =
  FanInPush
    { conn = Nothing,
      transient = False,
      route = RouteAny,
      nativePriority = Nothing,
      origin = Nothing,
      targets = NE.toList targets,
      json = payload,
      apsData = Nothing,
      isCellsEvent = False
    }
```

If the rank-2 `gen` helper fights type inference, inline `V.unfoldrExactN s.streams genId g1` per case. `split` is fine in random-1.2 (renamed `splitGen` only in 1.3).

- [ ] **Step 4: Run tests**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all PASS, no warnings.

- [ ] **Step 5: Commit**

```bash
git add tools/fan-in-perf
git commit -m "fan-in-perf: stream pools and one-kind push generation

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 6: Stats (per-writer counters, snapshots, tick math)

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Stats.hs`
- Create: `tools/fan-in-perf/test/FanInPerf/StatsSpec.hs`
- Modify: `tools/fan-in-perf/fan-in-perf.cabal` (add modules)

**Interfaces:**
- Consumes: `TargetKind`, `allKinds` (Task 4).
- Produces:
  - `WriterStats` (abstract), `newWriterStats :: IO WriterStats`, `recordSuccess :: WriterStats -> TargetKind -> Int -> Word64 -> IO ()` (kind, targets, latency ns), `recordError :: WriterStats -> TargetKind -> IO ()`
  - `Snapshot` (abstract, `Eq, Show`), `emptySnapshot`, `readSnapshot :: WriterStats -> IO Snapshot`, `sumSnapshots :: [Snapshot] -> Snapshot`, `diffSnapshot :: Snapshot -> Snapshot -> Snapshot` (new, old)
  - `pushesOf, targetsOf, errorsOf :: TargetKind -> Snapshot -> Int`, `totalPushes, totalTargets, totalErrors :: Snapshot -> Int`, `latencyBuckets :: Snapshot -> VU.Vector Int`
  - `numBuckets :: Int`, `bucketIndex :: Word64 -> Int`, `bucketUpperBoundSeconds :: Int -> Double`, `quantileSeconds :: Double -> VU.Vector Int -> Maybe Double`
  - `data TickState = TickState {startTime :: Double, lastTime :: Double, previous :: Snapshot, maxRateSoFar :: Double}`, `initialTickState :: Double -> TickState`
  - `data TickReport = TickReport {elapsed, pushRate, maxPushRate, targetRate, errorRate, errorRatio :: Double, p50, p99 :: Maybe Double, delta, total :: Snapshot}` (`Eq, Show`)
  - `tick :: Double -> Double -> Snapshot -> TickState -> (TickReport, TickState)` (warmup secs, now secs, summed total)

- [ ] **Step 1: Write failing tests** — `test/FanInPerf/StatsSpec.hs`

```haskell
module FanInPerf.StatsSpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Stats
import FanInPerf.Targets (TargetKind (..))
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "bucketIndex" $ do
    it "maps edges to log2 buckets" $
      map bucketIndex [0, 1, 2, 3, 1023, 1024, maxBound]
        `shouldBe` [0, 0, 1, 1, 9, 10, numBuckets - 1]
    prop "latency is below its bucket's upper bound" $ \(Positive (ns :: Word64)) ->
      ns < 2 ^ (numBuckets - 1 :: Int) ==>
        fromIntegral ns / 1e9 < bucketUpperBoundSeconds (bucketIndex ns)

  describe "quantileSeconds" $ do
    it "is Nothing without samples" $
      quantileSeconds 0.5 (VU.replicate numBuckets 0) `shouldBe` Nothing
    it "returns the upper bound of the bucket holding the quantile" $
      let buckets = VU.generate numBuckets (\b -> if b == 3 then 90 else if b == 10 then 10 else 0)
       in (quantileSeconds 0.5 buckets, quantileSeconds 0.99 buckets)
            `shouldBe` (Just (bucketUpperBoundSeconds 3), Just (bucketUpperBoundSeconds 10))

  describe "WriterStats" $ do
    it "records successes and errors per kind" $ do
      ws <- newWriterStats
      replicateM_ 3 (recordSuccess ws KindTeam 2 1000)
      recordError ws KindUser
      s <- readSnapshot ws
      (pushesOf KindTeam s, targetsOf KindTeam s, errorsOf KindUser s, totalPushes s, totalErrors s)
        `shouldBe` (3, 6, 1, 3, 1)
      latencyBuckets s VU.! bucketIndex 1000 `shouldBe` 3

    it "sums writers that ran concurrently" $ do
      wss <- replicateM 8 newWriterStats
      forConcurrently_ wss $ \ws -> replicateM_ 10000 (recordSuccess ws KindUser 1 500)
      s <- sumSnapshots <$> traverse readSnapshot wss
      totalPushes s `shouldBe` 80000

  describe "tick" $ do
    let mkTotal pushes errs = do
          ws <- newWriterStats
          replicateM_ pushes (recordSuccess ws KindUser 2 1000)
          replicateM_ errs (recordError ws KindUser)
          readSnapshot ws

    it "computes rates from the delta and ignores max during warmup" $ do
      t1 <- mkTotal 100 0
      let (r1, st1) = tick 5 1 t1 (initialTickState 0)
      (r1.pushRate, r1.targetRate, r1.maxPushRate) `shouldBe` (100, 200, 0)
      t2 <- mkTotal 400 0
      let (r2, _) = tick 5 6 t2 st1
      (r2.pushRate, r2.maxPushRate) `shouldBe` (60, 60)

    it "never produces NaN or Infinity for dt = 0 and no traffic" $ do
      let (r, _) = tick 5 0 emptySnapshot (initialTickState 0)
      [r.pushRate, r.targetRate, r.errorRate, r.errorRatio, r.maxPushRate] `shouldBe` [0, 0, 0, 0, 0]
      (r.p50, r.p99) `shouldBe` (Nothing, Nothing)

    it "computes the error ratio over the last tick" $ do
      t <- mkTotal 3 1
      let (r, _) = tick 0 1 t (initialTickState 0)
      (r.errorRate, r.errorRatio) `shouldBe` (1, 0.25)
```

- [ ] **Step 2: Run tests to see them fail**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile FAIL (`FanInPerf.Stats` missing).

- [ ] **Step 3: Implement `FanInPerf.Stats`**

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Lock-free statistics. Every writer thread owns one 'WriterStats' and is
-- its only writer; the ticker thread reads all of them. Counters live in a
-- pinned, 64-byte aligned 'MutablePrimArray' padded to whole cache lines, so
-- writers never share a cache line and recording never allocates.
module FanInPerf.Stats
  ( WriterStats,
    newWriterStats,
    recordSuccess,
    recordError,
    Snapshot,
    emptySnapshot,
    readSnapshot,
    sumSnapshots,
    diffSnapshot,
    pushesOf,
    targetsOf,
    errorsOf,
    totalPushes,
    totalTargets,
    totalErrors,
    latencyBuckets,
    numBuckets,
    bucketIndex,
    bucketUpperBoundSeconds,
    quantileSeconds,
    TickState (..),
    initialTickState,
    TickReport (..),
    tick,
  )
where

import Data.Bits (countLeadingZeros)
import Data.Primitive.ByteArray (MutableByteArray (..), newAlignedPinnedByteArray)
import Data.Primitive.PrimArray (MutablePrimArray (..), setPrimArray)
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Targets (TargetKind, allKinds)
import GHC.Exts (Int (I#), atomicReadIntArray#, fetchAddIntArray#)
import GHC.IO (IO (IO))
import Imports

numKinds :: Int
numKinds = length allKinds

-- | Bucket @b@ holds latencies in @[2^b, 2^(b+1))@ ns: 1 ns .. ~18 min.
numBuckets :: Int
numBuckets = 40

slotPushes, slotTargets, slotErrors :: TargetKind -> Int
slotPushes k = fromEnum k
slotTargets k = numKinds + fromEnum k
slotErrors k = 2 * numKinds + fromEnum k

slotBucket :: Int -> Int
slotBucket b = 3 * numKinds + b

numSlots :: Int
numSlots = 3 * numKinds + numBuckets

-- | Whole cache lines (8 Ints each) plus one spare line.
allocatedSlots :: Int
allocatedSlots = (numSlots `div` 8 + 2) * 8

newtype WriterStats = WriterStats (MutablePrimArray RealWorld Int)

newWriterStats :: IO WriterStats
newWriterStats = do
  MutableByteArray mba <- newAlignedPinnedByteArray (allocatedSlots * 8) 64
  let arr = MutablePrimArray mba
  setPrimArray arr 0 allocatedSlots 0
  pure (WriterStats arr)

-- primitive-0.9 only offers atomics on PrimVar, so use the primops directly.
fetchAddSlot :: MutablePrimArray RealWorld Int -> Int -> Int -> IO ()
fetchAddSlot (MutablePrimArray mba) (I# i) (I# n) =
  IO $ \s -> case fetchAddIntArray# mba i n s of
    (# s', _ #) -> (# s', () #)

atomicReadSlot :: MutablePrimArray RealWorld Int -> Int -> IO Int
atomicReadSlot (MutablePrimArray mba) (I# i) =
  IO $ \s -> case atomicReadIntArray# mba i s of
    (# s', r #) -> (# s', I# r #)

recordSuccess :: WriterStats -> TargetKind -> Int -> Word64 -> IO ()
recordSuccess (WriterStats a) k targets latencyNs = do
  fetchAddSlot a (slotPushes k) 1
  fetchAddSlot a (slotTargets k) targets
  fetchAddSlot a (slotBucket (bucketIndex latencyNs)) 1

recordError :: WriterStats -> TargetKind -> IO ()
recordError (WriterStats a) k = fetchAddSlot a (slotErrors k) 1

-- | Cells are read one by one; a snapshot may be off by one push between
-- cells, which is irrelevant at one-second granularity.
newtype Snapshot = Snapshot (VU.Vector Int)
  deriving (Eq, Show)

emptySnapshot :: Snapshot
emptySnapshot = Snapshot (VU.replicate numSlots 0)

readSnapshot :: WriterStats -> IO Snapshot
readSnapshot (WriterStats a) = Snapshot <$> VU.generateM numSlots (atomicReadSlot a)

sumSnapshots :: [Snapshot] -> Snapshot
sumSnapshots = foldl' (\(Snapshot x) (Snapshot y) -> Snapshot (VU.zipWith (+) x y)) emptySnapshot

diffSnapshot :: Snapshot -> Snapshot -> Snapshot
diffSnapshot (Snapshot new) (Snapshot old) = Snapshot (VU.zipWith (-) new old)

slot :: Int -> Snapshot -> Int
slot i (Snapshot v) = v VU.! i

pushesOf, targetsOf, errorsOf :: TargetKind -> Snapshot -> Int
pushesOf = slot . slotPushes
targetsOf = slot . slotTargets
errorsOf = slot . slotErrors

totalPushes, totalTargets, totalErrors :: Snapshot -> Int
totalPushes s = sum [pushesOf k s | k <- allKinds]
totalTargets s = sum [targetsOf k s | k <- allKinds]
totalErrors s = sum [errorsOf k s | k <- allKinds]

latencyBuckets :: Snapshot -> VU.Vector Int
latencyBuckets (Snapshot v) = VU.slice (slotBucket 0) numBuckets v

bucketIndex :: Word64 -> Int
bucketIndex ns = min (numBuckets - 1) (63 - countLeadingZeros (max 1 ns))

bucketUpperBoundSeconds :: Int -> Double
bucketUpperBoundSeconds b = 2 ^^ (b + 1) / 1e9

-- | Upper bound of the bucket containing quantile @q@.
quantileSeconds :: Double -> VU.Vector Int -> Maybe Double
quantileSeconds q buckets
  | total <= 0 = Nothing
  | otherwise = bucketUpperBoundSeconds <$> VU.findIndex (>= threshold) cumulative
  where
    cumulative = VU.postscanl' (+) 0 buckets
    total = VU.sum buckets
    threshold = max 1 (ceiling (q * fromIntegral total))

data TickState = TickState
  { startTime :: Double,
    lastTime :: Double,
    previous :: Snapshot,
    maxRateSoFar :: Double
  }

initialTickState :: Double -> TickState
initialTickState now = TickState now now emptySnapshot 0

data TickReport = TickReport
  { elapsed :: Double,
    pushRate :: Double,
    maxPushRate :: Double,
    targetRate :: Double,
    errorRate :: Double,
    errorRatio :: Double,
    p50 :: Maybe Double,
    p99 :: Maybe Double,
    delta :: Snapshot,
    total :: Snapshot
  }
  deriving (Eq, Show)

tick :: Double -> Double -> Snapshot -> TickState -> (TickReport, TickState)
tick warmup now total st =
  let dt = now - st.lastTime
      delta = diffSnapshot total st.previous
      rate :: Int -> Double
      rate n = if dt > 0 then fromIntegral n / dt else 0
      pushes = totalPushes delta
      errors = totalErrors delta
      pushRate = rate pushes
      elapsed = now - st.startTime
      maxPushRate = if elapsed > warmup then max st.maxRateSoFar pushRate else st.maxRateSoFar
      errorRatio = if pushes + errors > 0 then fromIntegral errors / fromIntegral (pushes + errors) else 0
      latency = latencyBuckets total
      report =
        TickReport
          { elapsed,
            pushRate,
            maxPushRate,
            targetRate = rate (totalTargets delta),
            errorRate = rate errors,
            errorRatio,
            p50 = quantileSeconds 0.5 latency,
            p99 = quantileSeconds 0.99 latency,
            delta,
            total
          }
   in (report, st {lastTime = now, previous = total, maxRateSoFar = maxPushRate})
```

Add `FanInPerf.Stats` to `exposed-modules` and `FanInPerf.StatsSpec` to test `other-modules`. Test needs `async` in test `build-depends`.

- [ ] **Step 4: Run tests**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all PASS, no warnings.

- [ ] **Step 5: Commit**

```bash
git add tools/fan-in-perf
git commit -m "fan-in-perf: lock-free per-writer stats and tick math

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 7: Terminal output

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Terminal.hs`, `tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs`
- Modify: cabal module lists

**Interfaces:**
- Consumes: `TickReport`, `totalPushes`, `totalTargets`, `totalErrors` (Task 6).
- Produces: `groupThousands :: Int -> Text`, `formatLatency :: Maybe Double -> Text`, `formatStatus :: TickReport -> Text`, `formatSummary :: TickReport -> [Text]`, `renderStatusLine :: Bool -> Text -> Text`, `renderLogLine :: Bool -> Text -> Text`, `newtype Console = Console {isTty :: Bool}`, `newConsole :: IO Console`, `drawStatus :: Console -> Text -> IO ()`, `printLine :: Console -> Text -> IO ()`.

- [ ] **Step 1: Failing tests**

```haskell
module FanInPerf.TerminalSpec (spec) where

import Data.Text qualified as T
import FanInPerf.Stats
import FanInPerf.Terminal
import Imports
import Test.Hspec

report :: TickReport
report =
  TickReport
    { elapsed = 42.2,
      pushRate = 8312.4,
      maxPushRate = 9105,
      targetRate = 41560,
      errorRate = 3,
      errorRatio = 0.0004,
      p50 = Just 0.0031,
      p99 = Just 0.0124,
      delta = emptySnapshot,
      total = emptySnapshot
    }

spec :: Spec
spec = do
  describe "groupThousands" $
    it "groups digits by three" $
      map groupThousands [0, 999, 1000, 1234567, -9105]
        `shouldBe` ["0", "999", "1 000", "1 234 567", "-9 105"]

  describe "formatLatency" $
    it "formats ms, seconds and missing values" $
      map formatLatency [Nothing, Just 0.0031, Just 2.5]
        `shouldBe` ["-", "3.1ms", "2.50s"]

  describe "formatStatus" $
    it "renders the status line" $
      formatStatus report
        `shouldBe` "t=42s push/s cur=8 312 max=9 105 | err/s cur=3 (0.04%) | targets/s cur=41 560 | p50=3.1ms p99=12.4ms"

  describe "renderStatusLine" $ do
    it "redraws in place on a TTY" $
      renderStatusLine True "x" `shouldBe` "\r\ESC[2Kx"
    it "prints plain lines without escape codes otherwise" $ do
      renderStatusLine False "x" `shouldBe` "x\n"
      T.any (== '\ESC') (renderStatusLine False (formatStatus report)) `shouldBe` False

  describe "renderLogLine" $ do
    it "clears the status line first on a TTY" $
      renderLogLine True "err" `shouldBe` "\r\ESC[2Kerr\n"
    it "is a plain line otherwise" $
      renderLogLine False "err" `shouldBe` "err\n"

  describe "formatSummary" $
    it "survives an empty run" $
      formatSummary report {elapsed = 0, p50 = Nothing, p99 = Nothing}
        `shouldSatisfy` all (not . T.isInfixOf "NaN")
```

- [ ] **Step 2: Run tests to see them fail**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile FAIL.

- [ ] **Step 3: Implement**

```haskell
module FanInPerf.Terminal
  ( groupThousands,
    formatLatency,
    formatStatus,
    formatSummary,
    renderStatusLine,
    renderLogLine,
    Console (..),
    newConsole,
    drawStatus,
    printLine,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import FanInPerf.Stats
import Imports
import Text.Printf (printf)

groupThousands :: Int -> Text
groupThousands n
  | n < 0 = "-" <> groupThousands (negate n)
  | otherwise = T.intercalate " " . reverse . map T.reverse . T.chunksOf 3 . T.reverse . T.pack $ show n

formatLatency :: Maybe Double -> Text
formatLatency = \case
  Nothing -> "-"
  Just s
    | s < 1 -> T.pack (printf "%.1fms" (s * 1000))
    | otherwise -> T.pack (printf "%.2fs" s)

percent :: Double -> Text
percent x = T.pack (printf "%.2f%%" (x * 100))

rateText :: Double -> Text
rateText = groupThousands . round

formatStatus :: TickReport -> Text
formatStatus r =
  T.intercalate
    " | "
    [ "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> rateText r.maxPushRate,
      "err/s cur=" <> rateText r.errorRate <> " (" <> percent r.errorRatio <> ")",
      "targets/s cur=" <> rateText r.targetRate,
      "p50=" <> formatLatency r.p50 <> " p99=" <> formatLatency r.p99
    ]

formatSummary :: TickReport -> [Text]
formatSummary r =
  let pushes = totalPushes r.total
      errors = totalErrors r.total
      ratio = if pushes + errors > 0 then fromIntegral errors / fromIntegral (pushes + errors) else 0
      avgRate = if r.elapsed > 0 then fromIntegral pushes / r.elapsed else 0
   in [ "summary: duration=" <> T.pack (printf "%.1fs" r.elapsed),
        "  pushes=" <> groupThousands pushes <> " targets=" <> groupThousands (totalTargets r.total) <> " errors=" <> groupThousands errors <> " (" <> percent ratio <> ")",
        "  push/s avg=" <> rateText avgRate <> " max=" <> rateText r.maxPushRate,
        "  latency p50=" <> formatLatency r.p50 <> " p99=" <> formatLatency r.p99
      ]

clearLine :: Text
clearLine = "\r\ESC[2K"

renderStatusLine :: Bool -> Text -> Text
renderStatusLine tty line = if tty then clearLine <> line else line <> "\n"

renderLogLine :: Bool -> Text -> Text
renderLogLine tty line = (if tty then clearLine else "") <> line <> "\n"

newtype Console = Console {isTty :: Bool}

newConsole :: IO Console
newConsole = do
  tty <- hIsTerminalDevice stdout
  hSetBuffering stdout (BlockBuffering Nothing)
  pure (Console tty)

drawStatus :: Console -> Text -> IO ()
drawStatus c line = T.putStr (renderStatusLine c.isTty line) >> hFlush stdout

printLine :: Console -> Text -> IO ()
printLine c line = T.putStr (renderLogLine c.isTty line) >> hFlush stdout
```

Note: `printf "%.1fms" 3.1` — `0.0031 * 1000` may print `3.1`; if a rounding artefact shows `3.1` vs `3.0`, keep the test values as given (3.1 ms → `"3.1ms"`, 12.4 ms → `"12.4ms"`).

- [ ] **Step 4: Run tests**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add tools/fan-in-perf
git commit -m "fan-in-perf: terminal status line and summary

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 8: Prometheus metrics + `/metrics` server

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Metrics.hs`, `tools/fan-in-perf/test/FanInPerf/MetricsSpec.hs`
- Modify: cabal module lists

**Interfaces:**
- Consumes: `TickReport`, `pushesOf`, `targetsOf`, `errorsOf`, `latencyBuckets`, `numBuckets`, `bucketUpperBoundSeconds` (Task 6); `allKinds`, `kindName` (Task 4).
- Produces: `Metrics` (abstract), `newMetrics :: Text -> Int -> IO Metrics` (experiment, writers), `publish :: Metrics -> TickReport -> IO ()`, `latencySampleGroup :: Text -> VU.Vector Int -> P.SampleGroup`, `metricsApp :: Wai.Application`, `runMetricsServer :: Int -> IO ()`.

- [ ] **Step 1: Failing tests**

```haskell
module FanInPerf.MetricsSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Metrics
import FanInPerf.Stats
import FanInPerf.Targets (TargetKind (..))
import Imports
import Network.HTTP.Types (Status, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Prometheus qualified as P
import Test.Hspec

statusFor :: [Text] -> IO (Maybe Status)
statusFor path = do
  ref <- newIORef Nothing
  _ <-
    metricsApp
      Wai.defaultRequest {Wai.pathInfo = path}
      (\r -> writeIORef ref (Just (Wai.responseStatus r)) >> pure ResponseReceived)
  readIORef ref

spec :: Spec
spec = do
  describe "latencySampleGroup" $
    it "emits cumulative buckets, +Inf, _sum and _count" $ do
      let buckets = VU.generate numBuckets (\b -> if b == 0 then 2 else if b == 2 then 3 else 0)
          P.SampleGroup _ ty samples = latencySampleGroup "produce" buckets
          values name = [v | P.Sample n _ v <- samples, n == name]
          leValues = [(lookup "le" ls, v) | P.Sample n ls v <- samples, n == "fanin_perf_push_duration_seconds_bucket"]
      ty `shouldBe` P.HistogramType
      take 3 (map snd leValues) `shouldBe` ["2", "2", "5"]
      last leValues `shouldBe` (Just "+Inf", "5")
      length leValues `shouldBe` numBuckets + 1
      values "fanin_perf_push_duration_seconds_count" `shouldBe` ["5"]
      length (values "fanin_perf_push_duration_seconds_sum") `shouldBe` 1

  describe "publish" $
    it "exports counters and gauges with experiment and kind labels" $ do
      m <- newMetrics "spec" 4
      ws <- newWriterStats
      replicateM_ 3 (recordSuccess ws KindTeam 1 1000)
      total <- readSnapshot ws
      let (r, _) = tick 0 1 total (initialTickState 0)
      publish m r
      out <- LBS8.unpack <$> P.exportMetricsAsText
      out `shouldContain` "fanin_perf_pushes_total{experiment=\"spec\",kind=\"team\"} 3"
      out `shouldContain` "fanin_perf_push_rate_current{experiment=\"spec\"} 3"
      out `shouldContain` "fanin_perf_writers{experiment=\"spec\"} 4"
      out `shouldContain` "fanin_perf_push_duration_seconds_count{experiment=\"spec\"} 3"

  describe "metricsApp" $ do
    it "serves /metrics" $ statusFor ["metrics"] `shouldReturn` Just status200
    it "404s elsewhere" $ statusFor ["other"] `shouldReturn` Just status404
```

`P.SampleType` may lack `Eq`; if so, replace the `ty` assertion with a `case ty of P.HistogramType -> pure (); _ -> expectationFailure "not a histogram"`. Counter values render as e.g. `3.0`; `shouldContain` with the prefix `... 3` matches both `3` and `3.0`.

- [ ] **Step 2: Run tests to see them fail**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile FAIL.

- [ ] **Step 3: Implement**

```haskell
module FanInPerf.Metrics
  ( Metrics,
    newMetrics,
    publish,
    latencySampleGroup,
    metricsApp,
    runMetricsServer,
  )
where

import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Stats
import FanInPerf.Targets (allKinds, kindName)
import Imports
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Prometheus qualified as P

-- | Only the ticker thread calls 'publish'; writers never touch these.
data Metrics = Metrics
  { experiment :: Text,
    pushes :: P.Vector P.Label2 P.Counter,
    targets :: P.Vector P.Label2 P.Counter,
    errors :: P.Vector P.Label2 P.Counter,
    pushRateCurrent :: P.Vector P.Label1 P.Gauge,
    pushRateMax :: P.Vector P.Label1 P.Gauge,
    errorRateCurrent :: P.Vector P.Label1 P.Gauge,
    errorRatioCurrent :: P.Vector P.Label1 P.Gauge,
    latency :: IORef (VU.Vector Int)
  }

newMetrics :: Text -> Int -> IO Metrics
newMetrics experiment writers = do
  let counterVec name help = P.register $ P.vector ("experiment", "kind") $ P.counter (P.Info name help)
      gaugeVec name help = P.register $ P.vector "experiment" $ P.gauge (P.Info name help)
  pushes <- counterVec "fanin_perf_pushes_total" "Successful pushes"
  targets <- counterVec "fanin_perf_targets_total" "Targets written by successful pushes"
  errors <- counterVec "fanin_perf_errors_total" "Failed pushes"
  pushRateCurrent <- gaugeVec "fanin_perf_push_rate_current" "Pushes per second during the last tick"
  pushRateMax <- gaugeVec "fanin_perf_push_rate_max" "Maximal pushes per second after warmup"
  errorRateCurrent <- gaugeVec "fanin_perf_error_rate_current" "Errors per second during the last tick"
  errorRatioCurrent <- gaugeVec "fanin_perf_error_ratio_current" "errors / (pushes + errors) during the last tick"
  writersGauge <- gaugeVec "fanin_perf_writers" "Concurrent writer threads"
  P.withLabel writersGauge experiment (`P.setGauge` fromIntegral writers)
  latency <- newIORef (VU.replicate numBuckets 0)
  _ <- P.register (latencyMetric experiment latency)
  pure Metrics {..}

publish :: Metrics -> TickReport -> IO ()
publish m r = do
  for_ allKinds $ \k -> do
    let lbl = (m.experiment, kindName k)
    add m.pushes lbl (pushesOf k r.delta)
    add m.targets lbl (targetsOf k r.delta)
    add m.errors lbl (errorsOf k r.delta)
  set m.pushRateCurrent r.pushRate
  set m.pushRateMax r.maxPushRate
  set m.errorRateCurrent r.errorRate
  set m.errorRatioCurrent r.errorRatio
  writeIORef m.latency (latencyBuckets r.total)
  where
    add v lbl n = when (n > 0) $ P.withLabel v lbl (void . (`P.addCounter` fromIntegral n))
    set g x = P.withLabel g m.experiment (`P.setGauge` x)

-- | Histogram served from the ticker's latest bucket snapshot. Bucket bounds
-- are powers of two in nanoseconds; '_sum' is approximated by bucket midpoints.
latencyMetric :: Text -> IORef (VU.Vector Int) -> P.Metric ()
latencyMetric experiment ref =
  P.Metric $ pure ((), (: []) . latencySampleGroup experiment <$> readIORef ref)

latencySampleGroup :: Text -> VU.Vector Int -> P.SampleGroup
latencySampleGroup experiment buckets =
  P.SampleGroup info P.HistogramType (bucketSamples <> [sumSample, countSample])
  where
    name = "fanin_perf_push_duration_seconds"
    info = P.Info name "Push latency in seconds"
    lbl = ("experiment", experiment)
    count = VU.sum buckets
    cumulative = VU.toList (VU.postscanl' (+) 0 buckets)
    bucketSamples =
      [ P.Sample (name <> "_bucket") [lbl, ("le", T.pack (show (bucketUpperBoundSeconds b)))] (bshow c)
      | (b, c) <- zip [0 ..] cumulative
      ]
        <> [P.Sample (name <> "_bucket") [lbl, ("le", "+Inf")] (bshow count)]
    -- midpoint of [2^b, 2^(b+1)) is 0.75 * upper bound
    approxSum :: Double
    approxSum = sum [fromIntegral c * 0.75 * bucketUpperBoundSeconds b | (b, c) <- zip [0 ..] (VU.toList buckets)]
    sumSample = P.Sample (name <> "_sum") [lbl] (bshow approxSum)
    countSample = P.Sample (name <> "_count") [lbl] (bshow count)
    bshow :: (Show a) => a -> ByteString
    bshow = BS8.pack . show

metricsApp :: Wai.Application
metricsApp req respond = case Wai.pathInfo req of
  ["metrics"] -> do
    body <- P.exportMetricsAsText
    respond $ Wai.responseLBS status200 [(hContentType, "text/plain; version=0.0.4")] body
  _ -> respond $ Wai.responseLBS status404 [] "not found"

-- | Binds 0.0.0.0 so the dockerised OTel collector can scrape the host.
runMetricsServer :: Int -> IO ()
runMetricsServer port =
  Warp.runSettings (Warp.setHost "*4" (Warp.setPort port Warp.defaultSettings)) metricsApp
```

`writersGauge` is not stored in `Metrics` (set once); remove the unused-binding warning by keeping it local as shown (`RecordWildCards` ignores it).

- [ ] **Step 4: Run tests**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add tools/fan-in-perf
git commit -m "fan-in-perf: prometheus metrics and /metrics endpoint

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 9: CLI options

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Options.hs`, `tools/fan-in-perf/test/FanInPerf/OptionsSpec.hs`
- Modify: cabal module lists

**Interfaces:**
- Consumes: `TargetEntry`, `parseTargetSpec` (Task 4).
- Produces: `data Isolation = ReadCommitted | Serializable` (`Eq, Show`); `data GlobalOptions = GlobalOptions {db :: Text, poolSize :: Maybe Int, metricsPort :: Int, domain :: Domain, isolation :: Isolation}` (no `Show`); `data ProduceOptions = ProduceOptions {writers :: Int, targets :: NonEmpty TargetEntry, clientsPerUser :: Int, payloadBytes :: Int, duration :: Maybe Int, warmup :: Int}` (`Eq, Show`); `data Command = Reset | Produce ProduceOptions` (`Eq, Show`); `data Options = Options {global :: GlobalOptions, command :: Command}`; `optionsInfo :: ParserInfo Options`.

- [ ] **Step 1: Failing tests**

```haskell
module FanInPerf.OptionsSpec (spec) where

import Data.Domain (Domain (..))
import Data.List.NonEmpty (NonEmpty (..))
import FanInPerf.Options
import FanInPerf.Targets
import Imports
import Options.Applicative
import Test.Hspec

parse :: [String] -> Maybe Options
parse = getParseResult . execParserPure defaultPrefs optionsInfo

spec :: Spec
spec = do
  it "parses produce with defaults" $ do
    let Just o = parse ["--db", "postgresql://u:p@localhost/db", "produce", "--targets", "team:10"]
    o.global.db `shouldBe` "postgresql://u:p@localhost/db"
    o.global.poolSize `shouldBe` Nothing
    o.global.metricsPort `shouldBe` 9400
    o.global.domain `shouldBe` Domain "example.com"
    o.global.isolation `shouldBe` ReadCommitted
    o.command
      `shouldBe` Produce
        ProduceOptions
          { writers = 16,
            targets = TargetEntry KindTeam 10 1 :| [],
            clientsPerUser = 1,
            payloadBytes = 512,
            duration = Nothing,
            warmup = 5
          }

  it "parses all produce flags" $ do
    let Just o =
          parse
            [ "--db", "x", "--pool-size", "8", "--metrics-port", "9500", "--domain", "b.example.com", "--isolation", "serializable",
              "produce", "--writers", "4", "--targets", "user:100x5", "--clients-per-user", "2",
              "--payload-bytes", "64", "--duration", "30", "--warmup", "0"
            ]
    (o.global.poolSize, o.global.metricsPort, o.global.domain, o.global.isolation)
      `shouldBe` (Just 8, 9500, Domain "b.example.com", Serializable)
    o.command
      `shouldBe` Produce (ProduceOptions 4 (TargetEntry KindUser 100 5 :| []) 2 64 (Just 30) 0)

  it "parses reset" $
    fmap (.command) (parse ["--db", "x", "reset"]) `shouldBe` Just Reset

  forM_
    [ ["produce", "--targets", "team:10"],
      ["--db", "x", "produce"],
      ["--db", "x", "produce", "--targets", "team:0"],
      ["--db", "x", "produce", "--targets", "team:10", "--writers", "0"],
      ["--db", "x", "--isolation", "dirty", "reset"],
      ["--db", "x", "--metrics-port", "70000", "reset"],
      ["--db", "x", "--domain", "not a domain", "reset"],
      ["--db", "x"]
    ]
    $ \args ->
      it ("rejects " <> unwords args) $ isNothing (parse args) `shouldBe` True
```

(`-Wincomplete-uni-patterns` warns on `let Just o`; use `o <- maybe (expectationFailure "parse failed" >> undefined) pure (parse ...)` or a helper `parseOk :: [String] -> IO Options` that fails the test on `Nothing`. Prefer the helper.)

- [ ] **Step 2: Run tests to see them fail**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile FAIL.

- [ ] **Step 3: Implement**

```haskell
module FanInPerf.Options
  ( Isolation (..),
    GlobalOptions (..),
    ProduceOptions (..),
    Command (..),
    Options (..),
    optionsInfo,
  )
where

import Data.Domain
import Data.Text qualified as T
import FanInPerf.Targets
import Imports
import Options.Applicative

data Isolation = ReadCommitted | Serializable
  deriving (Eq, Show)

-- | No 'Show' instance: 'db' contains the database password.
data GlobalOptions = GlobalOptions
  { db :: Text,
    poolSize :: Maybe Int,
    metricsPort :: Int,
    domain :: Domain,
    isolation :: Isolation
  }

data ProduceOptions = ProduceOptions
  { writers :: Int,
    targets :: NonEmpty TargetEntry,
    clientsPerUser :: Int,
    payloadBytes :: Int,
    duration :: Maybe Int,
    warmup :: Int
  }
  deriving (Eq, Show)

data Command = Reset | Produce ProduceOptions
  deriving (Eq, Show)

data Options = Options
  { global :: GlobalOptions,
    command :: Command
  }

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    (fullDesc <> progDesc "Benchmark the notification fan-in PostgreSQL store (WPB-26288)")

optionsParser :: Parser Options
optionsParser =
  Options
    <$> globalParser
    <*> hsubparser
      ( command "reset" (info (pure Reset) (progDesc "Truncate all fan-in notification tables"))
          <> command "produce" (info (Produce <$> produceParser) (progDesc "Experiment A: maximal rate of adding notifications"))
      )

globalParser :: Parser GlobalOptions
globalParser =
  GlobalOptions
    <$> strOption (long "db" <> metavar "CONNSTR" <> help "PostgreSQL connection string")
    <*> optional (option positive (long "pool-size" <> metavar "N" <> help "Connection pool size (default: --writers for produce, 1 for reset)"))
    <*> option port (long "metrics-port" <> metavar "PORT" <> value 9400 <> showDefault <> help "Port of the /metrics endpoint")
    <*> option domainReader (long "domain" <> metavar "DOMAIN" <> value (Domain "example.com") <> showDefaultWith (T.unpack . domainText) <> help "Local backend domain")
    <*> option isolationReader (long "isolation" <> metavar "read-committed|serializable" <> value ReadCommitted <> showDefaultWith (const "read-committed") <> help "Isolation level of push transactions")

produceParser :: Parser ProduceOptions
produceParser =
  ProduceOptions
    <$> option positive (long "writers" <> metavar "W" <> value 16 <> showDefault <> help "Concurrent writer threads")
    <*> option targetsReader (long "targets" <> metavar "SPEC" <> help "Target mix KIND:STREAMS[xK],... e.g. user:1000x20,team:10")
    <*> option positive (long "clients-per-user" <> metavar "C" <> value 1 <> showDefault <> help "Client ids per 'clients' target")
    <*> option positive (long "payload-bytes" <> metavar "B" <> value 512 <> showDefault <> help "Approximate JSON payload size")
    <*> optional (option positive (long "duration" <> metavar "SECS" <> help "Run length (default: until Ctrl-C)"))
    <*> option nonNegative (long "warmup" <> metavar "SECS" <> value 5 <> showDefault <> help "Seconds ignored for max-rate tracking")

positive :: ReadM Int
positive = eitherReader $ \s -> case readMaybe s of
  Just n | n > 0 -> Right n
  _ -> Left "expected a positive integer"

nonNegative :: ReadM Int
nonNegative = eitherReader $ \s -> case readMaybe s of
  Just n | n >= 0 -> Right n
  _ -> Left "expected a non-negative integer"

port :: ReadM Int
port = eitherReader $ \s -> case readMaybe s of
  Just n | n > 0 && n <= 65535 -> Right n
  _ -> Left "expected a port between 1 and 65535"

domainReader :: ReadM Domain
domainReader = eitherReader (mkDomain . T.pack)

isolationReader :: ReadM Isolation
isolationReader = eitherReader $ \case
  "read-committed" -> Right ReadCommitted
  "serializable" -> Right Serializable
  _ -> Left "expected read-committed or serializable"

targetsReader :: ReadM (NonEmpty TargetEntry)
targetsReader = eitherReader (parseTargetSpec . T.pack)
```

Note: global flags go before the sub-command (`fan-in-perf --db … produce --targets …`).

- [ ] **Step 4: Run tests**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add tools/fan-in-perf
git commit -m "fan-in-perf: optparse-applicative CLI

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 10: Store runner, produce loop, main

**Files:**
- Create: `tools/fan-in-perf/src/FanInPerf/Store.hs`, `tools/fan-in-perf/src/FanInPerf/Produce.hs`, `tools/fan-in-perf/src/FanInPerf/Run.hs`, `tools/fan-in-perf/test/FanInPerf/ProduceSpec.hs`
- Modify: `tools/fan-in-perf/app/Main.hs`, cabal module lists, `tools/fan-in-perf/README.md`

**Interfaces:**
- Consumes: everything above; `interpretFanInNotificationsStoreToPostgres` (Task 1); `interpretFanInNotificationsAdminToPostgres`, `truncateAll`, `ping` (Task 2); `initPostgresPoolFromConnString` (Task 3).
- Produces:
  - `FanInPerf.Store`: `data Env = Env {pool :: Pool, local :: Local (), isolation :: TxSessions.IsolationLevel}`, `type StoreEffects`, `runStore :: Env -> Sem StoreEffects a -> IO (Either UsageError a)`, `describeUsageError :: UsageError -> Text`, `toIsolationLevel :: Isolation -> TxSessions.IsolationLevel`
  - `FanInPerf.Produce`: `data PushOutcome = PushOk | PushFailed Text`, `writerStep :: (NonEmpty Target -> IO PushOutcome) -> Domain -> V.Vector Entry -> WriterStats -> (Text -> IO ()) -> StdGen -> IO StdGen`, `runProduce :: Console -> Env -> Domain -> ProduceOptions -> IO ()`
  - `FanInPerf.Run`: `run :: Console -> Options -> IO ()`

- [ ] **Step 1: Failing tests** — `test/FanInPerf/ProduceSpec.hs`

```haskell
module FanInPerf.ProduceSpec (spec) where

import Data.Domain (Domain (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import FanInPerf.Produce
import FanInPerf.Stats
import FanInPerf.Store (describeUsageError)
import FanInPerf.Targets
import Hasql.Errors (ConnectionError (NetworkingConnectionError))
import Hasql.Pool (UsageError (..))
import Imports
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec = do
  let dom = Domain "example.com"
      (entries, _) = mkEntries 1 (TargetEntry KindTeam 3 2 :| []) (mkStdGen 1)

  describe "writerStep" $ do
    it "records a successful push with its target count" $ do
      ws <- newWriterStats
      _ <- writerStep (\_ -> pure PushOk) dom entries ws (\_ -> pure ()) (mkStdGen 2)
      s <- readSnapshot ws
      (pushesOf KindTeam s, targetsOf KindTeam s, totalErrors s) `shouldBe` (1, 2, 0)

    it "records a store error and reports it" $ do
      ws <- newWriterStats
      reported <- newIORef []
      _ <- writerStep (\_ -> pure (PushFailed "conflict")) dom entries ws (\m -> modifyIORef reported (m :)) (mkStdGen 2)
      s <- readSnapshot ws
      errorsOf KindTeam s `shouldBe` 1
      readIORef reported `shouldReturn` ["team: conflict"]

    it "counts an exception as error and keeps going" $ do
      ws <- newWriterStats
      reported <- newIORef []
      g <- writerStep (\_ -> throwIO (userError "boom")) dom entries ws (\m -> modifyIORef reported (m :)) (mkStdGen 2)
      _ <- writerStep (\_ -> pure PushOk) dom entries ws (\_ -> pure ()) g
      s <- readSnapshot ws
      (errorsOf KindTeam s, pushesOf KindTeam s) `shouldBe` (1, 1)
      readIORef reported >>= (`shouldSatisfy` any (T.isInfixOf "boom"))

  describe "describeUsageError" $
    it "does not leak connection details" $ do
      let msg = describeUsageError (ConnectionError (NetworkingConnectionError "host=db.internal password=hunter2"))
      msg `shouldSatisfy` (not . T.isInfixOf "hunter2")
      msg `shouldSatisfy` (not . T.isInfixOf "db.internal")
```

- [ ] **Step 2: Run tests to see them fail**

Run: `cabal test fan-in-perf-tests | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: compile FAIL.

- [ ] **Step 3: Implement `FanInPerf.Store`**

```haskell
module FanInPerf.Store
  ( Env (..),
    StoreEffects,
    runStore,
    describeUsageError,
    toIsolationLevel,
  )
where

import Data.Qualified
import Data.Text qualified as T
import FanInPerf.Options (Isolation (..))
import Hasql.Pool (UsageError (..))
import Hasql.Pool.Extended (Pool)
import Hasql.Transaction.Sessions qualified as TxSessions
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.FanInNotificationsAdmin
import Wire.FanInNotificationsAdmin.Postgres
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres

data Env = Env
  { pool :: Pool,
    local :: Local (),
    isolation :: TxSessions.IsolationLevel
  }

type StoreEffects =
  '[ FanInNotificationsStore,
     FanInNotificationsAdmin,
     Input (Local ()),
     Input Pool,
     Error UsageError,
     Embed IO
   ]

runStore :: Env -> Sem StoreEffects a -> IO (Either UsageError a)
runStore env =
  runM
    . runError
    . runInputConst env.pool
    . runInputConst env.local
    . interpretFanInNotificationsAdminToPostgres
    . interpretFanInNotificationsStoreToPostgres env.isolation

-- | Connection errors can contain host names or credentials; keep them out of
-- the terminal.
describeUsageError :: UsageError -> Text
describeUsageError = \case
  ConnectionError _ -> "database connection error"
  AcquisitionTimeoutUsageError -> "connection pool acquisition timeout"
  SessionError e -> "database session error: " <> T.pack (show e)

toIsolationLevel :: Isolation -> TxSessions.IsolationLevel
toIsolationLevel = \case
  ReadCommitted -> TxSessions.ReadCommitted
  Serializable -> TxSessions.Serializable
```

If `UsageError` has more constructors, the compiler flags it (`-Wincomplete-patterns`); map them to generic texts.

- [ ] **Step 4: Implement `FanInPerf.Produce`**

```haskell
module FanInPerf.Produce
  ( PushOutcome (..),
    writerStep,
    runProduce,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Aeson qualified as A
import Data.Domain
import Data.List (unfoldr)
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Metrics
import FanInPerf.Options (ProduceOptions (..))
import FanInPerf.Stats
import FanInPerf.Store
import FanInPerf.Targets
import FanInPerf.Terminal
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Imports
import System.Random
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsStore

data PushOutcome = PushOk | PushFailed Text

-- | One push: generate targets, run, record. Synchronous exceptions are
-- counted as errors so a writer never dies; async ones (cancel) propagate.
writerStep ::
  (NonEmpty Target -> IO PushOutcome) ->
  Domain ->
  V.Vector Entry ->
  WriterStats ->
  (Text -> IO ()) ->
  StdGen ->
  IO StdGen
writerStep doPush dom entries stats reportError g = do
  let ((kind, targets), g') = genTargets dom entries g
  t0 <- getMonotonicTimeNSec
  outcome <- either (PushFailed . T.pack . displayException) id <$> tryAny (doPush targets)
  t1 <- getMonotonicTimeNSec
  case outcome of
    PushOk -> recordSuccess stats kind (length targets) (t1 - t0)
    PushFailed msg -> do
      recordError stats kind
      reportError (kindName kind <> ": " <> msg)
  pure g'

storePush :: Env -> A.Object -> NonEmpty Target -> IO PushOutcome
storePush env payload targets =
  either (PushFailed . describeUsageError) (const PushOk)
    <$> runStore env (pushViaFanIn (mkPush payload targets))

runProduce :: Console -> Env -> Domain -> ProduceOptions -> IO ()
runProduce console env dom opts = do
  gen <- initStdGen
  let (entries, gen') = mkEntries opts.clientsPerUser opts.targets gen
      payload = mkPayload opts.payloadBytes
      writerGens = take opts.writers (unfoldr (Just . split) gen')
  metrics <- newMetrics "produce" opts.writers
  stats <- replicateM opts.writers newWriterStats
  errors <- newTBQueueIO 1000
  let -- error path only; drops messages when the ticker falls behind
      reportError msg = atomically $ do
        full <- isFullTBQueue errors
        unless full (writeTBQueue errors msg)
      writer (ws, g0) =
        let loop !g = writerStep (storePush env payload) dom entries ws reportError g >>= loop
         in loop g0
  start <- getMonotonicTime
  stateRef <- newIORef (initialTickState start)
  let doTick = tickOnce console metrics (fromIntegral opts.warmup) stats errors stateRef
  printLine console $
    "produce: writers=" <> T.pack (show opts.writers) <> " targets=" <> renderTargetSpec opts.targets
  withAsync (mapConcurrently_ writer (zip stats writerGens)) $ \writersA -> do
    link writersA
    withAsync (forever (threadDelay 1_000_000 >> void doTick)) $ \tickerA -> do
      link tickerA
      waitForStop opts.duration
  -- leaving 'withAsync' cancelled writers and ticker
  report <- doTick
  traverse_ (printLine console) (formatSummary report)

-- | Runs on the ticker thread only: aggregates, prints, publishes.
tickOnce :: Console -> Metrics -> Double -> [WriterStats] -> TBQueue Text -> IORef TickState -> IO TickReport
tickOnce console metrics warmup stats errors ref = do
  now <- getMonotonicTime
  total <- sumSnapshots <$> traverse readSnapshot stats
  st <- readIORef ref
  let (report, st') = tick warmup now total st
  writeIORef ref st'
  msgs <- atomically (flushTBQueue errors)
  traverse_ (printLine console) (take maxErrorLines msgs)
  when (length msgs > maxErrorLines) $
    printLine console ("... " <> T.pack (show (length msgs - maxErrorLines)) <> " more errors suppressed")
  publish metrics report
  drawStatus console (formatStatus report)
  pure report
  where
    maxErrorLines = 10

-- | Returns after @duration@ seconds or on Ctrl-C (GHC delivers SIGINT as
-- 'UserInterrupt' to the main thread).
waitForStop :: Maybe Int -> IO ()
waitForStop mDuration = handleJust isInterrupt pure wait
  where
    wait = maybe (forever (threadDelay 1_000_000)) (\d -> threadDelay (d * 1_000_000)) mDuration
    isInterrupt UserInterrupt = Just ()
    isInterrupt _ = Nothing
```

- [ ] **Step 5: Implement `FanInPerf.Run` and `Main`**

```haskell
module FanInPerf.Run (run) where

import Control.Concurrent.Async (link, withAsync)
import Data.Misc (Duration (..))
import Data.Qualified (toLocalUnsafe)
import Data.Text.IO qualified as T
import FanInPerf.Metrics (runMetricsServer)
import FanInPerf.Options
import FanInPerf.Produce (runProduce)
import FanInPerf.Store
import FanInPerf.Terminal
import Hasql.Pool.Extended (PoolConfig (..), initPostgresPoolFromConnString)
import Imports
import PostgresqlConnectionString qualified
import System.Exit (ExitCode (..), exitWith)
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsAdmin (ping, truncateAll)

run :: Console -> Options -> IO ()
run console opts = do
  -- never echo the input: it contains the password
  connStr <- either (const (abort "invalid --db connection string")) pure (PostgresqlConnectionString.parse opts.global.db)
  let size = fromMaybe (defaultPoolSize opts.command) opts.global.poolSize
  pool <- initPostgresPoolFromConnString (poolConfig size) connStr Nothing
  let env =
        Env
          { pool,
            local = toLocalUnsafe opts.global.domain (),
            isolation = toIsolationLevel opts.global.isolation
          }
  checkDatabase env
  case opts.command of
    Reset ->
      runStore env truncateAll
        >>= either
          (\e -> abort ("reset failed: " <> describeUsageError e))
          (const (printLine console "truncated all fan-in notification tables"))
    Produce p ->
      withAsync (runMetricsServer opts.global.metricsPort) $ \server -> do
        -- e.g. port already in use: fail loudly instead of running unobserved
        link server
        runProduce console env opts.global.domain p

checkDatabase :: Env -> IO ()
checkDatabase env =
  tryAny (runStore env ping) >>= \case
    Right (Right ()) -> pure ()
    Right (Left e) -> abort ("cannot reach database: " <> describeUsageError e)
    Left _ -> abort "cannot reach database"

defaultPoolSize :: Command -> Int
defaultPoolSize = \case
  Reset -> 1
  Produce p -> p.writers

poolConfig :: Int -> PoolConfig
poolConfig size =
  PoolConfig
    { size,
      acquisitionTimeout = Duration 10,
      idlenessTimeout = Duration 600
    }

abort :: Text -> IO a
abort msg = T.hPutStrLn stderr msg >> exitWith (ExitFailure 1)
```

`app/Main.hs`:

```haskell
module Main (main) where

import FanInPerf.Options (optionsInfo)
import FanInPerf.Run (run)
import FanInPerf.Terminal (newConsole)
import Imports
import Options.Applicative (execParser)

main :: IO ()
main = do
  opts <- execParser optionsInfo
  console <- newConsole
  run console opts
```

Add `FanInPerf.Store`, `FanInPerf.Produce`, `FanInPerf.Run` to `exposed-modules`, `FanInPerf.ProduceSpec` to test `other-modules`.

- [ ] **Step 6: README**

`tools/fan-in-perf/README.md`:

````markdown
# fan-in-perf

Benchmarks the notification fan-in PostgreSQL store (WPB-26288) through
`Wire.FanInNotificationsStore`. Design: `docs/superpowers/specs/2026-10-08-fan-in-perf-design.md`.

```sh
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" reset
fan-in-perf --db "postgresql://wire-server:posty-the-gres@localhost:5432/backendA" \
  produce --writers 32 --targets user:100000x20,team:10,epoch:1000 --duration 60
```

Global flags go before the sub-command. `--targets` entries are `KIND:STREAMS[xK]`
(`KIND` ∈ `user|clients|team|epoch|connections`): `STREAMS` distinct stream keys,
`K` targets of that kind per push. Each push uses exactly one kind.

Metrics: `http://localhost:9400/metrics` (`--metrics-port`), scraped by the
dockerephemeral OTel collector and shown in Grafana dashboard "fan-in-perf".
````

(The credentials shown are the local dockerephemeral defaults from `deploy/dockerephemeral/docker-compose.yaml`, not secrets.)

- [ ] **Step 7: Run tests + build exe**

Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
Expected: all PASS, no warnings.

Smoke (no DB needed): `cabal run fan-in-perf -- --help | head -20` shows both sub-commands; `cabal run fan-in-perf -- --db 'not a conn string' reset; echo "exit=$?"` prints `invalid --db connection string` (without the input) and `exit=1`.

- [ ] **Step 8: Commit**

```bash
git add tools/fan-in-perf
git commit -m "fan-in-perf: experiment A produce loop, reset, main

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 11: Grafana LGTM integration

**Files:**
- Modify: `deploy/dockerephemeral/docker-compose.yaml` (services `otel-collector`, `grafana-lgtm`)
- Modify: `deploy/dockerephemeral/docker/otel-collector-config.yaml`
- Create: `deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json`

**Interfaces:**
- Consumes: metric names from Task 8; port 9400.

- [ ] **Step 1: Compose**

In `otel-collector` add:

```yaml
    extra_hosts:
      - "host.docker.internal:host-gateway"
```

In `grafana-lgtm.volumes` add next to the postgres-exporter dashboard:

```yaml
      - ./docker/grafana-dashboards/fan-in-perf.json:/otel-lgtm/grafana/conf/provisioning/dashboards/custom/fan-in-perf.json:ro
```

- [ ] **Step 2: Collector scrape job**

In `otel-collector-config.yaml` under `receivers.prometheus.config.scrape_configs` add:

```yaml
        - job_name: 'fan-in-perf'
          scrape_interval: 5s
          static_configs:
            - targets: ['host.docker.internal:9400']
```

- [ ] **Step 3: Dashboard JSON**

Create `fan-in-perf.json` (datasource `null` = default Prometheus datasource, as in `postgres-exporter.json`):

```json
{
  "title": "fan-in-perf",
  "uid": "fan-in-perf",
  "schemaVersion": 39,
  "version": 1,
  "refresh": "5s",
  "time": { "from": "now-15m", "to": "now" },
  "templating": { "list": [] },
  "panels": [
    {
      "id": 1, "type": "timeseries", "title": "Push rate (pushes/s)", "datasource": null,
      "gridPos": { "x": 0, "y": 0, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "sum by (experiment) (rate(fanin_perf_pushes_total[15s]))", "legendFormat": "rate() {{experiment}}" },
        { "refId": "B", "expr": "fanin_perf_push_rate_current", "legendFormat": "current {{experiment}}" },
        { "refId": "C", "expr": "fanin_perf_push_rate_max", "legendFormat": "max {{experiment}}" }
      ]
    },
    {
      "id": 2, "type": "timeseries", "title": "Push rate by kind", "datasource": null,
      "gridPos": { "x": 12, "y": 0, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "sum by (kind) (rate(fanin_perf_pushes_total[15s]))", "legendFormat": "{{kind}}" }
      ]
    },
    {
      "id": 3, "type": "timeseries", "title": "Error rate (errors/s)", "datasource": null,
      "gridPos": { "x": 0, "y": 8, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "sum by (kind) (rate(fanin_perf_errors_total[15s]))", "legendFormat": "{{kind}}" },
        { "refId": "B", "expr": "fanin_perf_error_rate_current", "legendFormat": "current {{experiment}}" }
      ]
    },
    {
      "id": 4, "type": "timeseries", "title": "Error ratio", "datasource": null,
      "gridPos": { "x": 12, "y": 8, "w": 12, "h": 8 },
      "fieldConfig": { "defaults": { "unit": "percentunit" }, "overrides": [] },
      "targets": [
        { "refId": "A", "expr": "fanin_perf_error_ratio_current", "legendFormat": "{{experiment}}" }
      ]
    },
    {
      "id": 5, "type": "timeseries", "title": "Push latency", "datasource": null,
      "gridPos": { "x": 0, "y": 16, "w": 12, "h": 8 },
      "fieldConfig": { "defaults": { "unit": "s" }, "overrides": [] },
      "targets": [
        { "refId": "A", "expr": "histogram_quantile(0.5, sum by (le) (rate(fanin_perf_push_duration_seconds_bucket[30s])))", "legendFormat": "p50" },
        { "refId": "B", "expr": "histogram_quantile(0.99, sum by (le) (rate(fanin_perf_push_duration_seconds_bucket[30s])))", "legendFormat": "p99" }
      ]
    },
    {
      "id": 6, "type": "timeseries", "title": "Targets rate (targets/s)", "datasource": null,
      "gridPos": { "x": 12, "y": 16, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "sum by (kind) (rate(fanin_perf_targets_total[15s]))", "legendFormat": "{{kind}}" }
      ]
    },
    {
      "id": 7, "type": "timeseries", "title": "Connection pool", "datasource": null,
      "gridPos": { "x": 0, "y": 24, "w": 12, "h": 8 },
      "targets": [
        { "refId": "A", "expr": "wire_hasql_pool_in_use{job=\"fan-in-perf\"}", "legendFormat": "in use" },
        { "refId": "B", "expr": "wire_hasql_pool_ready_for_use{job=\"fan-in-perf\"}", "legendFormat": "ready" },
        { "refId": "C", "expr": "fanin_perf_writers", "legendFormat": "writers" }
      ]
    }
  ]
}
```

Note: the OTel collector converts Prometheus metrics to OTLP and LGTM's Prometheus stores them again; counter names may gain/lose the `_total` suffix and `job` may appear as `service_name`. If panels are empty after the acceptance run (Task 12), check metric names in Grafana Explore and adjust the expressions.

- [ ] **Step 4: Validate syntax**

Run: `python3 -c "import json,yaml; json.load(open('deploy/dockerephemeral/docker/grafana-dashboards/fan-in-perf.json')); yaml.safe_load(open('deploy/dockerephemeral/docker-compose.yaml')); yaml.safe_load(open('deploy/dockerephemeral/docker/otel-collector-config.yaml')); print('ok')"`
Expected: `ok`.

- [ ] **Step 5: Commit**

```bash
git add deploy/dockerephemeral
git commit -m "dockerephemeral: scrape fan-in-perf metrics, add Grafana dashboard

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

- [ ] **Step 6: Hand-off note for the user (do not run)**

Tell the user: restart `otel-collector` and `grafana-lgtm` containers to pick up the changes; ensure host firewall allows docker bridge → host:9400.

---

### Task 12: Quality gates + acceptance run

**Files:** none new (fixes only).

- [ ] **Step 1: Format**

Run: `make format 2>&1 | tail -5`, then `git status --short` to see reformatted files.

- [ ] **Step 2: Lint**

Run: `make lint-all 2>&1 | grep -iE "warning|error|suggestion" | head -30`
Expected: nothing for changed files; fix what appears.

- [ ] **Step 3: Nix/Cabal alignment**

Run: `make regen-local-nix-derivations 2>&1 | tail -3 && git status --short nix tools/fan-in-perf/default.nix libs/*/default.nix`
Expected: no unexpected diffs after regeneration (commit any regenerated files).

- [ ] **Step 4: Whole-project build + unit tests of changed packages**

Run: `make c | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` → no errors/warnings.
Run: `make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` → PASS.
Run: `make c package=fan-in-perf test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'` → PASS.

- [ ] **Step 5: Commit fixes**

```bash
git add -A tools/fan-in-perf libs nix cabal.project deploy
git commit -m "fan-in-perf: format, lint, nix alignment

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

(Skip if nothing changed. Never add `rfc.txt`, `task.md`, `ticket.xml`, `.claude/`.)

- [ ] **Step 6: Acceptance run (user approved; requires user to have re-created the schema after Task 1)**

Ask the user to confirm the schema was re-created (Task 1 Step 7), then:

```sh
DB='postgresql://wire-server:posty-the-gres@localhost:5432/backendA'
cabal run fan-in-perf -- --db "$DB" reset
timeout 30 cabal run fan-in-perf -- --db "$DB" produce --writers 8 --targets team:10,user:1000x20,epoch:50,clients:500x2,connections:200 --duration 10 > /tmp/fan-in-perf.log 2>&1; echo "exit=$?"
tail -8 /tmp/fan-in-perf.log
```

While it runs (second shell, within the 10 s): `curl -s localhost:9400/metrics | grep -E '^fanin_perf_(pushes_total|push_rate_max|errors_total)' | head`

Expected: `exit=0`; log (non-TTY) has one status line per second and a summary with pushes > 0 and errors = 0; `/metrics` shows `fanin_perf_*` series for every kind. Any errors → investigate with superpowers:systematic-debugging before reporting.

- [ ] **Step 7: Report**

Report to user: summary numbers, any errors, and the hand-off items (schema regen, container restart, firewall, Grafana check).
