module FanInPerf.Run (run) where

import Control.Concurrent.Async (link, withAsync)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Misc (Duration (..), showT)
import Data.Qualified (toLocalUnsafe)
import FanInPerf.Consume (runConsume, runConsumeOnly)
import FanInPerf.Database
import FanInPerf.Metrics (runMetricsServer)
import FanInPerf.NotificationsAdmin (SchemaShape (..), checkpoint, countDeadlocks, countPartitions, countRows, createDatabase, createTables, currentShape, databaseExists, dropTables, getServerInfo, ping, setupPartman, unDbName, vacuumAnalyze)
import FanInPerf.Options
import FanInPerf.Pool
import FanInPerf.Produce (runProduce)
import FanInPerf.Settle (waitUntilStable)
import FanInPerf.Store
import FanInPerf.Summary (DatabaseSummary (..), RunSummary (database))
import FanInPerf.Targets (localDomain)
import FanInPerf.Terminal
import Hasql.Pool.Extended (PoolConfig (..))
import Imports
import PostgresqlConnectionString qualified
import UnliftIO.Exception (tryAny)

run :: Console -> Options -> IO ()
run console opts = do
  -- never echo the input: it contains the password
  connStr <- either (const (abort "invalid --db connection string")) pure (PostgresqlConnectionString.parse opts.global.db)
  let size = fromMaybe (defaultPoolSize opts.command) opts.global.poolSize
  fams <- mkPoolFamilies
  pool <- initPool fams WriterPool (poolConfig size) connStr
  let env =
        Env
          { pool,
            local = toLocalUnsafe localDomain (),
            isolation = opts.global.isolation
          }
  -- the maintenance database, through its own single-connection pool
  let mkAdminEnv = do
        adminPool <- initPool fams AdminPool (poolConfig 1) (adminConnStr connStr)
        pure env {pool = adminPool}
  -- fail now, not after a run of minutes
  forM_ opts.global.summaryJson $ \file -> do
    unless (hasSummary opts.command) $ abort "--summary-json: only for produce and the consume commands"
    BL.writeFile file mempty
  let writeSummary :: RunSummary -> IO ()
      writeSummary s = forM_ opts.global.summaryJson $ \file -> BL.writeFile file (A.encode s <> "\n")
      -- A load command: @prepare@ returns the run, which is measured with the
      -- metrics server up and the database facts around it.
      runLoad :: IO (IO RunSummary) -> IO ()
      runLoad prepare = do
        checkDatabase env
        summary <- withAsync (runMetricsServer opts.global.metricsPort) $ \server -> do
          -- e.g. port already in use: fail loudly instead of running unobserved
          link server
          prepare >>= withDatabaseFacts console env
        writeSummary summary
  case opts.command of
    ResetDb reset -> do
      name <- experimentDb connStr & either abort pure
      adminEnv <- mkAdminEnv
      exists <- runOrAbort "database check" adminEnv (databaseExists name)
      unless exists $ do
        runOrAbort "create database" adminEnv (createDatabase name)
        printLine console ("created database " <> unDbName name)
      checkDatabase env
      runOrAbort "drop tables" env dropTables
      -- partman first: the partitioned schema needs it
      when (reset.shape == Partitioned) $
        runOrAbort "setup partman" env (setupPartman name)
      runOrAbort "create tables" env (createTables reset.shape)
      printLine console ("recreated fan-in notification tables (" <> shapeName reset.shape <> ")")
      when (reset.shape == Partitioned && reset.settle) $ settlePartman console env
    Vacuum -> do
      checkDatabase env
      runOrAbort "vacuum" env vacuumAnalyze
      printLine console "vacuumed (VACUUM ANALYZE)"
    DbInfo -> do
      checkDatabase env
      info <- runOrAbort "server info" env getServerInfo
      BL.putStr (A.encode info <> "\n")
    Produce p -> runLoad $ pure (runProduce console env p)
    Consume c -> runLoad $ do
      -- partitioned: the drops are sampled on a connection of their own
      samplerEnv <-
        runOrAbort "shape check" env currentShape >>= \case
          Just Partitioned -> do
            samplerPool <- initPool fams SamplerPool (poolConfig 1) connStr
            pure (Just env {pool = samplerPool})
          _ -> pure Nothing
      -- readers get their own pool so they cannot starve the writers
      readerPool <- initPool fams ReaderPool (poolConfig c.readerPoolSize) connStr
      pure (runConsume console env env {pool = readerPool} samplerEnv c)
    ConsumeOnly c -> runLoad $ do
      runOrAbort "shape check" env currentShape >>= \case
        Just Partitioned -> printLine console "warning: partitioned tables: pg_partman retention can drop rows during the run, they then count as missing"
        _ -> pure ()
      -- counting and reading both use the readers' pool (the writer pool stays idle)
      readerPool <- initPool fams ReaderPool (poolConfig c.readerPoolSize) connStr
      pure (runConsumeOnly console env {pool = readerPool} c)

checkDatabase :: Env -> IO ()
checkDatabase env =
  tryAny (runStore env ping) >>= \case
    Right (Right ()) -> pure ()
    Right (Left e) -> abort ("cannot reach database: " <> showT e)
    Left _ -> abort "cannot reach database"

-- | Whether the command runs a load and so has a summary.
hasSummary :: Command -> Bool
hasSummary = \case
  Produce _ -> True
  Consume _ -> True
  ConsumeOnly _ -> True
  _ -> False

defaultPoolSize :: Command -> Int
defaultPoolSize = \case
  ResetDb _ -> 1
  Vacuum -> 1
  DbInfo -> 1
  Produce p -> p.writers
  Consume c -> c.writerOptions.writers
  ConsumeOnly _ -> 1

poolConfig :: Int -> PoolConfig
poolConfig size =
  PoolConfig
    { size,
      -- 0: wait for a connection without a timeout. hasql-resource-pool forks a
      -- sleeping thread per acquire that lives for the whole timeout (hundreds
      -- of thousands of threads, GBs of stacks, a starved ticker at load). Sizes
      -- keep waits short: writers' pool = writers, readers go through a gate.
      acquisitionTimeout = Duration 0,
      idlenessTimeout = Duration 600
    }

-- | Around a run, on the writers' pool but outside the measured phase:
--
-- * CHECKPOINT first, so that every run starts at the same point of the
--   checkpoint cycle (see 'checkpoint').
-- * The deadlocks of the database during the run and the rows it holds at the
--   end, printed below the summary (@summary.database@ in the JSON).
withDatabaseFacts :: Console -> Env -> IO RunSummary -> IO RunSummary
withDatabaseFacts console env act = do
  runOrAbort "checkpoint" env checkpoint
  deadlocksBefore <- runOrAbort "deadlock count" env countDeadlocks
  summary <- act
  -- backends report their statistics (deadlocks) with a delay, and a push that
  -- is still committing after the writers were stopped has no effect on them
  threadDelay 2_000_000
  deadlocksAfter <- runOrAbort "deadlock count" env countDeadlocks
  rows <- runOrAbort "row count" env countRows
  let db = DatabaseSummary {deadlocks = deadlocksAfter - deadlocksBefore, rows}
  traverse_ (printLine console) (formatDatabase db)
  pure summary {database = Just db}

-- | pg_partman's first maintenance run drops the premade past partitions in
-- bulk; a run started before it is done would be disturbed by it. Waits until
-- the number of partitions did not change for 75 s.
settlePartman :: Console -> Env -> IO ()
settlePartman console env = do
  printLine console "waiting for pg_partman to settle (partition count of the six tables stable for 75 s)"
  waitUntilStable 75 240 5 (runOrAbort "partition count" env countPartitions) >>= \case
    Just n -> printLine console ("pg_partman settled with " <> showT n <> " partitions in total (six tables)")
    Nothing -> printLine console "warning: partition count did not settle within 240 s"

shapeName :: SchemaShape -> Text
shapeName = \case
  Plain -> "plain"
  Partitioned -> "partitioned"
