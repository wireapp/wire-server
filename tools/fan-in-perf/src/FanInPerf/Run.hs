module FanInPerf.Run (run) where

import Control.Concurrent.Async (link, withAsync)
import Data.Misc (Duration (..), showT)
import Data.Qualified (toLocalUnsafe)
import Data.Text qualified as T
import FanInPerf.Consume (runConsume)
import FanInPerf.Database
import FanInPerf.Metrics (runMetricsServer)
import FanInPerf.Options
import FanInPerf.Pool
import FanInPerf.Produce (runProduce)
import FanInPerf.Store
import FanInPerf.Targets (localDomain)
import FanInPerf.Terminal
import Hasql.Pool.Extended (PoolConfig (..))
import Imports
import Polysemy (Sem)
import PostgresqlConnectionString qualified
import System.Exit (die)
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsAdmin (MigrateResult (..), SchemaShape (..), createDatabase, databaseExists, dropDatabase, migrate, ping, setupPartman, truncateAll, unDbName)

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
  case opts.command of
    Migrate shape -> do
      name <- experimentDb connStr & either abort pure
      adminEnv <- mkAdminEnv
      exists <- runOrAbort "database check" adminEnv (databaseExists name)
      unless exists $ do
        runOrAbort "create database" adminEnv (createDatabase name)
        printLine console ("created database " <> unDbName name)
      checkDatabase env
      -- partman first: the partitioned migration needs it
      when (shape == Partitioned) $ runOrAbort "setup partman" env (setupPartman name)
      runOrAbort "migrate" env (migrate shape) >>= \case
        Migrated -> printLine console ("created fan-in notification tables (" <> shapeName shape <> ")")
        AlreadyMigrated -> printLine console "fan-in notification tables already exist"
        ExistsWithOtherShape other -> abort ("fan-in notification tables exist with other shape (" <> shapeName other <> "); run drop-db first")
    DropDb -> do
      name <- experimentDb connStr & either abort pure
      adminEnv <- mkAdminEnv
      runOrAbort "drop database" adminEnv (dropDatabase name)
      printLine console ("dropped database " <> unDbName name)
    Reset -> do
      checkDatabase env
      runStore env truncateAll
        >>= either
          (\e -> abort ("reset failed: " <> showT e))
          (const (printLine console "truncated all fan-in notification tables"))
    Produce p -> do
      checkDatabase env
      withMetricsServer (runProduce console env p)
    Consume c -> withMetricsServer $ do
      checkDatabase env
      -- readers get their own pool so they cannot starve the writers
      readerPool <- initPool fams ReaderPool (poolConfig c.readerPoolSize) connStr
      runConsume console env env {pool = readerPool} c
  where
    withMetricsServer act =
      withAsync (runMetricsServer opts.global.metricsPort) $ \server -> do
        -- e.g. port already in use: fail loudly instead of running unobserved
        link server
        act

checkDatabase :: Env -> IO ()
checkDatabase env =
  tryAny (runStore env ping) >>= \case
    Right (Right ()) -> pure ()
    Right (Left e) -> abort ("cannot reach database: " <> showT e)
    Left _ -> abort "cannot reach database"

defaultPoolSize :: Command -> Int
defaultPoolSize = \case
  Migrate _ -> 1
  DropDb -> 1
  Reset -> 1
  Produce p -> p.writers
  Consume c -> c.writerOptions.writers

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

runOrAbort :: Text -> Env -> Sem StoreEffects a -> IO a
runOrAbort what env act =
  runStore env act >>= either (\e -> abort (what <> " failed: " <> showT e)) pure

shapeName :: SchemaShape -> Text
shapeName = \case
  Plain -> "plain"
  Partitioned -> "partitioned"

abort :: Text -> IO a
abort = die . T.unpack
