module FanInPerf.Run (run) where

import Control.Concurrent.Async (link, withAsync)
import Data.Misc (Duration (..), showT)
import Data.Qualified (toLocalUnsafe)
import Data.Text qualified as T
import FanInPerf.Consume (runConsume)
import FanInPerf.Metrics (runMetricsServer)
import FanInPerf.Options
import FanInPerf.Pool
import FanInPerf.Produce (runProduce)
import FanInPerf.Store
import FanInPerf.Targets (localDomain)
import FanInPerf.Terminal
import Hasql.Pool.Extended (PoolConfig (..))
import Imports
import PostgresqlConnectionString qualified
import System.Exit (die)
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsAdmin (MigrateResult (..), SchemaShape (..), migrate, ping, truncateAll)

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
  checkDatabase env
  case opts.command of
    Migrate ->
      runStore env (migrate Plain) >>= \case
        Left e -> abort ("migrate failed: " <> showT e)
        Right Migrated -> printLine console "created fan-in notification tables"
        Right AlreadyMigrated -> printLine console "fan-in notification tables already exist"
        Right (ExistsWithOtherShape _) -> abort "fan-in notification tables exist with a different shape"
    Reset ->
      runStore env truncateAll
        >>= either
          (\e -> abort ("reset failed: " <> showT e))
          (const (printLine console "truncated all fan-in notification tables"))
    Produce p -> withMetricsServer (runProduce console env p)
    Consume c -> withMetricsServer $ do
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
  Migrate -> 1
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

abort :: Text -> IO a
abort = die . T.unpack
