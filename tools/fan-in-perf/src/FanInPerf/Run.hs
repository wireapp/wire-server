module FanInPerf.Run (run) where

import Control.Concurrent.Async (link, withAsync)
import Data.Misc (Duration (..))
import Data.Qualified (toLocalUnsafe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FanInPerf.Consume (runConsume)
import FanInPerf.Metrics (runMetricsServer)
import FanInPerf.Options
import FanInPerf.Produce (runProduce)
import FanInPerf.Store
import FanInPerf.Targets (localDomain)
import FanInPerf.Terminal
import Hasql.Pool.Extended (PoolConfig (..), initPostgresPoolFromConnString, initPostgresPoolSharingMetrics)
import Imports
import PostgresqlConnectionString qualified
import System.Exit (ExitCode (..), exitWith)
import UnliftIO.Exception (tryAny)
import Wire.FanInNotificationsAdmin (MigrateResult (..), migrate, ping, truncateAll)

run :: Console -> Options -> IO ()
run console opts = do
  -- never echo the input: it contains the password
  connStr <- either (const (abort "invalid --db connection string")) pure (PostgresqlConnectionString.parse opts.global.db)
  let size = fromMaybe (defaultPoolSize opts.command) opts.global.poolSize
  pool <- initPostgresPoolFromConnString (poolConfig size) connStr Nothing
  let env =
        Env
          { pool,
            local = toLocalUnsafe localDomain (),
            isolation = opts.global.isolation
          }
  checkDatabase env
  case opts.command of
    Migrate ->
      runStore env migrate >>= \case
        Left e -> abort ("migrate failed: " <> T.pack (show e))
        Right Migrated -> printLine console "created fan-in notification tables"
        Right AlreadyMigrated -> printLine console "fan-in notification tables already exist"
    Reset ->
      runStore env truncateAll
        >>= either
          (\e -> abort ("reset failed: " <> T.pack (show e)))
          (const (printLine console "truncated all fan-in notification tables"))
    Produce p ->
      withAsync (runMetricsServer opts.global.metricsPort) $ \server -> do
        -- e.g. port already in use: fail loudly instead of running unobserved
        link server
        runProduce console env p
    Consume c ->
      withAsync (runMetricsServer opts.global.metricsPort) $ \server -> do
        link server
        -- readers get their own pool so they cannot starve the writers
        readerPool <- initPostgresPoolSharingMetrics pool (poolConfig c.readerPoolSize) connStr Nothing
        let readerEnv = Env {pool = readerPool, local = env.local, isolation = env.isolation}
        runConsume console env readerEnv c

checkDatabase :: Env -> IO ()
checkDatabase env =
  tryAny (runStore env ping) >>= \case
    Right (Right ()) -> pure ()
    Right (Left e) -> abort ("cannot reach database: " <> T.pack (show e))
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
      acquisitionTimeout = Duration 10,
      idlenessTimeout = Duration 600
    }

abort :: Text -> IO a
abort msg = T.hPutStrLn stderr msg >> exitWith (ExitFailure 1)
