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
