-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Hasql.Pool.Extended where

import Data.Aeson
import Data.Map as Map
import Data.Misc
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Hasql.Connection qualified
import Hasql.Connection.Settings qualified as HasqlConnSettings
import Hasql.Pool qualified as HasqlPool
import Imports
import PostgresqlConnectionString qualified
import Prometheus
import Util.Options

data PoolConfig = PoolConfig
  { size :: Int,
    -- | Configured pool acquisition wait time. hasql-resource-pool only
    -- accepts whole seconds here, so we round up to the nearest second and
    -- pass it through as the pool acquisition timeout.
    acquisitionTimeout :: Duration,
    -- | Controls how long idle connections stay resident in the pool.
    idlenessTimeout :: Duration
  }
  deriving (Eq, Show)

instance FromJSON PoolConfig where
  parseJSON = withObject "PoolConfig" $ \o ->
    PoolConfig
      <$> o .: "size"
      <*> o .: "acquisitionTimeout"
      <*> o .: "idlenessTimeout"

data HasqlPoolMetrics = HasqlPoolMetrics
  { readyForUseGauge :: Gauge,
    inUseGauge :: Gauge,
    establishedCounter :: Counter,
    terminationCounter :: Counter,
    connectionFailureCounter :: Counter,
    sessionFailureCounter :: Counter,
    sessionCounter :: Counter,
    connectionAcquisitionDuration :: Histogram,
    sessionDuration :: Histogram
  }

data Pool = Pool
  { rawPool :: HasqlPool.Pool,
    metrics :: HasqlPoolMetrics,
    -- we periodically store the total number of live connections
    -- to be able to approximate the terminated count
    totalConnectionsStats :: IORef Int
  }

recordHasqlPoolConnectionAcquisition :: HasqlPoolMetrics -> Double -> IO ()
recordHasqlPoolConnectionAcquisition metrics secs =
  observe metrics.connectionAcquisitionDuration secs

recordHasqlPoolConnectionEstablished :: HasqlPoolMetrics -> IO ()
recordHasqlPoolConnectionEstablished metrics =
  void $ addCounter metrics.establishedCounter 1

recordHasqlPoolConnectionFailure :: HasqlPoolMetrics -> IO ()
recordHasqlPoolConnectionFailure metrics =
  void $ addCounter metrics.connectionFailureCounter 1

recordHasqlPoolSessionStarted :: Pool -> IO ()
recordHasqlPoolSessionStarted pool =
  void $ addCounter pool.metrics.sessionCounter 1

recordHasqlPoolSessionFailure :: Pool -> IO ()
recordHasqlPoolSessionFailure pool =
  void $ addCounter pool.metrics.sessionFailureCounter 1

recordHasqlPoolSessionDuration :: Pool -> Double -> IO ()
recordHasqlPoolSessionDuration pool secs =
  observe pool.metrics.sessionDuration secs

recordHasqlPoolStats :: Pool -> IO ()
recordHasqlPoolStats pool = do
  -- hasql-resource-pool does not expose per-acquire/release callbacks, so
  -- these gauges are refreshed from the pool's current total connections stats instead.
  poolStats <- HasqlPool.stats pool.rawPool
  setGauge pool.metrics.readyForUseGauge (fromIntegral poolStats.available)
  setGauge pool.metrics.inUseGauge (fromIntegral poolStats.currentUsage)
  let total = poolStats.currentUsage + poolStats.available
  prevTotal <- readIORef pool.totalConnectionsStats
  let delta = total - prevTotal
  when (delta < 0) $ replicateM_ (abs delta) (addCounter pool.metrics.terminationCounter 1)
  writeIORef pool.totalConnectionsStats (poolStats.currentUsage + poolStats.available)

startHasqlPoolStatsReporter :: Pool -> IO ()
startHasqlPoolStatsReporter pool = void $ forkIO $ forever $ do
  recordHasqlPoolStats pool
  threadDelay (5 * 1_000_000) -- 5s

-- | Creates a pool from postgres config params.
--
-- 'acquisitionTimeout' is mapped to the pool acquisition timeout,
-- 'idlenessTimeout' controls how long idle connections stay resident.
initPostgresPool :: PoolConfig -> Map Text Text -> Maybe FilePathSecrets -> IO Pool
initPostgresPool config pgConfig mFpSecrets = do
  mPw <- for mFpSecrets initCredentials
  let pgSettings =
        HasqlConnSettings.connectionString (PostgresqlConnectionString.toUrl $ PostgresqlConnectionString.fromKeyValueParams pgConfig)
          <> foldMap HasqlConnSettings.password mPw
  metrics <- mkHasqlPoolMetrics
  totalConnectionsStats <- newIORef 0
  rawPool <-
    HasqlPool.acquireWith
      (instrumentedConnectionGetter metrics (Hasql.Connection.acquire pgSettings))
      ( config.size,
        realToFrac config.idlenessTimeout.duration,
        poolAcquireSettings config.acquisitionTimeout
      )
  let pool = Pool {rawPool, metrics, totalConnectionsStats}
  startHasqlPoolStatsReporter pool
  pure pool
  where
    instrumentedConnectionGetter metrics getter = do
      started <- getCurrentTime
      res <- getter
      ended <- getCurrentTime
      recordHasqlPoolConnectionAcquisition metrics (realToFrac (diffUTCTime ended started))
      case res of
        Right _ -> recordHasqlPoolConnectionEstablished metrics
        Left _ -> recordHasqlPoolConnectionFailure metrics
      pure res

    mkHasqlPoolMetrics :: IO HasqlPoolMetrics
    mkHasqlPoolMetrics =
      HasqlPoolMetrics
        <$> register (gauge $ Info "wire_hasql_pool_ready_for_use" "Number of hasql pool connections ready for use")
        <*> register (gauge $ Info "wire_hasql_pool_in_use" "Number of hasql pool connections in use")
        <*> register (counter $ Info "wire_hasql_pool_connection_established_count" "Number of established connections")
        <*> register (counter $ Info "wire_hasql_pool_connection_terminated_count" "Number of terminated connections")
        <*> register (counter $ Info "wire_hasql_pool_connection_failure_count" "Number of failed connection acquisition attempts")
        <*> register (counter $ Info "wire_hasql_pool_session_failure_count" "Number of times a session has failed")
        <*> register (counter $ Info "wire_hasql_pool_session_count" "Number of times a session was created")
        <*> register (histogram (Info "wire_hasql_pool_connection_acquisition_seconds" "Time spent establishing new PostgreSQL connections") defaultBuckets)
        <*> register (histogram (Info "wire_hasql_pool_session_seconds" "Time spent using PostgreSQL sessions") defaultBuckets)

    poolAcquireSettings acquisitionTimeout =
      -- The custom getter above performs the actual connection establishment.
      -- This record configures pool behavior, including acquisition timing.
      HasqlPool.ConnectionSettings
        { host = "",
          port = 5432,
          user = "",
          password = "",
          dbName = "",
          connAcqTimeout = acquisitionTimeoutSeconds acquisitionTimeout,
          txIdleTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          stmtTimeout = HasqlPool.TimeoutSetting 0 HasqlPool.Seconds,
          sslMode = "prefer",
          sslRootCert = ""
        }

    acquisitionTimeoutSeconds d
      | d.duration <= 0 = 0
      | otherwise = fromInteger $ ceiling (realToFrac d.duration :: Double)
