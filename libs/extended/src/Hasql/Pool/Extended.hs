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
import Data.Map qualified as Map
import Data.Misc
import Data.Secret (SecretText, secretText)
import Hasql.Connection qualified
import Hasql.Connection.Settings qualified as HasqlConnSettings
import Hasql.Pool qualified as HasqlPool
import Imports
import PostgresqlConnectionString qualified
import Prometheus
import UnliftIO.IO (getMonotonicTime)
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

-- | Render a PostgreSQL connection string in libpq key-value format.
--
-- Passwords from the optional secret file are inserted into the key-value map
-- before rendering. The result is wrapped because it may contain the password.
postgresqlConnectionStringWithPassword :: Map Text Text -> Maybe FilePathSecrets -> IO SecretText
postgresqlConnectionStringWithPassword pgConfig mFpSecrets = do
  mPw <- for mFpSecrets initCredentials
  let pgConfig' = maybe pgConfig (\pw -> Map.insert "password" pw pgConfig) mPw
  pure . secretText . PostgresqlConnectionString.toKeyValueString $
    PostgresqlConnectionString.fromKeyValueParams pgConfig'

data HasqlPoolMetrics = HasqlPoolMetrics
  { readyForUseGauge :: Gauge,
    inUseGauge :: Gauge,
    establishedCounter :: Counter,
    connectionFailureCounter :: Counter,
    acquisitionTimeoutCounter :: Counter,
    sessionFailureCounter :: Counter,
    sessionCounter :: Counter,
    connectionAcquisitionDuration :: Histogram,
    sessionDuration :: Histogram
  }

data Pool = Pool
  { rawPool :: HasqlPool.Pool,
    metrics :: HasqlPoolMetrics,
    -- | Pool acquisition timeout in seconds, rounded up from the configured
    -- duration. This is used by the session runner to bound waiting for an
    -- available connection slot.
    poolAcquisitionTimeout :: Duration
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

recordHasqlPoolSessionStarted :: HasqlPoolMetrics -> IO ()
recordHasqlPoolSessionStarted metrics =
  void $ addCounter metrics.sessionCounter 1

recordHasqlPoolSessionFailure :: HasqlPoolMetrics -> IO ()
recordHasqlPoolSessionFailure metrics =
  void $ addCounter metrics.sessionFailureCounter 1

recordHasqlPoolSessionDuration :: HasqlPoolMetrics -> Double -> IO ()
recordHasqlPoolSessionDuration metrics secs =
  observe metrics.sessionDuration secs

recordHasqlPoolAcquisitionTimeout :: HasqlPoolMetrics -> IO ()
recordHasqlPoolAcquisitionTimeout metrics =
  void $ addCounter metrics.acquisitionTimeoutCounter 1

recordHasqlPoolStats :: Pool -> IO ()
recordHasqlPoolStats pool = do
  -- hasql-resource-pool does not expose per-acquire/release callbacks, so
  -- these gauges are refreshed from the pool's current total connections stats instead.
  poolStats <- HasqlPool.stats pool.rawPool
  setGauge pool.metrics.readyForUseGauge (fromIntegral poolStats.available)
  setGauge pool.metrics.inUseGauge (fromIntegral poolStats.currentUsage)

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
    instrumentedConnectionGetter metrics getter = do
      started <- getMonotonicTime
      res <- getter
      ended <- getMonotonicTime
      recordHasqlPoolConnectionAcquisition metrics (ended - started)
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
        <*> register (counter $ Info "wire_hasql_pool_connection_failure_count" "Number of failed connection acquisition attempts")
        <*> register (counter $ Info "wire_hasql_pool_acquisition_timeout_count" "Number of pool acquisition timeouts")
        <*> register (counter $ Info "wire_hasql_pool_session_failure_count" "Number of times a session has failed")
        <*> register (counter $ Info "wire_hasql_pool_session_count" "Number of times a session was created")
        <*> register (histogram (Info "wire_hasql_pool_connection_acquisition_seconds" "Time spent establishing new PostgreSQL connections") defaultBuckets)
        <*> register (histogram (Info "wire_hasql_pool_session_seconds" "Time spent using PostgreSQL sessions") defaultBuckets)

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
