module Hasql.Pool.Extended where

import Data.Aeson
import Data.Map as Map
import Data.Misc
import Hasql.Connection.Setting qualified as HasqlSetting
import Hasql.Connection.Setting.Connection qualified as HasqlConn
import Hasql.Connection.Setting.Connection.Param qualified as HasqlConfig
import Hasql.Pool as HasqlPool
import Hasql.Pool.Config qualified as HasqlPool
import Hasql.Pool.Observation
import Imports
import Prometheus
import Util.Options

data PoolConfig = PoolConfig
  { size :: Int,
    acquisitionTimeout :: Duration,
    agingTimeout :: Duration,
    idlenessTimeout :: Duration
  }
  deriving (Eq, Show)

instance FromJSON PoolConfig where
  parseJSON = withObject "PoolConfig" $ \o ->
    PoolConfig
      <$> o .: "size"
      <*> o .: "acquisitionTimeout"
      <*> o .: "agingTimeout"
      <*> o .: "idlenessTimeout"

-- | Creates a pool from postgres config params
--
-- HasqlConn.params translates pgParams into connection (which just holds the connection string and is not a real connection)
-- HasqlSetting.connection unwraps the connection string out of connection
-- HasqlPool.staticConnectionSettings translates the connection string to the pool settings
-- HasqlPool.settings translates the pool settings into pool config
-- HasqlPool.acquire creates the pool.
-- ezpz.
initPostgresPool :: PoolConfig -> Map Text Text -> Maybe FilePathSecrets -> IO HasqlPool.Pool
initPostgresPool config pgConfig mFpSecrets = do
  mPw <- for mFpSecrets initCredentials
  let pgConfigWithPw = maybe pgConfig (\pw -> Map.insert "password" pw pgConfig) mPw
      pgParams = Map.foldMapWithKey (\k v -> [HasqlConfig.other k v]) pgConfigWithPw
  metrics <- initHasqlPoolMetrics
  HasqlPool.acquire $
    HasqlPool.settings
      [ HasqlPool.staticConnectionSettings $
          [HasqlSetting.connection $ HasqlConn.params pgParams],
        HasqlPool.size config.size,
        HasqlPool.acquisitionTimeout config.acquisitionTimeout.duration,
        HasqlPool.agingTimeout config.agingTimeout.duration,
        HasqlPool.idlenessTimeout config.idlenessTimeout.duration,
        HasqlPool.observationHandler (observationHandler metrics)
      ]

data HasqlPoolMetrics = HasqlPoolMetrics
  { readyForUseGauge :: Gauge,
    inUseGauge :: Gauge,
    establishedCounter :: Counter,
    terminationCounter :: Counter,
    sessionFailureCounter :: Counter,
    sessionCounter :: Counter
  }

initHasqlPoolMetrics :: IO HasqlPoolMetrics
initHasqlPoolMetrics = do
  HasqlPoolMetrics
    <$> register (gauge $ Info "wire_hasql_pool_ready_for_use" "Number of hasql pool connections ready for use")
    <*> register (gauge $ Info "wire_hasql_pool_in_use" "Number of hasql pool connections ready for use")
    <*> register (counter $ Info "wire_hasql_pool_connection_established_count" "Number of established connections")
    <*> register (counter $ Info "wire_hasql_pool_connection_terminated_count" "Number of terminated connections")
    <*> register (counter $ Info "wire_hasql_pool_session_failure_count" "Number of times a session has failed")
    <*> register (counter $ Info "wire_hasql_pool_session_count" "Number of times a session was created")

observationHandler :: HasqlPoolMetrics -> Observation -> IO ()
observationHandler metrics (ConnectionObservation _ status) = do
  case status of
    ConnectingConnectionStatus -> pure ()
    ReadyForUseConnectionStatus reason -> do
      case reason of
        SessionFailedConnectionReadyForUseReason _ -> do
          decGauge metrics.inUseGauge
          incCounter metrics.sessionFailureCounter
        SessionSucceededConnectionReadyForUseReason ->
          decGauge metrics.inUseGauge
        EstablishedConnectionReadyForUseReason ->
          incCounter metrics.establishedCounter
      incGauge metrics.readyForUseGauge
    InUseConnectionStatus -> do
      decGauge metrics.readyForUseGauge
      incGauge metrics.inUseGauge
      incCounter metrics.sessionCounter
    TerminatedConnectionStatus _ -> do
      decGauge metrics.readyForUseGauge
