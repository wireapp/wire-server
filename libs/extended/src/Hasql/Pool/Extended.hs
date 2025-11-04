module Hasql.Pool.Extended where

import Data.Aeson
import Data.Map as Map
import Data.Misc
import Data.Set qualified as Set
import Data.UUID
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
  connsRef <- newIORef $ Connections mempty mempty mempty
  HasqlPool.acquire $
    HasqlPool.settings
      [ HasqlPool.staticConnectionSettings $
          [HasqlSetting.connection $ HasqlConn.params pgParams],
        HasqlPool.size config.size,
        HasqlPool.acquisitionTimeout config.acquisitionTimeout.duration,
        HasqlPool.agingTimeout config.agingTimeout.duration,
        HasqlPool.idlenessTimeout config.idlenessTimeout.duration,
        HasqlPool.observationHandler (observationHandler connsRef metrics)
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

data Connections = Connections
  { connecting :: Set UUID,
    inUse :: Set UUID,
    readyForUse :: Set UUID
  }

observationHandler :: IORef Connections -> HasqlPoolMetrics -> Observation -> IO ()
observationHandler connsRef metrics (ConnectionObservation connId status) = do
  case status of
    ConnectingConnectionStatus -> do
      modifyIORef' connsRef (\conns -> conns {connecting = Set.insert connId conns.connecting})
    ReadyForUseConnectionStatus reason -> do
      connsChange <- case reason of
        SessionFailedConnectionReadyForUseReason _ -> do
          incCounter metrics.sessionFailureCounter
          pure $ \conns -> conns {inUse = Set.delete connId conns.inUse}
        SessionSucceededConnectionReadyForUseReason -> do
          pure $ \conns -> conns {inUse = Set.delete connId conns.inUse}
        EstablishedConnectionReadyForUseReason -> do
          incCounter metrics.establishedCounter
          pure (\conns -> conns {connecting = Set.delete connId conns.connecting})

      (inUseSize, readyForUseSize) <- atomicModifyIORef' connsRef $ \conns ->
        let newConns = (connsChange conns) {readyForUse = Set.insert connId conns.readyForUse}
         in (newConns, (Set.size newConns.inUse, Set.size newConns.readyForUse))

      setGauge metrics.readyForUseGauge (fromIntegral readyForUseSize)
      setGauge metrics.inUseGauge (fromIntegral inUseSize)
    InUseConnectionStatus -> do
      incCounter metrics.sessionCounter
      (inUseSize, readyForUseSize) <- atomicModifyIORef' connsRef $ \conns ->
        let newConns =
              conns
                { readyForUse = Set.delete connId conns.readyForUse,
                  inUse = Set.insert connId conns.inUse
                }
         in (newConns, (Set.size newConns.inUse, Set.size newConns.readyForUse))
      setGauge metrics.readyForUseGauge (fromIntegral readyForUseSize)
      setGauge metrics.inUseGauge (fromIntegral inUseSize)
    TerminatedConnectionStatus _ -> do
      (inUseSize, readyForUseSize) <- atomicModifyIORef' connsRef $ \conns ->
        let newConns =
              conns
                { readyForUse = Set.delete connId conns.readyForUse,
                  inUse = Set.delete connId conns.inUse
                }
         in (newConns, (Set.size newConns.inUse, Set.size newConns.readyForUse))
      setGauge metrics.readyForUseGauge (fromIntegral readyForUseSize)
      setGauge metrics.inUseGauge (fromIntegral inUseSize)
