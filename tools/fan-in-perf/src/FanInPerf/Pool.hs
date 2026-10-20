-- | Connection pools of the tool. Both pools report into the same
-- @fanin_perf_pool_*@ metric families, told apart by the @role@ label.
module FanInPerf.Pool
  ( PoolRole (..),
    PoolFamilies,
    mkPoolFamilies,
    poolMetrics,
    initPool,
  )
where

import Hasql.Pool.Extended
import Imports
import PostgresqlConnectionString qualified
import Prometheus qualified as P

data PoolRole = WriterPool | ReaderPool
  deriving (Eq, Show)

roleLabel :: PoolRole -> Text
roleLabel = \case
  WriterPool -> "writer"
  ReaderPool -> "reader"

data PoolFamilies = PoolFamilies
  { readyForUse :: P.Vector Text P.Gauge,
    inUse :: P.Vector Text P.Gauge,
    established :: P.Vector Text P.Counter,
    connectionFailure :: P.Vector Text P.Counter,
    acquisitionTimeout :: P.Vector Text P.Counter,
    sessionFailure :: P.Vector Text P.Counter,
    session :: P.Vector Text P.Counter,
    connectionAcquisition :: P.Vector Text P.Histogram,
    sessionDuration :: P.Vector Text P.Histogram
  }

-- | Registers the families; call once per process (the registry is global).
mkPoolFamilies :: IO PoolFamilies
mkPoolFamilies =
  PoolFamilies
    <$> gaugeVec "fanin_perf_pool_ready_for_use" "Number of pool connections ready for use"
    <*> gaugeVec "fanin_perf_pool_in_use" "Number of pool connections in use"
    <*> counterVec "fanin_perf_pool_connection_established_count" "Number of established connections"
    <*> counterVec "fanin_perf_pool_connection_failure_count" "Number of failed connection attempts"
    <*> counterVec "fanin_perf_pool_acquisition_timeout_count" "Number of pool acquisition timeouts"
    <*> counterVec "fanin_perf_pool_session_failure_count" "Number of failed sessions"
    <*> counterVec "fanin_perf_pool_session_count" "Number of sessions"
    <*> histogramVec "fanin_perf_pool_connection_acquisition_seconds" "Time spent establishing new PostgreSQL connections"
    <*> histogramVec "fanin_perf_pool_session_seconds" "Time spent using PostgreSQL sessions"
  where
    gaugeVec n h = P.register $ P.vector "role" $ P.gauge (P.Info n h)
    counterVec n h = P.register $ P.vector "role" $ P.counter (P.Info n h)
    histogramVec n h = P.register $ P.vector "role" $ P.histogram (P.Info n h) P.defaultBuckets

-- | The metrics of one role: the per-role child of every family.
poolMetrics :: PoolFamilies -> PoolRole -> IO HasqlPoolMetrics
poolMetrics fams role =
  HasqlPoolMetrics
    <$> child fams.readyForUse
    <*> child fams.inUse
    <*> child fams.established
    <*> child fams.connectionFailure
    <*> child fams.acquisitionTimeout
    <*> child fams.sessionFailure
    <*> child fams.session
    <*> child fams.connectionAcquisition
    <*> child fams.sessionDuration
  where
    child :: P.Vector Text m -> IO m
    child v = do
      ref <- newIORef Nothing
      P.withLabel v (roleLabel role) (writeIORef ref . Just)
      readIORef ref >>= maybe (fail "pool metrics: no child") pure

-- | Pool with its own role-labelled metrics and gauge reporter.
initPool :: PoolFamilies -> PoolRole -> PoolConfig -> PostgresqlConnectionString.ConnectionString -> IO Pool
initPool fams role config connStr = do
  metrics <- poolMetrics fams role
  pool <- acquirePool metrics config connStr Nothing
  startHasqlPoolStatsReporter pool
  pure pool
