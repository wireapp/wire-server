-- | Reader-side statistics, same scheme as "FanInPerf.Stats": one 'IORef' per
-- reader client written only by it, summed by the ticker.
module FanInPerf.ConsumeStats
  ( ReaderStats,
    newReaderStats,
    recordFetch,
    recordFetchError,
    recordAck,
    recordAckError,
    ReaderSnapshot,
    emptyReaderSnapshot,
    readReaderSnapshot,
    sumReaderSnapshots,
    diffReaderSnapshot,
    deliveredOf,
    fetchesOf,
    emptyFetchesOf,
    fetchErrorsOf,
    acksOf,
    ackErrorsOf,
    totalDelivered,
    totalFetches,
    totalEmptyFetches,
    totalFetchErrors,
    totalAcks,
    totalAckErrors,
    fetchLatencyBuckets,
    lagBuckets,
    ackLatencyBuckets,
    ConsumeTickState,
    initialConsumeTickState,
    ConsumeReport (..),
    consumeTick,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Stats (bucketIndex, minTickInterval, quantileSeconds, toBuckets)
import FanInPerf.Targets (TargetKind)
import Imports

data ReaderSnapshot = ReaderSnapshot
  { delivered :: !(Map TargetKind Int),
    fetches :: !(Map TargetKind Int),
    emptyFetches :: !(Map TargetKind Int),
    fetchErrors :: !(Map TargetKind Int),
    fetchLatency :: !(IntMap Int),
    lag :: !(IntMap Int),
    acks :: !(Map TargetKind Int),
    ackErrors :: !(Map TargetKind Int),
    ackLatency :: !(IntMap Int)
  }
  deriving (Eq, Show)

newtype ReaderStats = ReaderStats (IORef ReaderSnapshot)

emptyReaderSnapshot :: ReaderSnapshot
emptyReaderSnapshot =
  ReaderSnapshot
    { delivered = mempty,
      fetches = mempty,
      emptyFetches = mempty,
      fetchErrors = mempty,
      fetchLatency = mempty,
      lag = mempty,
      acks = mempty,
      ackErrors = mempty,
      ackLatency = mempty
    }

newReaderStats :: IO ReaderStats
newReaderStats = ReaderStats <$> newIORef emptyReaderSnapshot

-- | See the memory-barrier note in "FanInPerf.Stats".
modifyReader :: ReaderStats -> (ReaderSnapshot -> ReaderSnapshot) -> IO ()
modifyReader (ReaderStats r) f = atomicModifyIORef' r (\s -> (f s, ()))

bump :: TargetKind -> Int -> Map TargetKind Int -> Map TargetKind Int
bump = Map.insertWith (+)

-- | One fetch of a stream of the given kind: its latency (ns) and the lag (ns)
-- of every notification it returned.
recordFetch :: ReaderStats -> TargetKind -> Word64 -> [Word64] -> IO ()
recordFetch stats k latencyNs lagsNs =
  modifyReader stats $ \s ->
    s
      { delivered = bump k (length lagsNs) s.delivered,
        fetches = bump k 1 s.fetches,
        emptyFetches = if null lagsNs then bump k 1 s.emptyFetches else s.emptyFetches,
        fetchLatency = IntMap.insertWith (+) (bucketIndex latencyNs) 1 s.fetchLatency,
        lag = foldl' (\m ns -> IntMap.insertWith (+) (bucketIndex ns) 1 m) s.lag lagsNs
      }

recordFetchError :: ReaderStats -> TargetKind -> IO ()
recordFetchError stats k = modifyReader stats $ \s -> s {fetchErrors = bump k 1 s.fetchErrors}

-- | One ack upsert of a stream of the given kind and its latency (ns).
recordAck :: ReaderStats -> TargetKind -> Word64 -> IO ()
recordAck stats k latencyNs =
  modifyReader stats $ \s ->
    s
      { acks = bump k 1 s.acks,
        ackLatency = IntMap.insertWith (+) (bucketIndex latencyNs) 1 s.ackLatency
      }

recordAckError :: ReaderStats -> TargetKind -> IO ()
recordAckError stats k = modifyReader stats $ \s -> s {ackErrors = bump k 1 s.ackErrors}

readReaderSnapshot :: ReaderStats -> IO ReaderSnapshot
readReaderSnapshot (ReaderStats r) = atomicModifyIORef' r (\s -> (s, s))

sumReaderSnapshots :: [ReaderSnapshot] -> ReaderSnapshot
sumReaderSnapshots = foldl' add emptyReaderSnapshot
  where
    add x y =
      ReaderSnapshot
        { delivered = Map.unionWith (+) x.delivered y.delivered,
          fetches = Map.unionWith (+) x.fetches y.fetches,
          emptyFetches = Map.unionWith (+) x.emptyFetches y.emptyFetches,
          fetchErrors = Map.unionWith (+) x.fetchErrors y.fetchErrors,
          fetchLatency = IntMap.unionWith (+) x.fetchLatency y.fetchLatency,
          lag = IntMap.unionWith (+) x.lag y.lag,
          acks = Map.unionWith (+) x.acks y.acks,
          ackErrors = Map.unionWith (+) x.ackErrors y.ackErrors,
          ackLatency = IntMap.unionWith (+) x.ackLatency y.ackLatency
        }

diffReaderSnapshot :: ReaderSnapshot -> ReaderSnapshot -> ReaderSnapshot
diffReaderSnapshot new old =
  ReaderSnapshot
    { delivered = diffMap new.delivered old.delivered,
      fetches = diffMap new.fetches old.fetches,
      emptyFetches = diffMap new.emptyFetches old.emptyFetches,
      fetchErrors = diffMap new.fetchErrors old.fetchErrors,
      fetchLatency = IntMap.unionWith (+) new.fetchLatency (negate <$> old.fetchLatency),
      lag = IntMap.unionWith (+) new.lag (negate <$> old.lag),
      acks = diffMap new.acks old.acks,
      ackErrors = diffMap new.ackErrors old.ackErrors,
      ackLatency = IntMap.unionWith (+) new.ackLatency (negate <$> old.ackLatency)
    }
  where
    diffMap n o = Map.unionWith (+) n (negate <$> o)

deliveredOf, fetchesOf, emptyFetchesOf, fetchErrorsOf :: TargetKind -> ReaderSnapshot -> Int
deliveredOf k s = Map.findWithDefault 0 k s.delivered
fetchesOf k s = Map.findWithDefault 0 k s.fetches
emptyFetchesOf k s = Map.findWithDefault 0 k s.emptyFetches
fetchErrorsOf k s = Map.findWithDefault 0 k s.fetchErrors

acksOf, ackErrorsOf :: TargetKind -> ReaderSnapshot -> Int
acksOf k s = Map.findWithDefault 0 k s.acks
ackErrorsOf k s = Map.findWithDefault 0 k s.ackErrors

totalAcks, totalAckErrors :: ReaderSnapshot -> Int
totalAcks s = sum s.acks
totalAckErrors s = sum s.ackErrors

ackLatencyBuckets :: ReaderSnapshot -> VU.Vector Int
ackLatencyBuckets s = toBuckets s.ackLatency

totalDelivered, totalFetches, totalEmptyFetches, totalFetchErrors :: ReaderSnapshot -> Int
totalDelivered s = sum s.delivered
totalFetches s = sum s.fetches
totalEmptyFetches s = sum s.emptyFetches
totalFetchErrors s = sum s.fetchErrors

fetchLatencyBuckets, lagBuckets :: ReaderSnapshot -> VU.Vector Int
fetchLatencyBuckets s = toBuckets s.fetchLatency
lagBuckets s = toBuckets s.lag

data ConsumeTickState = ConsumeTickState
  { startedAt :: Double,
    lastTickAt :: Double,
    previousSnapshot :: ReaderSnapshot,
    peakRate :: Double,
    peakAckRate :: Double
  }

initialConsumeTickState :: Double -> ConsumeTickState
initialConsumeTickState now = ConsumeTickState now now emptyReaderSnapshot 0 0

data ConsumeReport = ConsumeReport
  { consumeRate :: Double,
    maxConsumeRate :: Double,
    fetchRate :: Double,
    -- | empty fetches / fetches during the last tick
    emptyFetchRatio :: Double,
    fetchErrorRate :: Double,
    lagP50 :: Maybe Double,
    lagP99 :: Maybe Double,
    fetchP50 :: Maybe Double,
    fetchP99 :: Maybe Double,
    -- | upserts per second; 0 / Nothing without acks
    ackRate :: Double,
    maxAckRate :: Double,
    ackErrorRate :: Double,
    ackP50 :: Maybe Double,
    ackP99 :: Maybe Double,
    -- | expected - delivered, clamped at 0: readers may see a push before the
    -- writer has recorded its expected deliveries.
    backlog :: Int,
    readerDelta :: ReaderSnapshot,
    readerTotal :: ReaderSnapshot
  }
  deriving (Eq, Show)

-- | @consumeTick warmup now expectedTotal readerTotal state@.
consumeTick :: Double -> Double -> Int -> ReaderSnapshot -> ConsumeTickState -> (ConsumeReport, ConsumeTickState)
consumeTick warmup now expectedTotal total st =
  let dt = now - st.lastTickAt
      delta = diffReaderSnapshot total st.previousSnapshot
      rate :: Int -> Double
      rate n = if dt > 0 then fromIntegral n / dt else 0
      elapsed = now - st.startedAt
      consumeRate = rate (totalDelivered delta)
      afterWarmup = elapsed > warmup && dt >= minTickInterval
      peak = if afterWarmup then max st.peakRate consumeRate else st.peakRate
      ackRate = rate (totalAcks delta)
      peakAck = if afterWarmup then max st.peakAckRate ackRate else st.peakAckRate
      fetchesInTick = totalFetches delta
      report =
        ConsumeReport
          { consumeRate,
            maxConsumeRate = peak,
            fetchRate = rate fetchesInTick,
            emptyFetchRatio = if fetchesInTick > 0 then fromIntegral (totalEmptyFetches delta) / fromIntegral fetchesInTick else 0,
            fetchErrorRate = rate (totalFetchErrors delta),
            lagP50 = quantileSeconds 0.5 (lagBuckets total),
            lagP99 = quantileSeconds 0.99 (lagBuckets total),
            fetchP50 = quantileSeconds 0.5 (fetchLatencyBuckets total),
            fetchP99 = quantileSeconds 0.99 (fetchLatencyBuckets total),
            ackRate,
            maxAckRate = peakAck,
            ackErrorRate = rate (totalAckErrors delta),
            ackP50 = quantileSeconds 0.5 (ackLatencyBuckets total),
            ackP99 = quantileSeconds 0.99 (ackLatencyBuckets total),
            backlog = max 0 (expectedTotal - totalDelivered total),
            readerDelta = delta,
            readerTotal = total
          }
   in (report, st {lastTickAt = now, previousSnapshot = total, peakRate = peak, peakAckRate = peakAck})
