-- | Reader-side statistics, same scheme as "FanInPerf.Stats": one 'IORef' per
-- reader client written only by it, summed by the ticker.
module FanInPerf.ConsumeStats
  ( ReaderStats,
    newReaderStats,
    recordFetch,
    recordFetchError,
    recordAck,
    recordAckError,
    recordGateWait,
    ReaderSnapshot,
    emptyReaderSnapshot,
    readReaderSnapshot,
    sumReaderSnapshots,
    sumReaders,
    deliveredSoFar,
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
    gateWaitBuckets,
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
import FanInPerf.Stats (bucketIndex, minTickInterval, quantileSeconds, ratio, toBuckets)
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
    ackLatency :: !(IntMap Int),
    gateWait :: !(IntMap Int)
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
      ackLatency = mempty,
      gateWait = mempty
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

-- | One wait (ns) for a slot of the reader gate before a store call.
recordGateWait :: ReaderStats -> Word64 -> IO ()
recordGateWait stats waitNs =
  modifyReader stats $ \s -> s {gateWait = IntMap.insertWith (+) (bucketIndex waitNs) 1 s.gateWait}

-- | Plain read, see "FanInPerf.Stats": monitoring never makes a reader retry its CAS.
readReaderSnapshot :: ReaderStats -> IO ReaderSnapshot
readReaderSnapshot (ReaderStats r) = readIORef r

-- | The readers' counters added up.
sumReaders :: [ReaderStats] -> IO ReaderSnapshot
sumReaders readerStats = sumReaderSnapshots <$> traverse readReaderSnapshot readerStats

-- | Deliveries so far, summed per reader: unlike 'sumReaders' it does not add
-- up the histograms of all readers, which matters for the polling of the drain
-- and the milestones.
deliveredSoFar :: [ReaderStats] -> IO Int
deliveredSoFar readerStats = sum <$> traverse (fmap totalDelivered . readReaderSnapshot) readerStats

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
          ackLatency = IntMap.unionWith (+) x.ackLatency y.ackLatency,
          gateWait = IntMap.unionWith (+) x.gateWait y.gateWait
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
      ackLatency = IntMap.unionWith (+) new.ackLatency (negate <$> old.ackLatency),
      gateWait = IntMap.unionWith (+) new.gateWait (negate <$> old.gateWait)
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

gateWaitBuckets :: ReaderSnapshot -> VU.Vector Int
gateWaitBuckets s = toBuckets s.gateWait

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
    -- | percentiles of the last tick (the summary uses the totals instead)
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
    -- | waits for a reader gate slot; Nothing without any
    gateWaitP50 :: Maybe Double,
    gateWaitP99 :: Maybe Double,
    -- | expected - delivered, clamped at 0: readers may see a push before the
    -- writer has recorded its expected deliveries.
    backlog :: Int,
    readerDelta :: ReaderSnapshot,
    readerTotal :: ReaderSnapshot
  }
  deriving (Eq, Show)

-- | @consumeTick warmup writing now expectedTotal readerTotal state@. Peaks
-- only count while the writers run (@writing@): the drain afterwards has the
-- readers to itself and would inflate them.
consumeTick :: Double -> Bool -> Double -> Int -> ReaderSnapshot -> ConsumeTickState -> (ConsumeReport, ConsumeTickState)
consumeTick warmup writing now expectedTotal total st =
  let dt = now - st.lastTickAt
      delta = diffReaderSnapshot total st.previousSnapshot
      rate :: Int -> Double
      rate n = ratio n dt
      elapsed = now - st.startedAt
      consumeRate = rate (totalDelivered delta)
      afterWarmup = writing && elapsed > warmup && dt >= minTickInterval
      peak = if afterWarmup then max st.peakRate consumeRate else st.peakRate
      ackRate = rate (totalAcks delta)
      peakAck = if afterWarmup then max st.peakAckRate ackRate else st.peakAckRate
      fetchesInTick = totalFetches delta
      report =
        ConsumeReport
          { consumeRate,
            maxConsumeRate = peak,
            fetchRate = rate fetchesInTick,
            emptyFetchRatio = ratio (totalEmptyFetches delta) fetchesInTick,
            fetchErrorRate = rate (totalFetchErrors delta),
            lagP50 = quantileSeconds 0.5 (lagBuckets delta),
            lagP99 = quantileSeconds 0.99 (lagBuckets delta),
            fetchP50 = quantileSeconds 0.5 (fetchLatencyBuckets delta),
            fetchP99 = quantileSeconds 0.99 (fetchLatencyBuckets delta),
            ackRate,
            maxAckRate = peakAck,
            ackErrorRate = rate (totalAckErrors delta),
            ackP50 = quantileSeconds 0.5 (ackLatencyBuckets delta),
            ackP99 = quantileSeconds 0.99 (ackLatencyBuckets delta),
            gateWaitP50 = quantileSeconds 0.5 (gateWaitBuckets delta),
            gateWaitP99 = quantileSeconds 0.99 (gateWaitBuckets delta),
            backlog = max 0 (expectedTotal - totalDelivered total),
            readerDelta = delta,
            readerTotal = total
          }
   in (report, st {lastTickAt = now, previousSnapshot = total, peakRate = peak, peakAckRate = peakAck})
