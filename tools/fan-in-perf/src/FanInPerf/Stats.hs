-- | Statistics without contention between writers. Every writer thread owns one
-- 'WriterStats' (an 'IORef' holding a pure 'Snapshot') and is its only writer;
-- the ticker thread reads all of them and sums the snapshots.
--
-- Both sides use the atomic 'IORef' operations ('atomicModifyIORef'') on
-- purpose: they act as memory barriers, so a snapshot published by a writer is
-- fully visible to the ticker on other cores. Plain 'readIORef' / 'writeIORef'
-- give no such ordering guarantee. The cost is irrelevant here (uncontended;
-- the ticker reads once per second).
module FanInPerf.Stats
  ( WriterStats,
    newWriterStats,
    recordSuccess,
    recordError,
    Snapshot,
    emptySnapshot,
    readSnapshot,
    sumSnapshots,
    diffSnapshot,
    pushesOf,
    targetsOf,
    errorsOf,
    totalPushes,
    totalTargets,
    totalErrors,
    latencyBuckets,
    numBuckets,
    bucketIndex,
    bucketUpperBoundSeconds,
    quantileSeconds,
    TickState (..),
    initialTickState,
    TickReport (..),
    tick,
  )
where

import Data.Bits (countLeadingZeros)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Targets (TargetKind)
import Imports

-- | Bucket @b@ holds latencies in @[2^b, 2^(b+1))@ ns: 1 ns .. ~18 min.
numBuckets :: Int
numBuckets = 40

-- | Counters per target kind (absent = 0) and a latency histogram.
data Snapshot = Snapshot
  { pushes :: !(Map TargetKind Int),
    targets :: !(Map TargetKind Int),
    errors :: !(Map TargetKind Int),
    latency :: !(VU.Vector Int)
  }
  deriving (Eq, Show)

newtype WriterStats = WriterStats (IORef Snapshot)

emptySnapshot :: Snapshot
emptySnapshot = Snapshot mempty mempty mempty (VU.replicate numBuckets 0)

newWriterStats :: IO WriterStats
newWriterStats = WriterStats <$> newIORef emptySnapshot

-- | Atomic (memory barrier sensitive) update; the strict 'Snapshot' is fully
-- evaluated when published, so no thunks build up.
modifyStats :: WriterStats -> (Snapshot -> Snapshot) -> IO ()
modifyStats (WriterStats r) f = atomicModifyIORef' r (\s -> (f s, ()))

bump :: TargetKind -> Int -> Map TargetKind Int -> Map TargetKind Int
bump k n = Map.insertWith (+) k n

recordSuccess :: WriterStats -> TargetKind -> Int -> Word64 -> IO ()
recordSuccess stats k n latencyNs =
  modifyStats stats $ \s ->
    s
      { pushes = bump k 1 s.pushes,
        targets = bump k n s.targets,
        latency = VU.accum (+) s.latency [(bucketIndex latencyNs, 1)]
      }

recordError :: WriterStats -> TargetKind -> IO ()
recordError stats k = modifyStats stats $ \s -> s {errors = bump k 1 s.errors}

-- | An identity atomic read-modify-write rather than 'readIORef': it is a full
-- barrier, so the ticker never observes a stale snapshot from another core.
-- (Lock-free, but it can make a concurrent writer's CAS retry once.)
readSnapshot :: WriterStats -> IO Snapshot
readSnapshot (WriterStats r) = atomicModifyIORef' r (\s -> (s, s))

sumSnapshots :: [Snapshot] -> Snapshot
sumSnapshots = foldl' addSnapshot emptySnapshot
  where
    addSnapshot x y =
      Snapshot
        { pushes = Map.unionWith (+) x.pushes y.pushes,
          targets = Map.unionWith (+) x.targets y.targets,
          errors = Map.unionWith (+) x.errors y.errors,
          latency = VU.zipWith (+) x.latency y.latency
        }

diffSnapshot :: Snapshot -> Snapshot -> Snapshot
diffSnapshot new old =
  Snapshot
    { pushes = diffMap new.pushes old.pushes,
      targets = diffMap new.targets old.targets,
      errors = diffMap new.errors old.errors,
      latency = VU.zipWith (-) new.latency old.latency
    }
  where
    -- absent key = 0, so a key in only one of the maps still diffs correctly
    diffMap new' old' = Map.unionWith (+) new' (negate <$> old')

pushesOf, targetsOf, errorsOf :: TargetKind -> Snapshot -> Int
pushesOf k s = Map.findWithDefault 0 k s.pushes
targetsOf k s = Map.findWithDefault 0 k s.targets
errorsOf k s = Map.findWithDefault 0 k s.errors

totalPushes, totalTargets, totalErrors :: Snapshot -> Int
totalPushes s = sum s.pushes
totalTargets s = sum s.targets
totalErrors s = sum s.errors

latencyBuckets :: Snapshot -> VU.Vector Int
latencyBuckets s = s.latency

bucketIndex :: Word64 -> Int
bucketIndex ns = min (numBuckets - 1) (63 - countLeadingZeros (max 1 ns))

bucketUpperBoundSeconds :: Int -> Double
bucketUpperBoundSeconds b = 2 ^^ (b + 1) / 1e9

-- | Upper bound of the bucket containing quantile @q@.
quantileSeconds :: Double -> VU.Vector Int -> Maybe Double
quantileSeconds q buckets
  | total <= 0 = Nothing
  | otherwise = bucketUpperBoundSeconds <$> VU.findIndex (>= threshold) cumulative
  where
    cumulative = VU.postscanl' (+) 0 buckets
    total = VU.sum buckets
    threshold = max 1 (ceiling (q * fromIntegral total))

data TickState = TickState
  { startTime :: Double,
    lastTime :: Double,
    previous :: Snapshot,
    maxRateSoFar :: Double
  }

initialTickState :: Double -> TickState
initialTickState now = TickState now now emptySnapshot 0

data TickReport = TickReport
  { elapsed :: Double,
    pushRate :: Double,
    maxPushRate :: Double,
    targetRate :: Double,
    errorRate :: Double,
    errorRatio :: Double,
    p50 :: Maybe Double,
    p99 :: Maybe Double,
    delta :: Snapshot,
    total :: Snapshot
  }
  deriving (Eq, Show)

-- | Ticks shorter than this (e.g. the final one after the ticker stopped)
-- give noisy rates and must not raise the max rate.
minTickInterval :: Double
minTickInterval = 0.5

tick :: Double -> Double -> Snapshot -> TickState -> (TickReport, TickState)
tick warmup now total st =
  let dt = now - st.lastTime
      delta = diffSnapshot total st.previous
      rate :: Int -> Double
      rate n = if dt > 0 then fromIntegral n / dt else 0
      pushes = totalPushes delta
      errors = totalErrors delta
      pushRate = rate pushes
      elapsed = now - st.startTime
      maxPushRate = if elapsed > warmup && dt >= minTickInterval then max st.maxRateSoFar pushRate else st.maxRateSoFar
      errorRatio = if pushes + errors > 0 then fromIntegral errors / fromIntegral (pushes + errors) else 0
      latency = latencyBuckets total
      report =
        TickReport
          { elapsed,
            pushRate,
            maxPushRate,
            targetRate = rate (totalTargets delta),
            errorRate = rate errors,
            errorRatio,
            p50 = quantileSeconds 0.5 latency,
            p99 = quantileSeconds 0.99 latency,
            delta,
            total
          }
   in (report, st {lastTime = now, previous = total, maxRateSoFar = maxPushRate})
