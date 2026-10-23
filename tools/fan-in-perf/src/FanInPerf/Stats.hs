-- | Statistics without contention between writers. Every writer thread owns one
-- 'WriterStats' (an 'IORef' holding a pure 'Snapshot') and is its only writer;
-- the ticker thread reads all of them and sums the snapshots.
--
-- Writers publish with 'atomicModifyIORef'' (a barrier: the snapshot is fully
-- visible to other cores once published). The ticker reads with a plain
-- 'readIORef': it never writes, so it can never make a writer's compare-and-swap
-- retry. It may see a snapshot a few microseconds old, which is fine for
-- monitoring: writer speed has priority over measurement.
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
    totalExpected,
    latencyBuckets,
    toBuckets,
    numBuckets,
    bucketIndex,
    bucketUpperBoundSeconds,
    bucketMidSeconds,
    quantileSeconds,
    ratio,
    TickState (..),
    initialTickState,
    TickReport (..),
    tick,
    minTickInterval,
  )
where

import Data.Bits (countLeadingZeros, shiftR)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Targets (TargetKind)
import Imports

-- | Log-linear histogram: bucket 0 holds everything below 4 ns (not measurable
-- anyways). From 4 ns on, every power-of-two range @[2^o, 2^(o+1))@ (@o >= 2@)
-- is split into 4 equally wide buckets:
--
-- * @o@ = floor (log2 ns), i.e. the range the value is in.
-- * @s@ = 0..3, the sub-bucket within that range. Each is @2^(o-2)@ ns wide, so
--   @s = (ns `shiftR` (o - 2)) - 4@ (the two bits below the leading one).
--
-- Bucket @4 * o + s - 7@ holds @[(4 + s) * 2^(o-2), (5 + s) * 2^(o-2))@ ns. The
-- @- 7@ makes the indices dense: the first range (@o = 2@, @s = 0@) is bucket 1.
-- Within a bucket the upper bound is at most 25% above the value (a plain
-- power-of-two scheme: up to 100%).
--
-- Examples:
--
-- * 3 ns    -> bucket 0, @[0, 4)@
-- * 4 ns    -> @o = 2, s = 0@, bucket 1, @[4, 5)@
-- * 7 ns    -> @o = 2, s = 3@, bucket 4, @[7, 8)@
-- * 8 ns    -> @o = 3, s = 0@ (width 2), bucket 5, @[8, 10)@
-- * 11 ns   -> @o = 3, s = 1@, bucket 6, @[10, 12)@
-- * 1000 ns -> @o = 9, s = 3@ (width 128), bucket 32, @[896, 1024)@
--
-- Range: 4 ns .. ~18 min (2^40 ns); larger values go to the last bucket.
numBuckets :: Int
numBuckets = 153

-- | Counters per target kind (absent = 0) and a sparse latency histogram
-- (bucket index -> count, absent = 0).
data Snapshot = Snapshot
  { pushes :: !(Map TargetKind Int),
    targets :: !(Map TargetKind Int),
    errors :: !(Map TargetKind Int),
    latency :: !(IntMap Int),
    expected :: !Int
  }
  deriving (Eq, Show)

newtype WriterStats = WriterStats (IORef Snapshot)

emptySnapshot :: Snapshot
emptySnapshot = Snapshot mempty mempty mempty mempty 0

newWriterStats :: IO WriterStats
newWriterStats = WriterStats <$> newIORef emptySnapshot

-- | Atomic (memory barrier sensitive) update; the strict 'Snapshot' is fully
-- evaluated when published, so no thunks build up.
modifyStats :: WriterStats -> (Snapshot -> Snapshot) -> IO ()
modifyStats (WriterStats r) f = atomicModifyIORef' r (\s -> (f s, ()))

bump :: TargetKind -> Int -> Map TargetKind Int -> Map TargetKind Int
bump k n = Map.insertWith (+) k n

-- | @recordSuccess stats kind targets expected latencyNs@: one push with its
-- target count and the notification deliveries readers should see because of
-- it (consume mode, experiments B/C; 0 in produce).
recordSuccess :: WriterStats -> TargetKind -> Int -> Int -> Word64 -> IO ()
recordSuccess stats k n expected latencyNs =
  modifyStats stats $ \s ->
    s
      { pushes = bump k 1 s.pushes,
        targets = bump k n s.targets,
        latency = IntMap.insertWith (+) (bucketIndex latencyNs) 1 s.latency,
        expected = s.expected + expected
      }

recordError :: WriterStats -> TargetKind -> IO ()
recordError stats k = modifyStats stats $ \s -> s {errors = bump k 1 s.errors}

-- | A plain read of the immutable snapshot (see the module header). After the
-- writer threads ended, it sees their last write.
readSnapshot :: WriterStats -> IO Snapshot
readSnapshot (WriterStats r) = readIORef r

sumSnapshots :: [Snapshot] -> Snapshot
sumSnapshots = foldl' addSnapshot emptySnapshot
  where
    addSnapshot x y =
      Snapshot
        { pushes = Map.unionWith (+) x.pushes y.pushes,
          targets = Map.unionWith (+) x.targets y.targets,
          errors = Map.unionWith (+) x.errors y.errors,
          latency = IntMap.unionWith (+) x.latency y.latency,
          expected = x.expected + y.expected
        }

diffSnapshot :: Snapshot -> Snapshot -> Snapshot
diffSnapshot new old =
  Snapshot
    { pushes = diffMap new.pushes old.pushes,
      targets = diffMap new.targets old.targets,
      errors = diffMap new.errors old.errors,
      latency = IntMap.unionWith (+) new.latency (negate <$> old.latency),
      expected = new.expected - old.expected
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

totalExpected :: Snapshot -> Int
totalExpected s = s.expected

latencyBuckets :: Snapshot -> VU.Vector Int
latencyBuckets s = toBuckets s.latency

toBuckets :: IntMap Int -> VU.Vector Int
toBuckets m = VU.accum (+) (VU.replicate numBuckets 0) (IntMap.toList m)

bucketIndex :: Word64 -> Int
bucketIndex ns
  | ns < 4 = 0
  | otherwise = min (numBuckets - 1) (4 * o + fromIntegral (ns `shiftR` (o - 2)) - 11)
  where
    o = 63 - countLeadingZeros ns

-- | Lower (inclusive) and upper (exclusive) bound of bucket @b@ in ns.
bucketBoundsNs :: Int -> (Double, Double)
bucketBoundsNs b
  | b == 0 = (0, 4)
  | otherwise = (fromIntegral (4 + s) * 2 ^^ (o - 2), fromIntegral (5 + s) * 2 ^^ (o - 2))
  where
    (o, s) = (b + 7) `divMod` 4

bucketUpperBoundSeconds :: Int -> Double
bucketUpperBoundSeconds b = snd (bucketBoundsNs b) / 1e9

-- | Middle of the bucket; the estimate of a value in it.
bucketMidSeconds :: Int -> Double
bucketMidSeconds b = let (lo, hi) = bucketBoundsNs b in (lo + hi) / 2e9

-- | Upper bound of the bucket containing quantile @q@.
quantileSeconds :: Double -> VU.Vector Int -> Maybe Double
quantileSeconds q buckets
  | total <= 0 = Nothing
  | otherwise = bucketUpperBoundSeconds <$> VU.findIndex (>= threshold) cumulative
  where
    cumulative = VU.postscanl' (+) 0 buckets
    total = VU.sum buckets
    threshold = max 1 (ceiling (q * fromIntegral total))

-- | @n / d@, 0 when @d@ is not positive (nothing measured yet).
ratio :: (Real a, Real b) => a -> b -> Double
ratio n d = if d > 0 then realToFrac n / realToFrac d else 0

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
    -- | latency percentiles of the pushes of the last tick ('Nothing' without any)
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
      rate n = ratio n dt
      pushes = totalPushes delta
      errors = totalErrors delta
      pushRate = rate pushes
      elapsed = now - st.startTime
      maxPushRate = if elapsed > warmup && dt >= minTickInterval then max st.maxRateSoFar pushRate else st.maxRateSoFar
      errorRatio = ratio errors (pushes + errors)
      latency = latencyBuckets delta
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
