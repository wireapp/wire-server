{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Lock-free statistics. Every writer thread owns one 'WriterStats' and is
-- its only writer; the ticker thread reads all of them. Counters live in a
-- pinned, 64-byte aligned 'MutablePrimArray' padded to whole cache lines, so
-- writers never share a cache line and recording never allocates.
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
import Data.Primitive.ByteArray (MutableByteArray (..), newAlignedPinnedByteArray)
import Data.Primitive.PrimArray (MutablePrimArray (..), setPrimArray)
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Targets (TargetKind, allKinds)
import GHC.Exts (Int (I#), RealWorld, atomicReadIntArray#, fetchAddIntArray#)
import GHC.IO (IO (IO))
import Imports

numKinds :: Int
numKinds = length allKinds

-- | Bucket @b@ holds latencies in @[2^b, 2^(b+1))@ ns: 1 ns .. ~18 min.
numBuckets :: Int
numBuckets = 40

slotPushes, slotTargets, slotErrors :: TargetKind -> Int
slotPushes k = fromEnum k
slotTargets k = numKinds + fromEnum k
slotErrors k = 2 * numKinds + fromEnum k

slotBucket :: Int -> Int
slotBucket b = 3 * numKinds + b

numSlots :: Int
numSlots = 3 * numKinds + numBuckets

-- | Whole cache lines (8 Ints each) plus one spare line.
allocatedSlots :: Int
allocatedSlots = (numSlots `div` 8 + 2) * 8

newtype WriterStats = WriterStats (MutablePrimArray RealWorld Int)

newWriterStats :: IO WriterStats
newWriterStats = do
  MutableByteArray mba <- newAlignedPinnedByteArray (allocatedSlots * 8) 64
  let arr = MutablePrimArray mba
  setPrimArray arr 0 allocatedSlots 0
  pure (WriterStats arr)

-- primitive-0.9 only offers atomics on PrimVar, so use the primops directly.
fetchAddSlot :: MutablePrimArray RealWorld Int -> Int -> Int -> IO ()
fetchAddSlot (MutablePrimArray mba) (I# i) (I# n) =
  IO $ \s -> case fetchAddIntArray# mba i n s of
    (# s', _ #) -> (# s', () #)

atomicReadSlot :: MutablePrimArray RealWorld Int -> Int -> IO Int
atomicReadSlot (MutablePrimArray mba) (I# i) =
  IO $ \s -> case atomicReadIntArray# mba i s of
    (# s', r #) -> (# s', I# r #)

recordSuccess :: WriterStats -> TargetKind -> Int -> Word64 -> IO ()
recordSuccess (WriterStats a) k targets latencyNs = do
  fetchAddSlot a (slotPushes k) 1
  fetchAddSlot a (slotTargets k) targets
  fetchAddSlot a (slotBucket (bucketIndex latencyNs)) 1

recordError :: WriterStats -> TargetKind -> IO ()
recordError (WriterStats a) k = fetchAddSlot a (slotErrors k) 1

-- | Cells are read one by one; a snapshot may be off by one push between
-- cells, which is irrelevant at one-second granularity.
newtype Snapshot = Snapshot (VU.Vector Int)
  deriving (Eq, Show)

emptySnapshot :: Snapshot
emptySnapshot = Snapshot (VU.replicate numSlots 0)

readSnapshot :: WriterStats -> IO Snapshot
readSnapshot (WriterStats a) = Snapshot <$> VU.generateM numSlots (atomicReadSlot a)

sumSnapshots :: [Snapshot] -> Snapshot
sumSnapshots = foldl' (\(Snapshot x) (Snapshot y) -> Snapshot (VU.zipWith (+) x y)) emptySnapshot

diffSnapshot :: Snapshot -> Snapshot -> Snapshot
diffSnapshot (Snapshot new) (Snapshot old) = Snapshot (VU.zipWith (-) new old)

slot :: Int -> Snapshot -> Int
slot i (Snapshot v) = v VU.! i

pushesOf, targetsOf, errorsOf :: TargetKind -> Snapshot -> Int
pushesOf = slot . slotPushes
targetsOf = slot . slotTargets
errorsOf = slot . slotErrors

totalPushes, totalTargets, totalErrors :: Snapshot -> Int
totalPushes s = sum [pushesOf k s | k <- allKinds]
totalTargets s = sum [targetsOf k s | k <- allKinds]
totalErrors s = sum [errorsOf k s | k <- allKinds]

latencyBuckets :: Snapshot -> VU.Vector Int
latencyBuckets (Snapshot v) = VU.slice (slotBucket 0) numBuckets v

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
