module FanInPerf.StatsSpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Stats
import FanInPerf.Targets (TargetKind (..))
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "bucketIndex" $ do
    it "maps edges to log2 buckets" $
      map bucketIndex [0, 1, 2, 3, 1023, 1024, maxBound]
        `shouldBe` [0, 0, 1, 1, 9, 10, numBuckets - 1]
    prop "latency is below its bucket's upper bound" $
      forAll (choose (1, 2 ^ (numBuckets - 1 :: Int) - 1 :: Word64)) $ \ns ->
        fromIntegral ns / 1e9 < bucketUpperBoundSeconds (bucketIndex ns)

  describe "quantileSeconds" $ do
    it "is Nothing without samples" $
      quantileSeconds 0.5 (VU.replicate numBuckets 0) `shouldBe` Nothing
    it "returns the upper bound of the bucket holding the quantile" $
      let buckets = VU.generate numBuckets (\b -> if b == 3 then 90 else if b == 10 then 10 else 0)
       in (quantileSeconds 0.5 buckets, quantileSeconds 0.99 buckets)
            `shouldBe` (Just (bucketUpperBoundSeconds 3), Just (bucketUpperBoundSeconds 10))

  describe "WriterStats" $ do
    it "records successes and errors per kind" $ do
      ws <- newWriterStats
      replicateM_ 3 (recordSuccess ws KindTeam 2 1000)
      recordError ws KindUser
      s <- readSnapshot ws
      (pushesOf KindTeam s, targetsOf KindTeam s, errorsOf KindUser s, totalPushes s, totalErrors s)
        `shouldBe` (3, 6, 1, 3, 1)
      latencyBuckets s VU.! bucketIndex 1000 `shouldBe` 3

    it "sums writers that ran concurrently" $ do
      wss <- replicateM 8 newWriterStats
      forConcurrently_ wss $ \ws -> replicateM_ 10000 (recordSuccess ws KindUser 1 500)
      s <- sumSnapshots <$> traverse readSnapshot wss
      totalPushes s `shouldBe` 80000

  describe "tick" $ do
    let mkTotal pushes errs = do
          ws <- newWriterStats
          replicateM_ pushes (recordSuccess ws KindUser 2 1000)
          replicateM_ errs (recordError ws KindUser)
          readSnapshot ws

    it "computes rates from the delta and ignores max during warmup" $ do
      t1 <- mkTotal 100 0
      let (r1, st1) = tick 5 1 t1 (initialTickState 0)
      (r1.pushRate, r1.targetRate, r1.maxPushRate) `shouldBe` (100, 200, 0)
      t2 <- mkTotal 400 0
      let (r2, _) = tick 5 6 t2 st1
      (r2.pushRate, r2.maxPushRate) `shouldBe` (60, 60)

    it "never produces NaN or Infinity for dt = 0 and no traffic" $ do
      let (r, _) = tick 5 0 emptySnapshot (initialTickState 0)
      [r.pushRate, r.targetRate, r.errorRate, r.errorRatio, r.maxPushRate] `shouldBe` [0, 0, 0, 0, 0]
      (r.p50, r.p99) `shouldBe` (Nothing, Nothing)

    it "a tiny final tick counts in totals but does not raise the max rate" $ do
      t1 <- mkTotal 100 0
      let (r1, st1) = tick 5 10 t1 (initialTickState 0)
      r1.maxPushRate `shouldBe` 10
      t2 <- mkTotal 10100 0
      let (r2, _) = tick 5 10.001 t2 st1
      r2.maxPushRate `shouldBe` 10
      totalPushes r2.total `shouldBe` 10100

    it "computes the error ratio over the last tick" $ do
      t <- mkTotal 3 1
      let (r, _) = tick 0 1 t (initialTickState 0)
      (r.errorRate, r.errorRatio) `shouldBe` (1, 0.25)
