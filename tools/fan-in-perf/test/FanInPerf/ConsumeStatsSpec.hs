module FanInPerf.ConsumeStatsSpec (spec) where

import Data.Vector.Unboxed qualified as VU
import FanInPerf.ConsumeStats
import FanInPerf.Stats (bucketIndex)
import FanInPerf.Targets (TargetKind (..))
import Imports
import Test.Hspec

snapshotAfter :: Int -> Int -> IO ReaderSnapshot
snapshotAfter fetchesWith emptyFetches = do
  rs <- newReaderStats
  replicateM_ fetchesWith (recordFetch rs KindTeam 1000 [500, 500, 500])
  replicateM_ emptyFetches (recordFetch rs KindTeam 1000 [])
  readReaderSnapshot rs

spec :: Spec
spec = do
  describe "ReaderStats" $ do
    it "records fetches, deliveries, empty fetches, errors and histograms" $ do
      rs <- newReaderStats
      recordFetch rs KindUser 1000 [500, 500]
      recordFetch rs KindUser 2000 []
      recordFetchError rs KindEpoch
      s <- readReaderSnapshot rs
      (deliveredOf KindUser s, fetchesOf KindUser s, emptyFetchesOf KindUser s, fetchErrorsOf KindEpoch s)
        `shouldBe` (2, 2, 1, 1)
      (totalDelivered s, totalFetches s, totalEmptyFetches s, totalFetchErrors s) `shouldBe` (2, 2, 1, 1)
      VU.sum (fetchLatencyBuckets s) `shouldBe` 2
      lagBuckets s VU.! bucketIndex 500 `shouldBe` 2

    it "sums readers and diffs snapshots, including kinds absent from one side" $ do
      a <- newReaderStats
      b <- newReaderStats
      recordFetch a KindUser 1000 [1]
      s0 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      recordFetch a KindUser 1000 [1, 1]
      recordFetch b KindTeam 1000 [1]
      s1 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      let d = diffReaderSnapshot s1 s0
      (deliveredOf KindUser d, deliveredOf KindTeam d, totalFetches d, VU.sum (lagBuckets d)) `shouldBe` (2, 1, 2, 3)

  describe "consumeTick" $ do
    it "computes rates from the delta and ignores the max during warmup" $ do
      t1 <- snapshotAfter 10 0 -- 30 delivered
      let (r1, st1) = consumeTick 5 1 100 t1 (initialConsumeTickState 0)
      (r1.consumeRate, r1.maxConsumeRate, r1.fetchRate) `shouldBe` (30, 0, 10)
      t2 <- snapshotAfter 30 0 -- 90 delivered
      let (r2, _) = consumeTick 5 6 100 t2 st1
      (r2.consumeRate, r2.maxConsumeRate) `shouldBe` (12, 12)

    it "computes the empty-fetch ratio over the last tick and survives no traffic" $ do
      t <- snapshotAfter 1 3
      let (r, _) = consumeTick 0 1 10 t (initialConsumeTickState 0)
      r.emptyFetchRatio `shouldBe` 0.75
      let (r0, _) = consumeTick 0 0 0 emptyReaderSnapshot (initialConsumeTickState 0)
      [r0.consumeRate, r0.fetchRate, r0.emptyFetchRatio, r0.fetchErrorRate, r0.maxConsumeRate] `shouldBe` [0, 0, 0, 0, 0]
      (r0.lagP50, r0.lagP99, r0.fetchP50, r0.fetchP99) `shouldBe` (Nothing, Nothing, Nothing, Nothing)

    it "reports backlog = expected - delivered, never negative" $ do
      t <- snapshotAfter 10 0 -- 30 delivered
      fst (consumeTick 0 1 100 t (initialConsumeTickState 0)) `shouldSatisfy` ((== 70) . (.backlog))
      fst (consumeTick 0 1 10 t (initialConsumeTickState 0)) `shouldSatisfy` ((== 0) . (.backlog))

    it "a tiny final tick counts in totals but does not raise the max rate" $ do
      t1 <- snapshotAfter 10 0
      let (r1, st1) = consumeTick 5 10 100 t1 (initialConsumeTickState 0)
      r1.maxConsumeRate `shouldBe` 3
      t2 <- snapshotAfter 1000 0
      let (r2, _) = consumeTick 5 10.001 100 t2 st1
      r2.maxConsumeRate `shouldBe` 3
      totalDelivered r2.readerTotal `shouldBe` 3000
