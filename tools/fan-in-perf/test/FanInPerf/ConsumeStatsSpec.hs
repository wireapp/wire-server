module FanInPerf.ConsumeStatsSpec (spec) where

import Data.Vector.Unboxed qualified as VU
import FanInPerf.ConsumeStats
import FanInPerf.Stats (bucketIndex, quantileSeconds)
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

    it "records, sums and diffs acks and ack errors without counting fetches" $ do
      a <- newReaderStats
      b <- newReaderStats
      recordAck a KindTeam 1000
      s0 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      recordAck a KindTeam 1000
      recordAck b KindEpoch 2000
      recordAckError b KindEpoch
      s1 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      let d = diffReaderSnapshot s1 s0
      (acksOf KindTeam s1, acksOf KindTeam d, acksOf KindEpoch d, ackErrorsOf KindEpoch d) `shouldBe` (2, 1, 1, 1)
      (totalAcks s1, totalAckErrors s1, VU.sum (ackLatencyBuckets d), totalFetches s1) `shouldBe` (3, 1, 2, 0)

    it "records, sums and diffs gate waits without counting fetches or acks" $ do
      a <- newReaderStats
      b <- newReaderStats
      recordGateWait a 1000
      s0 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      recordGateWait a 1000
      recordGateWait b 50_000_000
      s1 <- sumReaderSnapshots <$> traverse readReaderSnapshot [a, b]
      let d = diffReaderSnapshot s1 s0
      (VU.sum (gateWaitBuckets s1), VU.sum (gateWaitBuckets d)) `shouldBe` (3, 2)
      gateWaitBuckets s1 VU.! bucketIndex 1000 `shouldBe` 2
      gateWaitBuckets d VU.! bucketIndex 50_000_000 `shouldBe` 1
      (totalFetches s1, totalAcks s1) `shouldBe` (0, 0)

  describe "consumeTick" $ do
    it "computes gate wait quantiles, Nothing without waits" $ do
      rs <- newReaderStats
      replicateM_ 99 (recordGateWait rs 1000)
      recordGateWait rs 50_000_000
      t <- readReaderSnapshot rs
      let (r, _) = consumeTick 0 True 1 0 t (initialConsumeTickState 0)
      map (maybe False (< 0.001)) [r.gateWaitP50, r.gateWaitP99] `shouldBe` [True, True]
      replicateM_ 10 (recordGateWait rs 50_000_000)
      t2 <- readReaderSnapshot rs
      let (r2, _) = consumeTick 0 True 2 0 t2 (initialConsumeTickState 0)
      r2.gateWaitP99 `shouldSatisfy` maybe False (> 0.03)
      let (r0, _) = consumeTick 0 True 0 0 emptyReaderSnapshot (initialConsumeTickState 0)
      (r0.gateWaitP50, r0.gateWaitP99) `shouldBe` (Nothing, Nothing)

    it "computes rates from the delta and ignores the max during warmup" $ do
      t1 <- snapshotAfter 10 0 -- 30 delivered
      let (r1, st1) = consumeTick 5 True 1 100 t1 (initialConsumeTickState 0)
      (r1.consumeRate, r1.maxConsumeRate, r1.fetchRate) `shouldBe` (30, 0, 10)
      t2 <- snapshotAfter 30 0 -- 90 delivered
      let (r2, _) = consumeTick 5 True 6 100 t2 st1
      (r2.consumeRate, r2.maxConsumeRate) `shouldBe` (12, 12)

    it "reports percentiles of the last tick only, not of the whole run" $ do
      rs <- newReaderStats
      replicateM_ 100 (recordGateWait rs 50_000_000)
      t1 <- readReaderSnapshot rs
      let (r1, st1) = consumeTick 0 True 1 0 t1 (initialConsumeTickState 0)
      r1.gateWaitP50 `shouldSatisfy` maybe False (> 0.03)
      replicateM_ 100 (recordGateWait rs 1000)
      t2 <- readReaderSnapshot rs
      let (r2, _) = consumeTick 0 True 2 0 t2 st1
      r2.gateWaitP99 `shouldSatisfy` maybe False (< 0.001)
      quantileSeconds 0.99 (gateWaitBuckets r2.readerTotal) `shouldSatisfy` maybe False (> 0.03)

    it "does not raise the peaks once the writers stopped (drain)" $ do
      t1 <- snapshotAfter 10 0
      let (r1, st1) = consumeTick 0 True 1 100 t1 (initialConsumeTickState 0)
      t2 <- snapshotAfter 1000 0
      let (r2, _) = consumeTick 0 False 2 100 t2 st1
      (r2.consumeRate > r1.consumeRate, r2.maxConsumeRate) `shouldBe` (True, r1.maxConsumeRate)

    it "computes the empty-fetch ratio over the last tick and survives no traffic" $ do
      t <- snapshotAfter 1 3
      let (r, _) = consumeTick 0 True 1 10 t (initialConsumeTickState 0)
      r.emptyFetchRatio `shouldBe` 0.75
      let (r0, _) = consumeTick 0 True 0 0 emptyReaderSnapshot (initialConsumeTickState 0)
      [r0.consumeRate, r0.fetchRate, r0.emptyFetchRatio, r0.fetchErrorRate, r0.maxConsumeRate] `shouldBe` [0, 0, 0, 0, 0]
      (r0.lagP50, r0.lagP99, r0.fetchP50, r0.fetchP99) `shouldBe` (Nothing, Nothing, Nothing, Nothing)

    it "computes the ack rate, its max after warmup and ack latency quantiles" $ do
      rs <- newReaderStats
      replicateM_ 20 (recordAck rs KindUser 1000)
      recordAckError rs KindUser
      t1 <- readReaderSnapshot rs
      let (r1, st1) = consumeTick 5 True 1 0 t1 (initialConsumeTickState 0)
      (r1.ackRate, r1.maxAckRate, r1.ackErrorRate) `shouldBe` (20, 0, 1)
      isJust r1.ackP99 `shouldBe` True
      replicateM_ 60 (recordAck rs KindUser 1000)
      t2 <- readReaderSnapshot rs
      let (r2, _) = consumeTick 5 True 6 0 t2 st1
      (r2.ackRate, r2.maxAckRate) `shouldBe` (12, 12)
      let (r0, _) = consumeTick 0 True 0 0 emptyReaderSnapshot (initialConsumeTickState 0)
      (r0.ackRate, r0.ackP50, r0.ackP99) `shouldBe` (0, Nothing, Nothing)

    it "reports backlog = expected - delivered, never negative" $ do
      t <- snapshotAfter 10 0 -- 30 delivered
      fst (consumeTick 0 True 1 100 t (initialConsumeTickState 0)) `shouldSatisfy` ((== 70) . (.backlog))
      fst (consumeTick 0 True 1 10 t (initialConsumeTickState 0)) `shouldSatisfy` ((== 0) . (.backlog))

    it "a tiny final tick counts in totals but does not raise the max rate" $ do
      t1 <- snapshotAfter 10 0
      let (r1, st1) = consumeTick 5 True 10 100 t1 (initialConsumeTickState 0)
      r1.maxConsumeRate `shouldBe` 3
      t2 <- snapshotAfter 1000 0
      let (r2, _) = consumeTick 5 True 10.001 100 t2 st1
      r2.maxConsumeRate `shouldBe` 3
      totalDelivered r2.readerTotal `shouldBe` 3000
