module FanInPerf.SummarySpec (spec) where

import Data.Aeson (Value, decode, encode, toJSON)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import FanInPerf.ConsumeStats
import FanInPerf.Options (ReaderMode (..))
import FanInPerf.Stats
import FanInPerf.Summary
import FanInPerf.Targets (TargetKind (..))
import Imports
import Test.Hspec

report :: TickReport
report =
  TickReport
    { elapsed = 10,
      pushRate = 0,
      maxPushRate = 0,
      targetRate = 0,
      errorRate = 0,
      errorRatio = 0,
      p50 = Nothing,
      p99 = Nothing,
      delta = emptySnapshot,
      total = emptySnapshot
    }

creport :: ConsumeReport
creport =
  ConsumeReport
    { consumeRate = 0,
      maxConsumeRate = 0,
      fetchRate = 0,
      emptyFetchRatio = 0,
      fetchErrorRate = 0,
      lagP50 = Nothing,
      lagP99 = Nothing,
      fetchP50 = Nothing,
      fetchP99 = Nothing,
      ackRate = 0,
      maxAckRate = 0,
      ackErrorRate = 0,
      ackP50 = Nothing,
      ackP99 = Nothing,
      gateWaitP50 = Nothing,
      gateWaitP99 = Nothing,
      backlog = 0,
      readerDelta = emptyReaderSnapshot,
      readerTotal = emptyReaderSnapshot
    }

field :: [Text] -> Value -> Maybe Value
field [] v = Just v
field (k : ks) (A.Object o) = KM.lookup (K.fromText k) o >>= field ks
field _ _ = Nothing

spec :: Spec
spec = do
  describe "produceSummary as JSON" $ do
    it "has numbers in seconds and the pushes section only" $ do
      ws <- newWriterStats
      replicateM_ 5 (recordSuccess ws KindUser 2 0 3_000_000)
      recordError ws KindUser
      total <- readSnapshot ws
      let v = toJSON (produceSummary report {total, maxPushRate = 120})
      field ["command"] v `shouldBe` Just "produce"
      field ["duration"] v `shouldBe` num 10
      field ["pushes", "pushes"] v `shouldBe` num 5
      field ["pushes", "targets"] v `shouldBe` num 10
      field ["pushes", "errors"] v `shouldBe` num 1
      field ["pushes", "rate_avg"] v `shouldBe` num 0.5
      field ["pushes", "rate_max"] v `shouldBe` num 120
      field ["pushes", "latency", "p99"] v `shouldSatisfy` maybe False (/= A.Null)
      field ["deliveries"] v `shouldBe` Just A.Null
      field ["acks"] v `shouldBe` Just A.Null
    it "has null for a maximum never measured and for percentiles without data" $ do
      let v = toJSON (produceSummary report)
      field ["pushes", "rate_max"] v `shouldBe` Just A.Null
      field ["pushes", "latency", "p50"] v `shouldBe` Just A.Null
    it "round-trips through JSON text" $
      (decode (encode (produceSummary report)) :: Maybe Value) `shouldSatisfy` isJust

  describe "consumeSummary as JSON" $ do
    let phase = WriterPhase {duration = 5, delivered = 100, acks = 50}
        summary mode d = toJSON (consumeSummary mode 150 phase d report creport)
    it "has deliveries, backlog at writer stop and the drain" $ do
      let v = summary NoAcks (DrainComplete 1.5)
      field ["command"] v `shouldBe` Just "consume"
      field ["duration"] v `shouldBe` num 5
      field ["deliveries", "expected"] v `shouldBe` num 150
      field ["deliveries", "missing"] v `shouldBe` num 150
      field ["deliveries", "duplicates"] v `shouldBe` num 0
      field ["deliveries", "rate_avg"] v `shouldBe` num 20
      field ["deliveries", "backlog_at_writer_stop"] v `shouldBe` num 50
      field ["drain", "state"] v `shouldBe` Just "complete"
      field ["drain", "seconds"] v `shouldBe` num 1.5
      field ["acks"] v `shouldBe` Just A.Null
      field ["end"] v `shouldBe` Just A.Null
    it "names every drain state" $
      map (field ["drain", "state"] . summary NoAcks) [DrainComplete 1, DrainStalled 1, DrainTimedOut 1, DrainInterrupted]
        `shouldBe` map Just ["complete", "stalled", "timed_out", "interrupted"]
    it "has no drain duration when interrupted" $
      field ["drain", "seconds"] (summary NoAcks DrainInterrupted) `shouldBe` Just A.Null
    it "has the acks section in ack mode and labels the command" $ do
      let v = summary (Acks 1) (DrainComplete 1)
      field ["command"] v `shouldBe` Just "consume-ack"
      field ["acks", "acks"] v `shouldBe` num 0
      field ["acks", "rate_avg"] v `shouldBe` num 10

  describe "consumeOnlySummary as JSON" $ do
    it "has the end, the milestones in seconds and no pushes" $ do
      let v = toJSON ((consumeOnlySummary NoAcks 5 2 ConsumeFinished [(50, Just 0.5), (99, Nothing)] creport) {rowsTotal = Just 7})
      field ["command"] v `shouldBe` Just "consume-only"
      field ["end"] v `shouldBe` Just "finished"
      field ["rows_total"] v `shouldBe` num 7
      field ["pushes"] v `shouldBe` Just A.Null
      field ["drain"] v `shouldBe` Just A.Null
      field ["deliveries", "milestones"] v
        `shouldBe` (decode "[{\"percent\":50,\"seconds\":0.5},{\"percent\":99,\"seconds\":null}]" :: Maybe Value)
    it "names every end" $
      map (\e -> field ["end"] (toJSON (consumeOnlySummary NoAcks 0 1 e [] creport))) [ConsumeFinished, ConsumeTimedOut, ConsumeInterrupted]
        `shouldBe` map Just ["finished", "timed_out", "interrupted"]
    it "labels ack mode" $
      field ["command"] (toJSON (consumeOnlySummary (Acks 1) 0 1 ConsumeFinished [] creport)) `shouldBe` Just "consume-only-ack"

num :: Double -> Maybe Value
num = Just . toJSON
