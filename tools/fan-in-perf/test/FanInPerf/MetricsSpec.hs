module FanInPerf.MetricsSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Vector.Unboxed qualified as VU
import FanInPerf.ConsumeStats
import FanInPerf.Metrics
import FanInPerf.Stats
import FanInPerf.Targets (TargetKind (..))
import Imports
import Network.HTTP.Types (Status, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (..))
import Prometheus qualified as P
import Test.Hspec

statusFor :: [Text] -> IO (Maybe Status)
statusFor path = do
  ref <- newIORef Nothing
  _ <-
    metricsApp
      Wai.defaultRequest {Wai.pathInfo = path}
      (\r -> writeIORef ref (Just (Wai.responseStatus r)) >> pure ResponseReceived)
  readIORef ref

spec :: Spec
spec = do
  describe "latencySampleGroup" $
    it "emits cumulative buckets, +Inf, _sum and _count" $ do
      let buckets = VU.generate numBuckets (\b -> if b == 0 then 2 else if b == 2 then 3 else 0)
          P.SampleGroup _ ty samples = latencySampleGroup "produce" buckets
          values name = [v | P.Sample n _ v <- samples, n == name]
          leValues = [(lookup "le" ls, v) | P.Sample n ls v <- samples, n == "fanin_perf_push_duration_seconds_bucket"]
      case ty of
        P.HistogramType -> pure ()
        _ -> expectationFailure "not a histogram"
      take 3 (map snd leValues) `shouldBe` ["2", "2", "5"]
      last leValues `shouldBe` (Just "+Inf", "5")
      length leValues `shouldBe` numBuckets + 1
      values "fanin_perf_push_duration_seconds_count" `shouldBe` ["5"]
      length (values "fanin_perf_push_duration_seconds_sum") `shouldBe` 1

  describe "publish" $
    it "exports counters and gauges with experiment and kind labels" $ do
      m <- newMetrics "spec" 4
      ws <- newWriterStats
      replicateM_ 3 (recordSuccess ws KindTeam 1 0 1000)
      total <- readSnapshot ws
      let (r, _) = tick 0 1 total (initialTickState 0)
      publish m r
      out <- LBS8.unpack <$> P.exportMetricsAsText
      out `shouldContain` "fanin_perf_pushes_total{experiment=\"spec\",kind=\"team\"} 3"
      out `shouldContain` "fanin_perf_push_rate_current{experiment=\"spec\"} 3"
      out `shouldContain` "fanin_perf_writers{experiment=\"spec\"} 4"
      out `shouldContain` "fanin_perf_push_duration_seconds_count{experiment=\"spec\"} 3"

  describe "publishConsume" $
    it "exports consume counters, gauges and histograms" $ do
      m <- newConsumeMetrics "specc" 7
      rs <- newReaderStats
      replicateM_ 3 (recordFetch rs KindTeam 1000 [500])
      recordFetch rs KindTeam 1000 []
      replicateM_ 5 (recordGateWait rs 1000)
      total <- readReaderSnapshot rs
      let (r, _) = consumeTick 0 True 1 10 total (initialConsumeTickState 0)
      publishConsume m r
      out <- LBS8.unpack <$> P.exportMetricsAsText
      out `shouldContain` "fanin_perf_consumed_total{experiment=\"specc\",kind=\"team\"} 3"
      out `shouldContain` "fanin_perf_empty_fetches_total{experiment=\"specc\",kind=\"team\"} 1"
      out `shouldContain` "fanin_perf_consume_rate_current{experiment=\"specc\"} 3"
      out `shouldContain` "fanin_perf_backlog{experiment=\"specc\"} 7"
      out `shouldContain` "fanin_perf_clients{experiment=\"specc\"} 7"
      out `shouldContain` "fanin_perf_delivery_lag_seconds_count{experiment=\"specc\"} 3"
      out `shouldContain` "fanin_perf_fetch_duration_seconds_count{experiment=\"specc\"} 4"
      out `shouldContain` "fanin_perf_gate_wait_seconds_count{experiment=\"specc\"} 5"

  describe "publishAcks" $
    it "exports ack counters, gauges and the ack latency histogram" $ do
      m <- newAckMetrics "speca"
      rs <- newReaderStats
      replicateM_ 3 (recordAck rs KindTeam 1000)
      recordAckError rs KindTeam
      total <- readReaderSnapshot rs
      let (r, _) = consumeTick 0 True 1 0 total (initialConsumeTickState 0)
      publishAcks m r
      out <- LBS8.unpack <$> P.exportMetricsAsText
      out `shouldContain` "fanin_perf_acks_total{experiment=\"speca\",kind=\"team\"} 3"
      out `shouldContain` "fanin_perf_ack_errors_total{experiment=\"speca\",kind=\"team\"} 1"
      out `shouldContain` "fanin_perf_ack_rate_current{experiment=\"speca\"} 3"
      out `shouldContain` "fanin_perf_ack_duration_seconds_count{experiment=\"speca\"} 3"

  describe "metricsApp" $ do
    it "serves /metrics" $ statusFor ["metrics"] `shouldReturn` Just status200
    it "404s elsewhere" $ statusFor ["other"] `shouldReturn` Just status404
