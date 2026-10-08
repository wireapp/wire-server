module FanInPerf.Metrics
  ( Metrics,
    newMetrics,
    publish,
    latencySampleGroup,
    metricsApp,
    runMetricsServer,
  )
where

import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Stats
import FanInPerf.Targets (allKinds, kindName)
import Imports
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Prometheus qualified as P

-- | Only the ticker thread calls 'publish'; writers never touch these.
data Metrics = Metrics
  { experiment :: Text,
    pushes :: P.Vector P.Label2 P.Counter,
    targets :: P.Vector P.Label2 P.Counter,
    errors :: P.Vector P.Label2 P.Counter,
    pushRateCurrent :: P.Vector P.Label1 P.Gauge,
    pushRateMax :: P.Vector P.Label1 P.Gauge,
    errorRateCurrent :: P.Vector P.Label1 P.Gauge,
    errorRatioCurrent :: P.Vector P.Label1 P.Gauge,
    latency :: IORef (VU.Vector Int)
  }

newMetrics :: Text -> Int -> IO Metrics
newMetrics experiment writers = do
  let counterVec name help = P.register $ P.vector ("experiment", "kind") $ P.counter (P.Info name help)
      gaugeVec name help = P.register $ P.vector "experiment" $ P.gauge (P.Info name help)
  pushes <- counterVec "fanin_perf_pushes_total" "Successful pushes"
  targets <- counterVec "fanin_perf_targets_total" "Targets of successful pushes (clients kind counts users, not user x client rows)"
  errors <- counterVec "fanin_perf_errors_total" "Failed pushes"
  pushRateCurrent <- gaugeVec "fanin_perf_push_rate_current" "Pushes per second during the last tick"
  pushRateMax <- gaugeVec "fanin_perf_push_rate_max" "Maximal pushes per second after warmup"
  errorRateCurrent <- gaugeVec "fanin_perf_error_rate_current" "Errors per second during the last tick"
  errorRatioCurrent <- gaugeVec "fanin_perf_error_ratio_current" "errors / (pushes + errors) during the last tick"
  writersGauge <- gaugeVec "fanin_perf_writers" "Concurrent writer threads"
  P.withLabel writersGauge experiment (`P.setGauge` fromIntegral writers)
  latency <- newIORef (VU.replicate numBuckets 0)
  _ <- P.register (latencyMetric experiment latency)
  pure Metrics {..}

publish :: Metrics -> TickReport -> IO ()
publish m r = do
  for_ allKinds $ \k -> do
    let lbl = (m.experiment, kindName k)
    add m.pushes lbl (pushesOf k r.delta)
    add m.targets lbl (targetsOf k r.delta)
    add m.errors lbl (errorsOf k r.delta)
  set m.pushRateCurrent r.pushRate
  set m.pushRateMax r.maxPushRate
  set m.errorRateCurrent r.errorRate
  set m.errorRatioCurrent r.errorRatio
  writeIORef m.latency (latencyBuckets r.total)
  where
    add v lbl n = when (n > 0) $ P.withLabel v lbl (void . (`P.addCounter` fromIntegral n))
    set g x = P.withLabel g m.experiment (`P.setGauge` x)

-- | Histogram served from the ticker's latest bucket snapshot. Bucket bounds
-- are powers of two in nanoseconds; '_sum' is approximated by bucket midpoints.
latencyMetric :: Text -> IORef (VU.Vector Int) -> P.Metric ()
latencyMetric experiment ref =
  P.Metric $ pure ((), (: []) . latencySampleGroup experiment <$> readIORef ref)

latencySampleGroup :: Text -> VU.Vector Int -> P.SampleGroup
latencySampleGroup experiment buckets =
  P.SampleGroup info P.HistogramType (bucketSamples <> [sumSample, countSample])
  where
    name = "fanin_perf_push_duration_seconds"
    info = P.Info name "Push latency in seconds"
    lbl = ("experiment", experiment)
    count = VU.sum buckets
    cumulative = VU.toList (VU.postscanl' (+) 0 buckets)
    bucketSamples =
      [ P.Sample (name <> "_bucket") [lbl, ("le", T.pack (show (bucketUpperBoundSeconds b)))] (bshow c)
      | (b, c) <- zip [0 ..] cumulative
      ]
        <> [P.Sample (name <> "_bucket") [lbl, ("le", "+Inf")] (bshow count)]
    -- midpoint of [2^b, 2^(b+1)) is 0.75 * upper bound
    approxSum :: Double
    approxSum = sum [fromIntegral c * 0.75 * bucketUpperBoundSeconds b | (b, c) <- zip [0 ..] (VU.toList buckets)]
    sumSample = P.Sample (name <> "_sum") [lbl] (bshow approxSum)
    countSample = P.Sample (name <> "_count") [lbl] (bshow count)
    bshow :: (Show a) => a -> ByteString
    bshow = BS8.pack . show

metricsApp :: Wai.Application
metricsApp req respond = case Wai.pathInfo req of
  ["metrics"] -> do
    body <- P.exportMetricsAsText
    respond $ Wai.responseLBS status200 [(hContentType, "text/plain; version=0.0.4")] body
  _ -> respond $ Wai.responseLBS status404 [] "not found"

-- | Binds 0.0.0.0 so the dockerised OTel collector can scrape the host.
runMetricsServer :: Int -> IO ()
runMetricsServer port =
  Warp.runSettings (Warp.setHost "*4" (Warp.setPort port Warp.defaultSettings)) metricsApp
