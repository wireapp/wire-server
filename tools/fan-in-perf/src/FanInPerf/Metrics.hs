module FanInPerf.Metrics
  ( Metrics,
    newMetrics,
    publish,
    latencySampleGroup,
    histogramSampleGroup,
    ConsumeMetrics,
    newConsumeMetrics,
    publishConsume,
    AckMetrics,
    newAckMetrics,
    publishAcks,
    metricsApp,
    runMetricsServer,
  )
where

import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import FanInPerf.ConsumeStats
import FanInPerf.Stats
import FanInPerf.Targets (allKinds, kindName)
import Imports
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Prometheus qualified as P

-- | Only the ticker thread calls 'publish'; writers never touch these.
data Metrics = Metrics
  { experimentName :: Text,
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
newMetrics experimentName writers = do
  pushes <- counterVec "fanin_perf_pushes_total" "Successful pushes"
  targets <- counterVec "fanin_perf_targets_total" "Targets of successful pushes (clients kind counts users, not user x client rows)"
  errors <- counterVec "fanin_perf_errors_total" "Failed pushes"
  pushRateCurrent <- gaugeVec "fanin_perf_push_rate_current" "Pushes per second during the last tick"
  pushRateMax <- gaugeVec "fanin_perf_push_rate_max" "Maximal pushes per second after warmup"
  errorRateCurrent <- gaugeVec "fanin_perf_error_rate_current" "Errors per second during the last tick"
  errorRatioCurrent <- gaugeVec "fanin_perf_error_ratio_current" "errors / (pushes + errors) during the last tick"
  writersGauge <- gaugeVec "fanin_perf_writers" "Concurrent writer threads"
  P.withLabel writersGauge experimentName (`P.setGauge` fromIntegral writers)
  latency <- newIORef (VU.replicate numBuckets 0)
  _ <- P.register (histogramMetric "fanin_perf_push_duration_seconds" "Push latency in seconds" experimentName latency)
  pure Metrics {..}

publish :: Metrics -> TickReport -> IO ()
publish m r = do
  for_ allKinds $ \k -> do
    let lbl = (m.experimentName, kindName k)
    addCounter m.pushes lbl (pushesOf k r.delta)
    addCounter m.targets lbl (targetsOf k r.delta)
    addCounter m.errors lbl (errorsOf k r.delta)
  setGauge m.experimentName m.pushRateCurrent r.pushRate
  setGauge m.experimentName m.pushRateMax r.maxPushRate
  setGauge m.experimentName m.errorRateCurrent r.errorRate
  setGauge m.experimentName m.errorRatioCurrent r.errorRatio
  writeIORef m.latency (latencyBuckets r.total)

addCounter :: P.Vector P.Label2 P.Counter -> (Text, Text) -> Int -> IO ()
addCounter v lbl n = when (n > 0) $ P.withLabel v lbl (void . (`P.addCounter` fromIntegral n))

setGauge :: Text -> P.Vector P.Label1 P.Gauge -> Double -> IO ()
setGauge experiment g x = P.withLabel g experiment (`P.setGauge` x)

counterVec :: Text -> Text -> IO (P.Vector P.Label2 P.Counter)
counterVec name help = P.register $ P.vector ("experiment", "kind") $ P.counter (P.Info name help)

gaugeVec :: Text -> Text -> IO (P.Vector P.Label1 P.Gauge)
gaugeVec name help = P.register $ P.vector "experiment" $ P.gauge (P.Info name help)

-- | Histogram served from the ticker's latest bucket snapshot (see 'histogramSampleGroup').
histogramMetric :: Text -> Text -> Text -> IORef (VU.Vector Int) -> P.Metric ()
histogramMetric name help experiment ref =
  P.Metric $ pure ((), (: []) . histogramSampleGroup name help experiment <$> readIORef ref)

latencySampleGroup :: Text -> VU.Vector Int -> P.SampleGroup
latencySampleGroup = histogramSampleGroup "fanin_perf_push_duration_seconds" "Push latency in seconds"

-- | Bucket bounds are those of 'bucketIndex' (four per power of two).
histogramSampleGroup :: Text -> Text -> Text -> VU.Vector Int -> P.SampleGroup
histogramSampleGroup name help experiment buckets =
  P.SampleGroup info P.HistogramType (bucketSamples <> [sumSample, countSample])
  where
    info = P.Info name help
    lbl = ("experiment", experiment)
    count = VU.sum buckets
    cumulative = VU.toList (VU.postscanl' (+) 0 buckets)
    bucketSamples =
      [ P.Sample (name <> "_bucket") [lbl, ("le", T.pack (show (bucketUpperBoundSeconds b)))] (bshow c)
      | (b, c) <- zip [0 ..] cumulative
      ]
        <> [P.Sample (name <> "_bucket") [lbl, ("le", "+Inf")] (bshow count)]
    -- The tool keeps only bucket counts, not the observed values, so '_sum'
    -- is an estimate: every value is taken to lie in the middle of its bucket
    -- (at most 12.5% off for a single value). sum / count in Grafana is
    -- therefore an approximate mean.
    approxSum :: Double
    approxSum = sum [fromIntegral c * bucketMidSeconds b | (b, c) <- zip [0 ..] (VU.toList buckets)]
    sumSample = P.Sample (name <> "_sum") [lbl] (bshow approxSum)
    countSample = P.Sample (name <> "_count") [lbl] (bshow count)
    bshow :: (Show a) => a -> ByteString
    bshow = BS8.pack . show

data ConsumeMetrics = ConsumeMetrics
  { experiment :: Text,
    consumed :: P.Vector P.Label2 P.Counter,
    fetchCount :: P.Vector P.Label2 P.Counter,
    emptyFetchCount :: P.Vector P.Label2 P.Counter,
    fetchErrorCount :: P.Vector P.Label2 P.Counter,
    consumeRateCurrent :: P.Vector P.Label1 P.Gauge,
    consumeRateMax :: P.Vector P.Label1 P.Gauge,
    backlogGauge :: P.Vector P.Label1 P.Gauge,
    fetchLatencyRef :: IORef (VU.Vector Int),
    lagRef :: IORef (VU.Vector Int),
    gateWaitRef :: IORef (VU.Vector Int)
  }

-- | Registers the consume metrics; call once per process.
newConsumeMetrics :: Text -> Int -> IO ConsumeMetrics
newConsumeMetrics experiment clients = do
  consumed <- counterVec "fanin_perf_consumed_total" "Notifications delivered to readers"
  fetchCount <- counterVec "fanin_perf_fetches_total" "Fetches of a stream"
  emptyFetchCount <- counterVec "fanin_perf_empty_fetches_total" "Fetches that returned no notification"
  fetchErrorCount <- counterVec "fanin_perf_fetch_errors_total" "Failed fetches"
  consumeRateCurrent <- gaugeVec "fanin_perf_consume_rate_current" "Delivered notifications per second during the last tick"
  consumeRateMax <- gaugeVec "fanin_perf_consume_rate_max" "Maximal delivered notifications per second after warmup"
  backlogGauge <- gaugeVec "fanin_perf_backlog" "Expected minus delivered notifications (clamped at 0)"
  clientsGauge <- gaugeVec "fanin_perf_clients" "Simulated reader clients"
  P.withLabel clientsGauge experiment (`P.setGauge` fromIntegral clients)
  fetchLatencyRef <- newIORef (VU.replicate numBuckets 0)
  lagRef <- newIORef (VU.replicate numBuckets 0)
  gateWaitRef <- newIORef (VU.replicate numBuckets 0)
  _ <- P.register (histogramMetric "fanin_perf_fetch_duration_seconds" "Fetch latency in seconds" experiment fetchLatencyRef)
  _ <- P.register (histogramMetric "fanin_perf_delivery_lag_seconds" "Time from a notification's creation to its delivery to a reader" experiment lagRef)
  _ <- P.register (histogramMetric "fanin_perf_gate_wait_seconds" "Wait for a reader gate slot (--reader-pool-size concurrent store calls) before a fetch or ack, in seconds" experiment gateWaitRef)
  pure ConsumeMetrics {experiment, consumed, fetchCount, emptyFetchCount, fetchErrorCount, consumeRateCurrent, consumeRateMax, backlogGauge, fetchLatencyRef, lagRef, gateWaitRef}

-- | Only the ticker thread calls this.
publishConsume :: ConsumeMetrics -> ConsumeReport -> IO ()
publishConsume m r = do
  for_ allKinds $ \k -> do
    let lbl = (m.experiment, kindName k)
    addCounter m.consumed lbl (deliveredOf k r.readerDelta)
    addCounter m.fetchCount lbl (fetchesOf k r.readerDelta)
    addCounter m.emptyFetchCount lbl (emptyFetchesOf k r.readerDelta)
    addCounter m.fetchErrorCount lbl (fetchErrorsOf k r.readerDelta)
  setGauge m.experiment m.consumeRateCurrent r.consumeRate
  setGauge m.experiment m.consumeRateMax r.maxConsumeRate
  setGauge m.experiment m.backlogGauge (fromIntegral r.backlog)
  writeIORef m.fetchLatencyRef (fetchLatencyBuckets r.readerTotal)
  writeIORef m.lagRef (lagBuckets r.readerTotal)
  writeIORef m.gateWaitRef (gateWaitBuckets r.readerTotal)

data AckMetrics = AckMetrics
  { experiment :: Text,
    ackCount :: P.Vector P.Label2 P.Counter,
    ackErrorCount :: P.Vector P.Label2 P.Counter,
    ackRateCurrent :: P.Vector P.Label1 P.Gauge,
    ackRateMax :: P.Vector P.Label1 P.Gauge,
    ackLatencyRef :: IORef (VU.Vector Int)
  }

-- | Registers the ack metrics; call once per process and only when readers
-- acknowledge, so B's /metrics stay unchanged.
newAckMetrics :: Text -> IO AckMetrics
newAckMetrics experiment = do
  ackCount <- counterVec "fanin_perf_acks_total" "Ack upserts (one per stream at an ack point)"
  ackErrorCount <- counterVec "fanin_perf_ack_errors_total" "Failed ack upserts"
  ackRateCurrent <- gaugeVec "fanin_perf_ack_rate_current" "Ack upserts per second during the last tick"
  ackRateMax <- gaugeVec "fanin_perf_ack_rate_max" "Maximal ack upserts per second after warmup"
  ackLatencyRef <- newIORef (VU.replicate numBuckets 0)
  _ <- P.register (histogramMetric "fanin_perf_ack_duration_seconds" "Ack upsert latency in seconds" experiment ackLatencyRef)
  pure AckMetrics {..}

-- | Only the ticker thread calls this.
publishAcks :: AckMetrics -> ConsumeReport -> IO ()
publishAcks m r = do
  for_ allKinds $ \k -> do
    let lbl = (m.experiment, kindName k)
    addCounter m.ackCount lbl (acksOf k r.readerDelta)
    addCounter m.ackErrorCount lbl (ackErrorsOf k r.readerDelta)
  setGauge m.experiment m.ackRateCurrent r.ackRate
  setGauge m.experiment m.ackRateMax r.maxAckRate
  writeIORef m.ackLatencyRef (ackLatencyBuckets r.readerTotal)

metricsApp :: Wai.Application
metricsApp req respond = case Wai.pathInfo req of
  ["metrics"] -> do
    -- The odd content-type is explained here: https://prometheus.io/docs/instrumenting/content_negotiation/#protocol-types
    -- prometheus-client talks protocol version 0.0.4
    body <- P.exportMetricsAsText
    respond $ Wai.responseLBS status200 [(hContentType, "text/plain; version=0.0.4")] body
  _ -> respond $ Wai.responseLBS status404 [] "not found"

-- | Binds 0.0.0.0 so the dockerised OTel collector can scrape the host.
runMetricsServer :: Int -> IO ()
runMetricsServer port =
  Warp.runSettings (Warp.setHost "*4" (Warp.setPort port Warp.defaultSettings)) metricsApp
