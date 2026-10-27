-- | The result of a run as data: rendered as text by "FanInPerf.Terminal" and
-- as JSON for @--summary-json@, so the two cannot drift. Built once, after the
-- run; nothing here is on the hot path.
module FanInPerf.Summary
  ( RunSummary (..),
    PushSummary (..),
    DeliverySummary (..),
    Milestone (..),
    FetchSummary (..),
    AckSummary (..),
    DatabaseSummary (..),
    Percentiles (..),
    WriterPhase (..),
    Drain (..),
    ConsumeEnd (..),
    produceSummary,
    consumeSummary,
    consumeOnlySummary,
  )
where

import Data.Aeson (ToJSON (..), Value, camelTo2, genericToJSON, object, (.=))
import Data.Aeson qualified as A
import Data.Vector.Unboxed qualified as VU
import FanInPerf.ConsumeStats
import FanInPerf.GcImpact (GcImpact)
import FanInPerf.Options (ReaderMode (..), consumeOnlyLabel, experimentLabel)
import FanInPerf.Stats
import Imports

-- | Seconds from start until the writers stopped, and the notifications
-- delivered by then, and the ack upserts by then.
data WriterPhase = WriterPhase {duration :: Double, delivered :: Int, acks :: Int}

-- | How the drain after the writer phase ended, with its duration in seconds.
-- 'DrainStalled': nothing was delivered for a while, the rest is not coming.
data Drain = DrainComplete Double | DrainStalled Double | DrainTimedOut Double | DrainInterrupted

instance ToJSON Drain where
  toJSON = \case
    DrainComplete t -> drainJson "complete" (Just t)
    DrainStalled t -> drainJson "stalled" (Just t)
    DrainTimedOut t -> drainJson "timed_out" (Just t)
    DrainInterrupted -> drainJson "interrupted" Nothing
    where
      drainJson :: Text -> Maybe Double -> Value
      drainJson state seconds = object ["state" .= state, "seconds" .= seconds]

-- | How a consume-only run ended: every reader caught up and stopped (what is
-- still missing then was not delivered, e.g. failed fetches), or not.
data ConsumeEnd = ConsumeFinished | ConsumeTimedOut | ConsumeInterrupted
  deriving (Eq, Show)

instance ToJSON ConsumeEnd where
  toJSON = \case
    ConsumeFinished -> "finished"
    ConsumeTimedOut -> "timed_out"
    ConsumeInterrupted -> "interrupted"

-- | Upper bounds of the histogram buckets holding the median and the 99th
-- percentile (at most 25% above the value); 'Nothing' without observations.
data Percentiles = Percentiles {p50 :: Maybe Double, p99 :: Maybe Double}
  deriving stock (Eq, Show, Generic)

data PushSummary = PushSummary
  { pushes :: Int,
    targets :: Int,
    errors :: Int,
    errorRatio :: Double,
    -- | over the summary's duration, including warmup
    rateAvg :: Double,
    -- | after warmup, 'Nothing' if never measured
    rateMax :: Maybe Double,
    -- | cumulative, including warmup
    latency :: Percentiles
  }
  deriving stock (Eq, Show, Generic)

data DeliverySummary = DeliverySummary
  { delivered :: Int,
    expected :: Int,
    -- | expected - delivered, at least 0
    missing :: Int,
    -- | delivered - expected, at least 0
    duplicates :: Int,
    rateAvg :: Double,
    rateMax :: Maybe Double,
    -- | deliveries per push; consume only
    fanOut :: Maybe Double,
    -- | expected - delivered when the writers stopped; consume only
    backlogAtWriterStop :: Maybe Int,
    -- | age of the notifications at delivery; consume only
    lag :: Maybe Percentiles,
    -- | seconds until N % of the expected deliveries were reached; consume-only only
    milestones :: [Milestone]
  }
  deriving stock (Eq, Show, Generic)

data Milestone = Milestone {percent :: Int, seconds :: Maybe Double}
  deriving stock (Eq, Show, Generic)

data FetchSummary = FetchSummary
  { latency :: Percentiles,
    emptyRatio :: Double,
    errors :: Int,
    -- | waits for a reader gate slot
    gateWait :: Percentiles
  }
  deriving stock (Eq, Show, Generic)

data AckSummary = AckSummary
  { acks :: Int,
    errors :: Int,
    rateAvg :: Double,
    rateMax :: Maybe Double,
    latency :: Percentiles
  }
  deriving stock (Eq, Show, Generic)

-- | What the database says about the run; filled in after it, by the caller.
data DatabaseSummary = DatabaseSummary
  { -- | deadlocks detected during the run
    deadlocks :: Int,
    -- | rows in the notification tables at the end
    rows :: Int
  }
  deriving stock (Eq, Show, Generic)

data RunSummary = RunSummary
  { -- | @produce@, or the label of the reader mode (@consume-ack@, ...)
    command :: Text,
    -- | consume: the writer phase; consume-only: until the end of the run
    duration :: Double,
    pushes :: Maybe PushSummary,
    deliveries :: Maybe DeliverySummary,
    fetch :: Maybe FetchSummary,
    -- | with acknowledging readers only
    acks :: Maybe AckSummary,
    -- | consume only
    drain :: Maybe Drain,
    -- | consume-only only
    end :: Maybe ConsumeEnd,
    -- | rows of the counted streams in the database; consume-only only
    rowsTotal :: Maybe Int,
    database :: Maybe DatabaseSummary,
    -- | consume runs on partitioned tables only
    gc :: Maybe GcImpact
  }
  deriving stock (Generic)

jsonOptions :: A.Options
jsonOptions = A.defaultOptions {A.fieldLabelModifier = camelTo2 '_'}

instance ToJSON Percentiles where toJSON = genericToJSON jsonOptions

instance ToJSON PushSummary where toJSON = genericToJSON jsonOptions

instance ToJSON DeliverySummary where toJSON = genericToJSON jsonOptions

instance ToJSON Milestone where toJSON = genericToJSON jsonOptions

instance ToJSON FetchSummary where toJSON = genericToJSON jsonOptions

instance ToJSON AckSummary where toJSON = genericToJSON jsonOptions

instance ToJSON DatabaseSummary where toJSON = genericToJSON jsonOptions

instance ToJSON RunSummary where toJSON = genericToJSON jsonOptions

-- | A maximum rate; 'Nothing' while nothing was measured (warmup).
measured :: Double -> Maybe Double
measured x = if x > 0 then Just x else Nothing

bucketPercentiles :: VU.Vector Int -> Percentiles
bucketPercentiles buckets = Percentiles (quantileSeconds 0.5 buckets) (quantileSeconds 0.99 buckets)

pushSummary :: TickReport -> PushSummary
pushSummary r =
  let pushes = totalPushes r.total
      errors = totalErrors r.total
   in PushSummary
        { pushes,
          targets = totalTargets r.total,
          errors,
          errorRatio = ratio errors (pushes + errors),
          rateAvg = ratio pushes r.elapsed,
          rateMax = measured r.maxPushRate,
          latency = bucketPercentiles (latencyBuckets r.total)
        }

fetchSummary :: ConsumeReport -> FetchSummary
fetchSummary c =
  FetchSummary
    { latency = bucketPercentiles (fetchLatencyBuckets c.readerTotal),
      emptyRatio = ratio (totalEmptyFetches c.readerTotal) (totalFetches c.readerTotal),
      errors = totalFetchErrors c.readerTotal,
      gateWait = bucketPercentiles (gateWaitBuckets c.readerTotal)
    }

-- | @ackSummaryFor mode report avgAckRate@; the average is over the caller's
-- duration. Acks exist in 'Acks' mode only.
ackSummaryFor :: ReaderMode -> ConsumeReport -> Double -> Maybe AckSummary
ackSummaryFor mode c avgAckRate = case mode of
  NoAcks -> Nothing
  Acks _ ->
    Just
      AckSummary
        { acks = totalAcks c.readerTotal,
          errors = totalAckErrors c.readerTotal,
          rateAvg = avgAckRate,
          rateMax = measured c.maxAckRate,
          latency = bucketPercentiles (ackLatencyBuckets c.readerTotal)
        }

-- | (missing, duplicates), net counts: they do not cancel out.
deliveryCounts :: Int -> Int -> (Int, Int)
deliveryCounts delivered expected = (max 0 (expected - delivered), max 0 (delivered - expected))

-- | The pushes of a @produce@ run; @r.elapsed@ is its duration.
produceSummary :: TickReport -> RunSummary
produceSummary r =
  RunSummary
    { command = "produce",
      duration = r.elapsed,
      pushes = Just (pushSummary r),
      deliveries = Nothing,
      fetch = Nothing,
      acks = Nothing,
      drain = Nothing,
      end = Nothing,
      rowsTotal = Nothing,
      database = Nothing,
      gc = Nothing
    }

-- | @consumeSummary mode expectedDeliveries writerPhase drain pushReport
-- consumeReport@. Duration and averages cover the writer phase only (not the
-- drain); totals, maxima and percentiles cover the whole run.
--
-- The rates describe the system under load: during the drain nothing is
-- pushed and the readers have the database to themselves, which would inflate
-- an average. The drain is reported separately. Totals and percentiles must
-- include the drain, because a notification delivered then still counts and
-- @missing@ is only known once it is over. The backlog at writer stop (also a
-- writer-phase value) tells how far the readers were behind when the load
-- ended, i.e. whether the averages can be trusted.
consumeSummary :: ReaderMode -> Int -> WriterPhase -> Drain -> TickReport -> ConsumeReport -> RunSummary
consumeSummary mode expected phase drain r c =
  let delivered = totalDelivered c.readerTotal
      (missing, duplicates) = deliveryCounts delivered expected
   in (produceSummary r {elapsed = phase.duration})
        { command = experimentLabel mode,
          deliveries =
            Just
              DeliverySummary
                { delivered,
                  expected,
                  missing,
                  duplicates,
                  rateAvg = ratio phase.delivered phase.duration,
                  rateMax = measured c.maxConsumeRate,
                  fanOut = Just (ratio expected (totalPushes r.total)),
                  backlogAtWriterStop = Just (max 0 (expected - phase.delivered)),
                  lag = Just (bucketPercentiles (lagBuckets c.readerTotal)),
                  milestones = []
                },
          fetch = Just (fetchSummary c),
          acks = ackSummaryFor mode c (ratio phase.acks phase.duration),
          drain = Just drain
        }

-- | @consumeOnlySummary mode expectedDeliveries elapsed end milestones
-- consumeReport@. @elapsed@ is the time from the start of the readers until
-- the end of the run; the average over it includes the tail of the slowest
-- readers, which the @milestones@ show: (percent, seconds until that share of
-- the expected deliveries was reached, Nothing if never).
consumeOnlySummary :: ReaderMode -> Int -> Double -> ConsumeEnd -> [(Int, Maybe Double)] -> ConsumeReport -> RunSummary
consumeOnlySummary mode expected elapsed end milestones c =
  let delivered = totalDelivered c.readerTotal
      (missing, duplicates) = deliveryCounts delivered expected
   in RunSummary
        { command = consumeOnlyLabel mode,
          duration = elapsed,
          pushes = Nothing,
          deliveries =
            Just
              DeliverySummary
                { delivered,
                  expected,
                  missing,
                  duplicates,
                  rateAvg = ratio delivered elapsed,
                  rateMax = measured c.maxConsumeRate,
                  fanOut = Nothing,
                  backlogAtWriterStop = Nothing,
                  lag = Nothing,
                  milestones = [Milestone p t | (p, t) <- milestones]
                },
          fetch = Just (fetchSummary c),
          acks = ackSummaryFor mode c (ratio (totalAcks c.readerTotal) elapsed),
          drain = Nothing,
          end = Just end,
          rowsTotal = Nothing,
          database = Nothing,
          gc = Nothing
        }
