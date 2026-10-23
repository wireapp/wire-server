module FanInPerf.Terminal
  ( groupThousands,
    formatLatency,
    formatStatus,
    formatSummary,
    formatConsumeStatus,
    formatConsumeOnlyStatus,
    ConsumeEnd (..),
    formatConsumeOnlySummary,
    WriterPhase (..),
    Drain (..),
    formatConsumeSummary,
    formatCoverage,
    renderStatusLine,
    renderLogLine,
    Console (..),
    newConsole,
    drawStatus,
    printLine,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Unboxed qualified as VU
import FanInPerf.ConsumeStats
import FanInPerf.Options (ReaderMode (..), consumeOnlyLabel, experimentLabel)
import FanInPerf.Stats
import FanInPerf.Targets (TargetKind, kindName)
import Imports
import Text.Printf (printf)

groupThousands :: Int -> Text
groupThousands n
  | n < 0 = "-" <> groupThousands (negate n)
  | otherwise = T.intercalate "," . reverse . map T.reverse . T.chunksOf 3 . T.reverse . T.pack $ show n

formatLatency :: Maybe Double -> Text
formatLatency = \case
  Nothing -> "-"
  Just s
    | s < 1 -> T.pack (printf "%.1fms" (s * 1000))
    | otherwise -> T.pack (printf "%.2fs" s)

-- | @pct "p99" q@: @p99<=1.2ms@. Percentiles come from histogram buckets and
-- are the upper bound of the bucket holding the value (at most 25% above it).
pct :: Text -> Maybe Double -> Text
pct label = \case
  Nothing -> label <> "=-"
  Just s -> label <> "<=" <> formatLatency (Just s)

-- | A maximum rate; "-" while nothing was measured (warmup).
maxText :: Double -> Text
maxText x = if x > 0 then rateText x else "-"

percent :: Double -> Text
percent x = T.pack (printf "%.2f%%" (x * 100))

rateText :: Double -> Text
rateText = groupThousands . round

seconds :: Double -> Text
seconds = T.pack . printf "%.1fs"

-- | p50 and p99 of a histogram.
pcts :: VU.Vector Int -> Text
pcts buckets = pct "p50" (quantileSeconds 0.5 buckets) <> " " <> pct "p99" (quantileSeconds 0.99 buckets)

-- | Status line segment shared by produce and consume.
pushSegment :: TickReport -> Text
pushSegment r = "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> maxText r.maxPushRate

formatStatus :: TickReport -> Text
formatStatus r =
  T.intercalate
    " | "
    [ pushSegment r,
      "err/s cur=" <> rateText r.errorRate <> " (" <> percent r.errorRatio <> ")",
      "targets/s cur=" <> rateText r.targetRate,
      pct "push p50" r.p50 <> " " <> pct "p99" r.p99
    ]

formatSummary :: TickReport -> [Text]
formatSummary r =
  let pushes = totalPushes r.total
      errors = totalErrors r.total
      avgRate = ratio pushes r.elapsed
   in [ "summary: duration=" <> seconds r.elapsed,
        "  pushes=" <> groupThousands pushes <> " targets=" <> groupThousands (totalTargets r.total) <> " errors=" <> groupThousands errors <> " (" <> percent (ratio errors (pushes + errors)) <> ")",
        "  push/s avg=" <> rateText avgRate <> " max=" <> maxText r.maxPushRate <> " (avg incl. warmup, max after warmup)",
        "  latency (cumulative, incl. warmup) " <> pcts (latencyBuckets r.total)
      ]

ackSegment :: ConsumeReport -> Text
ackSegment c = "ack/s cur=" <> rateText c.ackRate <> " max=" <> maxText c.maxAckRate <> " " <> pct "p99" c.ackP99

-- | @label: delivered=.. expected=.. missing=.. duplicates=..@ (net counts).
deliveryLine :: Text -> Int -> Int -> Text
deliveryLine label delivered expected =
  label
    <> ": delivered="
    <> groupThousands delivered
    <> " expected="
    <> groupThousands expected
    <> " missing="
    <> groupThousands (max 0 (expected - delivered))
    <> " duplicates="
    <> groupThousands (max 0 (delivered - expected))

-- | Summary lines of the acks; the average is over the caller's duration.
ackSummaryLines :: ConsumeReport -> Double -> [Text]
ackSummaryLines c avgAckRate =
  [ "  acks=" <> groupThousands (totalAcks c.readerTotal) <> " errors=" <> groupThousands (totalAckErrors c.readerTotal) <> " ack/s avg=" <> rateText avgAckRate <> " max=" <> maxText c.maxAckRate,
    "  ack latency (cumulative) " <> pcts (ackLatencyBuckets c.readerTotal)
  ]

-- | Cumulative fetch and gate wait summary lines.
fetchGateLines :: ConsumeReport -> [Text]
fetchGateLines c =
  [ "  fetch (cumulative) " <> pcts (fetchLatencyBuckets c.readerTotal) <> " empty=" <> percent (ratio (totalEmptyFetches c.readerTotal) (totalFetches c.readerTotal)) <> " errors=" <> groupThousands (totalFetchErrors c.readerTotal),
    "  gate wait (cumulative) " <> pcts (gateWaitBuckets c.readerTotal)
  ]

formatConsumeStatus :: ReaderMode -> TickReport -> ConsumeReport -> Text
formatConsumeStatus mode r c =
  T.intercalate " | " $
    [ pushSegment r,
      "cons/s cur=" <> rateText c.consumeRate <> " max=" <> maxText c.maxConsumeRate
    ]
      <> [ackSegment c | acking]
      <> [ pct "lag p50" c.lagP50 <> " " <> pct "p99" c.lagP99,
           "backlog=" <> groupThousands c.backlog,
           "err/s push=" <> rateText r.errorRate <> " fetch=" <> rateText c.fetchErrorRate <> (if acking then " ack=" <> rateText c.ackErrorRate else "")
         ]
  where
    acking = mode /= NoAcks

-- | Status line of 'FanInPerf.Options.ConsumeOnly': no pushes, and no lag (it
-- would only be the age of the rows).
formatConsumeOnlyStatus :: ReaderMode -> Double -> ConsumeReport -> Text
formatConsumeOnlyStatus mode elapsed c =
  T.intercalate " | " $
    [ "t=" <> T.pack (show (round elapsed :: Int)) <> "s cons/s cur=" <> rateText c.consumeRate <> " max=" <> maxText c.maxConsumeRate
    ]
      <> [ackSegment c | acking]
      <> [ "backlog=" <> groupThousands c.backlog,
           "err/s fetch=" <> rateText c.fetchErrorRate <> (if acking then " ack=" <> rateText c.ackErrorRate else "")
         ]
  where
    acking = mode /= NoAcks

-- | Seconds from start until the writers stopped, and the notifications
-- delivered by then, and the ack upserts by then.
data WriterPhase = WriterPhase {duration :: Double, delivered :: Int, acks :: Int}

-- | How the drain after the writer phase ended, with its duration in seconds.
-- 'DrainStalled': nothing was delivered for a while, the rest is not coming.
data Drain = DrainComplete Double | DrainStalled Double | DrainTimedOut Double | DrainInterrupted

-- | @formatConsumeSummary expectedDeliveries writerPhase drain pushReport
-- consumeReport@. Duration and averages cover the writer phase only (not the
-- drain); totals, maxima and percentiles cover the whole run.
formatConsumeSummary :: ReaderMode -> Int -> WriterPhase -> Drain -> TickReport -> ConsumeReport -> [Text]
formatConsumeSummary mode expected phase drain r c =
  let delivered = totalDelivered c.readerTotal
      avgRate = ratio phase.delivered phase.duration
      pushes = totalPushes r.total
      backlogAtStop = max 0 (expected - phase.delivered)
      behind = avgRate > 0 && fromIntegral backlogAtStop > avgRate
      drainText = case drain of
        DrainComplete t -> "complete after " <> seconds t
        DrainStalled t -> "stalled after " <> seconds t <> " (no delivery for several poll intervals: the rest is lost, not in flight)"
        DrainTimedOut t -> "timed out after " <> seconds t
        DrainInterrupted -> "interrupted"
   in formatSummary r {elapsed = phase.duration}
        <> [ deliveryLine (experimentLabel mode) delivered expected,
             "  cons/s avg=" <> rateText avgRate <> " max=" <> maxText c.maxConsumeRate <> " (avg incl. warmup, max after warmup, both writer phase)",
             "  fan-out: " <> T.pack (printf "%.1f" (ratio expected pushes)) <> " deliveries per push (cons/s and push/s differ by this factor)",
             "  backlog at writer stop: " <> groupThousands backlogAtStop <> (if avgRate > 0 then " (" <> seconds (ratio backlogAtStop avgRate) <> " of delivery)" else ""),
             "  lag (cumulative) " <> pcts (lagBuckets c.readerTotal)
           ]
        <> fetchGateLines c
        <> [ "  warning: readers fell behind the writers (backlog above 1s of delivery at writer stop); missing then understates the cursor race, as readers stay below the head"
           | behind
           ]
        <> (if mode /= NoAcks then ackSummaryLines c (ratio phase.acks phase.duration) else [])
        <> ["  drain: " <> drainText]

-- | How a consume-only run ended: every reader caught up and stopped (what is
-- still missing then was not delivered, e.g. failed fetches), or not.
data ConsumeEnd = ConsumeFinished | ConsumeTimedOut | ConsumeInterrupted
  deriving (Eq, Show)

-- | @formatConsumeOnlySummary expectedDeliveries elapsed end milestones consumeReport@.
-- @elapsed@ is the time from the start of the readers until the end of the run;
-- the average over it includes the tail of the slowest readers, which the
-- @milestones@ show: (percent, seconds until that share of the expected
-- deliveries was reached, Nothing if never).
formatConsumeOnlySummary :: ReaderMode -> Int -> Double -> ConsumeEnd -> [(Int, Maybe Double)] -> ConsumeReport -> [Text]
formatConsumeOnlySummary mode expected elapsed end milestones c =
  let delivered = totalDelivered c.readerTotal
      avgRate = ratio delivered elapsed
      endText = case end of
        ConsumeFinished -> "all readers caught up after " <> seconds elapsed
        ConsumeTimedOut -> "timed out after " <> seconds elapsed
        ConsumeInterrupted -> "interrupted after " <> seconds elapsed
   in [ "summary: duration=" <> seconds elapsed,
        deliveryLine (consumeOnlyLabel mode) delivered expected,
        "  cons/s avg=" <> rateText avgRate <> " max=" <> maxText c.maxConsumeRate <> " (avg incl. warmup and tail, max after warmup)",
        "  time to delivered: " <> T.intercalate " " [T.pack (show p) <> "%=" <> maybe "-" seconds t | (p, t) <- milestones]
      ]
        <> fetchGateLines c
        <> (if mode /= NoAcks then ackSummaryLines c (ratio (totalAcks c.readerTotal) elapsed) else [])
        <> ["  end: " <> endText]

formatCoverage :: [(TargetKind, Double)] -> [Text]
formatCoverage = map line
  where
    line (k, c) =
      "coverage "
        <> kindName k
        <> ": "
        <> percent c
        <> " of streams have a reader"
        <> if c < 1 then " (warning: consumed/s will stay below targets/s)" else ""

clearLine :: Text
clearLine = "\r\ESC[2K"

renderStatusLine :: Bool -> Text -> Text
renderStatusLine tty line = if tty then clearLine <> line else line <> "\n"

renderLogLine :: Bool -> Text -> Text
renderLogLine tty line = (if tty then clearLine else "") <> line <> "\n"

newtype Console = Console {isTty :: Bool}

newConsole :: IO Console
newConsole = Console <$> hIsTerminalDevice stdout

drawStatus :: Console -> Text -> IO ()
drawStatus c line = T.putStr (renderStatusLine c.isTty line) >> hFlush stdout

printLine :: Console -> Text -> IO ()
printLine c line = T.putStr (renderLogLine c.isTty line) >> hFlush stdout
