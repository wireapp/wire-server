module FanInPerf.Terminal
  ( groupThousands,
    formatLatency,
    formatStatus,
    formatSummary,
    formatConsumeStatus,
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
import FanInPerf.Options (ReaderMode (..), experimentLabel)
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

formatStatus :: TickReport -> Text
formatStatus r =
  T.intercalate
    " | "
    [ "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> maxText r.maxPushRate,
      "err/s cur=" <> rateText r.errorRate <> " (" <> percent r.errorRatio <> ")",
      "targets/s cur=" <> rateText r.targetRate,
      pct "push p50" r.p50 <> " " <> pct "p99" r.p99
    ]

formatSummary :: TickReport -> [Text]
formatSummary r =
  let pushes = totalPushes r.total
      errors = totalErrors r.total
      ratio = if pushes + errors > 0 then fromIntegral errors / fromIntegral (pushes + errors) else 0
      avgRate = if r.elapsed > 0 then fromIntegral pushes / r.elapsed else 0
   in [ "summary: duration=" <> T.pack (printf "%.1fs" r.elapsed),
        "  pushes=" <> groupThousands pushes <> " targets=" <> groupThousands (totalTargets r.total) <> " errors=" <> groupThousands errors <> " (" <> percent ratio <> ")",
        "  push/s avg=" <> rateText avgRate <> " max=" <> maxText r.maxPushRate <> " (avg incl. warmup, max after warmup)",
        "  latency (cumulative, incl. warmup) " <> pct "p50" (cumulative 0.5 latencyBuckets r.total) <> " " <> pct "p99" (cumulative 0.99 latencyBuckets r.total)
      ]

-- | Percentile over everything counted so far.
cumulative :: Double -> (a -> VU.Vector Int) -> a -> Maybe Double
cumulative q buckets = quantileSeconds q . buckets

formatConsumeStatus :: ReaderMode -> TickReport -> ConsumeReport -> Text
formatConsumeStatus mode r c =
  T.intercalate " | " $
    [ "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> maxText r.maxPushRate,
      "cons/s cur=" <> rateText c.consumeRate <> " max=" <> maxText c.maxConsumeRate
    ]
      <> ["ack/s cur=" <> rateText c.ackRate <> " max=" <> maxText c.maxAckRate <> " " <> pct "p99" c.ackP99 | acking]
      <> [ pct "lag p50" c.lagP50 <> " " <> pct "p99" c.lagP99,
           "backlog=" <> groupThousands c.backlog,
           "err/s push=" <> rateText r.errorRate <> " fetch=" <> rateText c.fetchErrorRate <> (if acking then " ack=" <> rateText c.ackErrorRate else "")
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
      fetches = totalFetches c.readerTotal
      emptyRatio = if fetches > 0 then fromIntegral (totalEmptyFetches c.readerTotal) / fromIntegral fetches else 0
      avgRate = if phase.duration > 0 then fromIntegral phase.delivered / phase.duration else 0
      avgAckRate = if phase.duration > 0 then fromIntegral phase.acks / phase.duration else 0
      ackLines =
        [ "  acks=" <> groupThousands (totalAcks c.readerTotal) <> " errors=" <> groupThousands (totalAckErrors c.readerTotal) <> " ack/s avg=" <> rateText avgAckRate <> " max=" <> maxText c.maxAckRate,
          "  ack latency (cumulative) " <> cum 0.5 ackLatencyBuckets <> " " <> cum 0.99 ackLatencyBuckets
        ]
      cum :: Double -> (ReaderSnapshot -> VU.Vector Int) -> Text
      cum q buckets = pct (if q < 0.9 then "p50" else "p99") (cumulative q buckets c.readerTotal)
      pushes = totalPushes r.total
      backlogAtStop = max 0 (expected - phase.delivered)
      behind = avgRate > 0 && fromIntegral backlogAtStop > avgRate
      seconds :: Double -> Text
      seconds = T.pack . printf "%.1fs"
      drainText = case drain of
        DrainComplete t -> "complete after " <> seconds t
        DrainStalled t -> "stalled after " <> seconds t <> " (no delivery for 2s: the rest is lost, not in flight)"
        DrainTimedOut t -> "timed out after " <> seconds t
        DrainInterrupted -> "interrupted"
   in formatSummary r {elapsed = phase.duration}
        <> [ experimentLabel mode
               <> ": delivered="
               <> groupThousands delivered
               <> " expected="
               <> groupThousands expected
               <> " missing="
               <> groupThousands (max 0 (expected - delivered))
               <> " duplicates="
               <> groupThousands (max 0 (delivered - expected)),
             "  cons/s avg=" <> rateText avgRate <> " max=" <> maxText c.maxConsumeRate <> " (avg incl. warmup, max after warmup, both writer phase)",
             "  fan-out: " <> T.pack (printf "%.1f" (if pushes > 0 then fromIntegral expected / fromIntegral pushes else 0 :: Double)) <> " deliveries per push (cons/s and push/s differ by this factor)",
             "  backlog at writer stop: " <> groupThousands backlogAtStop <> (if avgRate > 0 then " (" <> T.pack (printf "%.1fs" (fromIntegral backlogAtStop / avgRate :: Double)) <> " of delivery)" else ""),
             "  lag (cumulative) " <> cum 0.5 lagBuckets <> " " <> cum 0.99 lagBuckets,
             "  fetch (cumulative) " <> cum 0.5 fetchLatencyBuckets <> " " <> cum 0.99 fetchLatencyBuckets <> " empty=" <> percent emptyRatio,
             "  gate wait (cumulative) " <> cum 0.5 gateWaitBuckets <> " " <> cum 0.99 gateWaitBuckets
           ]
        <> [ "  warning: readers fell behind the writers (backlog above 1s of delivery at writer stop); missing then understates the cursor race, as readers stay below the head"
           | behind
           ]
        <> (if mode /= NoAcks then ackLines else [])
        <> ["  drain: " <> drainText]

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
