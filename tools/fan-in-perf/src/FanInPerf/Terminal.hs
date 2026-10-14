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
import FanInPerf.ConsumeStats
import FanInPerf.Options (ReaderMode (..))
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

percent :: Double -> Text
percent x = T.pack (printf "%.2f%%" (x * 100))

rateText :: Double -> Text
rateText = groupThousands . round

formatStatus :: TickReport -> Text
formatStatus r =
  T.intercalate
    " | "
    [ "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> rateText r.maxPushRate,
      "err/s cur=" <> rateText r.errorRate <> " (" <> percent r.errorRatio <> ")",
      "targets/s cur=" <> rateText r.targetRate,
      "p50(cum.)=" <> formatLatency r.p50 <> " p99(cum.)=" <> formatLatency r.p99
    ]

formatSummary :: TickReport -> [Text]
formatSummary r =
  let pushes = totalPushes r.total
      errors = totalErrors r.total
      ratio = if pushes + errors > 0 then fromIntegral errors / fromIntegral (pushes + errors) else 0
      avgRate = if r.elapsed > 0 then fromIntegral pushes / r.elapsed else 0
   in [ "summary: duration=" <> T.pack (printf "%.1fs" r.elapsed),
        "  pushes=" <> groupThousands pushes <> " targets=" <> groupThousands (totalTargets r.total) <> " errors=" <> groupThousands errors <> " (" <> percent ratio <> ")",
        "  push/s avg=" <> rateText avgRate <> " max=" <> rateText r.maxPushRate,
        "  latency (cumulative, incl. warmup) p50=" <> formatLatency r.p50 <> " p99=" <> formatLatency r.p99
      ]

formatConsumeStatus :: ReaderMode -> TickReport -> ConsumeReport -> Text
formatConsumeStatus mode r c =
  T.intercalate " | " $
    [ "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> rateText r.maxPushRate,
      "cons/s cur=" <> rateText c.consumeRate <> " max=" <> rateText c.maxConsumeRate
    ]
      <> ["ack/s cur=" <> rateText c.ackRate <> " max=" <> rateText c.maxAckRate <> " p99(cum.)=" <> formatLatency c.ackP99 | acking]
      <> [ "lag(cum.) p50=" <> formatLatency c.lagP50 <> " p99=" <> formatLatency c.lagP99,
           "backlog=" <> groupThousands c.backlog,
           "err/s push=" <> rateText r.errorRate <> " fetch=" <> rateText c.fetchErrorRate <> (if acking then " ack=" <> rateText c.ackErrorRate else "")
         ]
  where
    acking = mode /= NoAcks

-- | Seconds from start until the writers stopped, and the notifications
-- delivered by then, and the ack upserts by then.
data WriterPhase = WriterPhase {duration :: Double, delivered :: Int, acks :: Int}

-- | How the drain after the writer phase ended, with its duration in seconds.
data Drain = DrainComplete Double | DrainTimedOut Double | DrainInterrupted

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
        [ "  acks=" <> groupThousands (totalAcks c.readerTotal) <> " errors=" <> groupThousands (totalAckErrors c.readerTotal) <> " ack/s avg=" <> rateText avgAckRate <> " max=" <> rateText c.maxAckRate,
          "  ack latency (cumulative) p50=" <> formatLatency c.ackP50 <> " p99=" <> formatLatency c.ackP99
        ]
      seconds :: Double -> Text
      seconds = T.pack . printf "%.1fs"
      drainText = case drain of
        DrainComplete t -> "complete after " <> seconds t
        DrainTimedOut t -> "timed out after " <> seconds t
        DrainInterrupted -> "interrupted"
   in formatSummary r {elapsed = phase.duration}
        <> [ "consume: delivered="
               <> groupThousands delivered
               <> " expected="
               <> groupThousands expected
               <> " missing="
               <> groupThousands (max 0 (expected - delivered))
               <> " duplicates="
               <> groupThousands (max 0 (delivered - expected)),
             "  cons/s avg=" <> rateText avgRate <> " max=" <> rateText c.maxConsumeRate,
             "  lag (cumulative) p50=" <> formatLatency c.lagP50 <> " p99=" <> formatLatency c.lagP99,
             "  fetch (cumulative) p50=" <> formatLatency c.fetchP50 <> " p99=" <> formatLatency c.fetchP99 <> " empty=" <> percent emptyRatio
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
