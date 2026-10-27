module FanInPerf.Terminal
  ( groupThousands,
    formatLatency,
    formatStatus,
    renderSummary,
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
import FanInPerf.ConsumeStats
import FanInPerf.Options (ReaderMode (..))
import FanInPerf.Stats
import FanInPerf.Summary
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

percentText :: Double -> Text
percentText x = T.pack (printf "%.2f%%" (x * 100))

rateText :: Double -> Text
rateText = groupThousands . round

secondsText :: Double -> Text
secondsText = T.pack . printf "%.1fs"

-- | p50 and p99 of a histogram.
pcts :: Percentiles -> Text
pcts q = pct "p50" q.p50 <> " " <> pct "p99" q.p99

-- | Status line segment shared by produce and consume.
pushSegment :: TickReport -> Text
pushSegment r = "t=" <> T.pack (show (round r.elapsed :: Int)) <> "s push/s cur=" <> rateText r.pushRate <> " max=" <> maxText r.maxPushRate

formatStatus :: TickReport -> Text
formatStatus r =
  T.intercalate
    " | "
    [ pushSegment r,
      "err/s cur=" <> rateText r.errorRate <> " (" <> percentText r.errorRatio <> ")",
      "targets/s cur=" <> rateText r.targetRate,
      pct "push p50" r.p50 <> " " <> pct "p99" r.p99
    ]

formatSummary :: TickReport -> [Text]
formatSummary = renderSummary . produceSummary

ackSegment :: ConsumeReport -> Text
ackSegment c = "ack/s cur=" <> rateText c.ackRate <> " max=" <> maxText c.maxAckRate <> " " <> pct "p99" c.ackP99

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

formatConsumeSummary :: ReaderMode -> Int -> WriterPhase -> Drain -> TickReport -> ConsumeReport -> [Text]
formatConsumeSummary mode expected phase drain r c = renderSummary (consumeSummary mode expected phase drain r c)

formatConsumeOnlySummary :: ReaderMode -> Int -> Double -> ConsumeEnd -> [(Int, Maybe Double)] -> ConsumeReport -> [Text]
formatConsumeOnlySummary mode expected elapsed end milestones c = renderSummary (consumeOnlySummary mode expected elapsed end milestones c)

-- | The summary printed at the end of every run.
renderSummary :: RunSummary -> [Text]
renderSummary s =
  ["summary: duration=" <> secondsText s.duration]
    <> maybe [] pushLines s.pushes
    <> maybe [] (deliveryLines s.command) s.deliveries
    <> maybe [] fetchGateLines s.fetch
    <> maybe [] behindWarning s.deliveries
    <> maybe [] ackLines s.acks
    <> ["  drain: " <> drainText d | d <- toList s.drain]
    <> ["  end: " <> endText s.duration e | e <- toList s.end]

pushLines :: PushSummary -> [Text]
pushLines p =
  [ "  pushes=" <> groupThousands p.pushes <> " targets=" <> groupThousands p.targets <> " errors=" <> groupThousands p.errors <> " (" <> percentText p.errorRatio <> ")",
    "  push/s avg=" <> rateText p.rateAvg <> " max=" <> maybe "-" rateText p.rateMax <> " (avg incl. warmup, max after warmup)",
    "  latency (cumulative, incl. warmup) " <> pcts p.latency
  ]

-- | @label: delivered=.. expected=.. missing=.. duplicates=..@ (net counts),
-- then what differs between the runs: with writers the averages cover the
-- writer phase, without them the whole run (milestones show its tail).
deliveryLines :: Text -> DeliverySummary -> [Text]
deliveryLines command d =
  ( command
      <> ": delivered="
      <> groupThousands d.delivered
      <> " expected="
      <> groupThousands d.expected
      <> " missing="
      <> groupThousands d.missing
      <> " duplicates="
      <> groupThousands d.duplicates
  )
    : case d.backlogAtWriterStop of
      Just backlog ->
        [ "  cons/s avg=" <> rateText d.rateAvg <> " max=" <> maybe "-" rateText d.rateMax <> " (avg incl. warmup, max after warmup, both writer phase)",
          "  fan-out: " <> T.pack (printf "%.1f" (fromMaybe 0 d.fanOut)) <> " deliveries per push (cons/s and push/s differ by this factor)",
          "  backlog at writer stop: " <> groupThousands backlog <> (if d.rateAvg > 0 then " (" <> secondsText (ratio backlog d.rateAvg) <> " of delivery)" else "")
        ]
          <> ["  lag (cumulative) " <> pcts q | q <- toList d.lag]
      Nothing ->
        [ "  cons/s avg=" <> rateText d.rateAvg <> " max=" <> maybe "-" rateText d.rateMax <> " (avg incl. warmup and tail, max after warmup)",
          "  time to delivered: " <> T.intercalate " " [T.pack (show m.percent) <> "%=" <> maybe "-" secondsText m.seconds | m <- d.milestones]
        ]

-- | Cumulative fetch and gate wait summary lines.
fetchGateLines :: FetchSummary -> [Text]
fetchGateLines f =
  [ "  fetch (cumulative) " <> pcts f.latency <> " empty=" <> percentText f.emptyRatio <> " errors=" <> groupThousands f.errors,
    "  gate wait (cumulative) " <> pcts f.gateWait
  ]

behindWarning :: DeliverySummary -> [Text]
behindWarning d =
  [ "  warning: readers fell behind the writers (backlog above 1s of delivery at writer stop); missing then understates the cursor race, as readers stay below the head"
  | Just backlog <- [d.backlogAtWriterStop],
    d.rateAvg > 0 && fromIntegral backlog > d.rateAvg
  ]

ackLines :: AckSummary -> [Text]
ackLines a =
  [ "  acks=" <> groupThousands a.acks <> " errors=" <> groupThousands a.errors <> " ack/s avg=" <> rateText a.rateAvg <> " max=" <> maybe "-" rateText a.rateMax,
    "  ack latency (cumulative) " <> pcts a.latency
  ]

drainText :: Drain -> Text
drainText = \case
  DrainComplete t -> "complete after " <> secondsText t
  DrainStalled t -> "stalled after " <> secondsText t <> " (no delivery for several poll intervals: the rest is lost, not in flight)"
  DrainTimedOut t -> "timed out after " <> secondsText t
  DrainInterrupted -> "interrupted"

endText :: Double -> ConsumeEnd -> Text
endText elapsed = \case
  ConsumeFinished -> "all readers caught up after " <> secondsText elapsed
  ConsumeTimedOut -> "timed out after " <> secondsText elapsed
  ConsumeInterrupted -> "interrupted after " <> secondsText elapsed

formatCoverage :: [(TargetKind, Double)] -> [Text]
formatCoverage = map line
  where
    line (k, c) =
      "coverage "
        <> kindName k
        <> ": "
        <> percentText c
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
