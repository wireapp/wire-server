module FanInPerf.Terminal
  ( groupThousands,
    formatLatency,
    formatStatus,
    formatSummary,
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
import FanInPerf.Stats
import Imports
import Text.Printf (printf)

groupThousands :: Int -> Text
groupThousands n
  | n < 0 = "-" <> groupThousands (negate n)
  | otherwise = T.intercalate " " . reverse . map T.reverse . T.chunksOf 3 . T.reverse . T.pack $ show n

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

clearLine :: Text
clearLine = "\r\ESC[2K"

renderStatusLine :: Bool -> Text -> Text
renderStatusLine tty line = if tty then clearLine <> line else line <> "\n"

renderLogLine :: Bool -> Text -> Text
renderLogLine tty line = (if tty then clearLine else "") <> line <> "\n"

newtype Console = Console {isTty :: Bool}

newConsole :: IO Console
newConsole = do
  tty <- hIsTerminalDevice stdout
  hSetBuffering stdout (BlockBuffering Nothing)
  pure (Console tty)

drawStatus :: Console -> Text -> IO ()
drawStatus c line = T.putStr (renderStatusLine c.isTty line) >> hFlush stdout

printLine :: Console -> Text -> IO ()
printLine c line = T.putStr (renderLogLine c.isTty line) >> hFlush stdout
