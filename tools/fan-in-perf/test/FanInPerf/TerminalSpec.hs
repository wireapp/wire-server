module FanInPerf.TerminalSpec (spec) where

import Data.Text qualified as T
import FanInPerf.ConsumeStats
import FanInPerf.Stats
import FanInPerf.Targets (TargetKind (..))
import FanInPerf.Terminal
import Imports
import Test.Hspec

report :: TickReport
report =
  TickReport
    { elapsed = 42.2,
      pushRate = 8312.4,
      maxPushRate = 9105,
      targetRate = 41560,
      errorRate = 3,
      errorRatio = 0.0004,
      p50 = Just 0.0031,
      p99 = Just 0.0124,
      delta = emptySnapshot,
      total = emptySnapshot
    }

creport :: ConsumeReport
creport =
  ConsumeReport
    { consumeRate = 8000.2,
      maxConsumeRate = 9000,
      fetchRate = 20000,
      emptyFetchRatio = 0.5,
      fetchErrorRate = 1,
      lagP50 = Just 0.004,
      lagP99 = Just 0.05,
      fetchP50 = Just 0.001,
      fetchP99 = Just 0.01,
      backlog = 1234,
      readerDelta = emptyReaderSnapshot,
      readerTotal = emptyReaderSnapshot
    }

deliveredSnapshot :: IO ReaderSnapshot
deliveredSnapshot = do
  rs <- newReaderStats
  recordFetch rs KindUser 1000 [1000, 1000, 1000]
  recordFetch rs KindUser 1000 []
  readReaderSnapshot rs

pushSnapshot :: Int -> IO Snapshot
pushSnapshot n = do
  ws <- newWriterStats
  replicateM_ n (recordSuccess ws KindUser 1 1000)
  readSnapshot ws

-- 30s of writing (the report's 42.2s include the drain), 600 delivered by then
phase :: WriterPhase
phase = WriterPhase {duration = 30, delivered = 600}

spec :: Spec
spec = do
  describe "groupThousands" $
    it "groups digits by three" $
      map groupThousands [0, 999, 1000, 1234567, -9105]
        `shouldBe` ["0", "999", "1,000", "1,234,567", "-9,105"]

  describe "formatLatency" $
    it "formats ms, seconds and missing values" $
      map formatLatency [Nothing, Just 0.0031, Just 2.5]
        `shouldBe` ["-", "3.1ms", "2.50s"]

  describe "formatStatus" $
    it "renders the status line" $
      formatStatus report
        `shouldBe` "t=42s push/s cur=8,312 max=9,105 | err/s cur=3 (0.04%) | targets/s cur=41,560 | p50(cum.)=3.1ms p99(cum.)=12.4ms"

  describe "renderStatusLine" $ do
    it "redraws in place on a TTY" $
      renderStatusLine True "x" `shouldBe` "\r\ESC[2Kx"
    it "prints plain lines without escape codes otherwise" $ do
      renderStatusLine False "x" `shouldBe` "x\n"
      T.any (== '\ESC') (renderStatusLine False (formatStatus report)) `shouldBe` False

  describe "renderLogLine" $ do
    it "clears the status line first on a TTY" $
      renderLogLine True "err" `shouldBe` "\r\ESC[2Kerr\n"
    it "is a plain line otherwise" $
      renderLogLine False "err" `shouldBe` "err\n"

  describe "formatSummary" $
    it "survives an empty run" $
      formatSummary report {elapsed = 0, p50 = Nothing, p99 = Nothing}
        `shouldSatisfy` (not . any (T.isInfixOf "NaN"))

  describe "formatConsumeStatus" $
    it "renders the consume status line" $
      formatConsumeStatus report creport
        `shouldBe` "t=42s push/s cur=8,312 max=9,105 | cons/s cur=8,000 max=9,000 | lag(cum.) p50=4.0ms p99=50.0ms | backlog=1,234 | err/s push=3 fetch=1"

  describe "formatConsumeSummary" $ do
    it "reports missing notifications and a timed-out drain" $ do
      s <- deliveredSnapshot
      let out = formatConsumeSummary 5 phase (DrainTimedOut 10) report creport {readerTotal = s}
      out `shouldSatisfy` any (T.isInfixOf "delivered=3 expected=5 missing=2 duplicates=0")
      out `shouldSatisfy` any (T.isInfixOf "drain: timed out after 10.0s")
    it "reports duplicates and a complete drain" $ do
      s <- deliveredSnapshot
      let out = formatConsumeSummary 2 phase (DrainComplete 1.23) report creport {readerTotal = s}
      out `shouldSatisfy` any (T.isInfixOf "missing=0 duplicates=1")
      out `shouldSatisfy` any (T.isInfixOf "drain: complete after 1.2s")
    it "reports an interrupted drain" $
      formatConsumeSummary 0 phase DrainInterrupted report creport
        `shouldSatisfy` any (T.isInfixOf "drain: interrupted")
    it "takes duration and averages from the writer phase, not the drain" $ do
      s <- pushSnapshot 300
      let out = formatConsumeSummary 0 phase (DrainTimedOut 10) report {total = s} creport
      out `shouldSatisfy` any (T.isInfixOf "duration=30.0s")
      out `shouldSatisfy` any (T.isInfixOf "push/s avg=10 ")
      out `shouldSatisfy` any (T.isInfixOf "cons/s avg=20 ")
    it "survives an empty run" $
      formatConsumeSummary 0 WriterPhase {duration = 0, delivered = 0} (DrainComplete 0) report {elapsed = 0} creport {lagP50 = Nothing, lagP99 = Nothing}
        `shouldSatisfy` (not . any (T.isInfixOf "NaN"))

  describe "formatCoverage" $
    it "warns below 100%" $
      formatCoverage [(KindUser, 0.02), (KindTeam, 1)]
        `shouldBe` [ "coverage user: 2.00% of streams have a reader (warning: consumed/s will stay below targets/s)",
                     "coverage team: 100.00% of streams have a reader"
                   ]
