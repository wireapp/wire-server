module FanInPerf.TerminalSpec (spec) where

import Data.Text qualified as T
import FanInPerf.Stats
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

spec :: Spec
spec = do
  describe "groupThousands" $
    it "groups digits by three" $
      map groupThousands [0, 999, 1000, 1234567, -9105]
        `shouldBe` ["0", "999", "1 000", "1 234 567", "-9 105"]

  describe "formatLatency" $
    it "formats ms, seconds and missing values" $
      map formatLatency [Nothing, Just 0.0031, Just 2.5]
        `shouldBe` ["-", "3.1ms", "2.50s"]

  describe "formatStatus" $
    it "renders the status line" $
      formatStatus report
        `shouldBe` "t=42s push/s cur=8 312 max=9 105 | err/s cur=3 (0.04%) | targets/s cur=41 560 | p50=3.1ms p99=12.4ms"

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
