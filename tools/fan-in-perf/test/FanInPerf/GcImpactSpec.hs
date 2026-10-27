module FanInPerf.GcImpactSpec (spec) where

import Data.Map.Strict qualified as Map
import FanInPerf.GcImpact
import Imports
import Test.Hspec

sample :: Double -> Double -> TickSample
sample at push = TickSample {at, push, ack = push / 2, ackP99 = Just 0.01, lagP99 = Just 0.1, backlog = 10}

-- | A drop of the given table.
dropOf :: Text -> Double -> DropEvent
dropOf table at = DropEvent {table, at}

-- | A drop of @user_notifications@.
dropAt :: Double -> DropEvent
dropAt = dropOf "user_notifications"

-- | A tick with the given ack p99.
sampleP99 :: Double -> Maybe Double -> TickSample
sampleP99 at ackP99 = TickSample {at, push = 1, ack = 1, ackP99, lagP99 = Just 0.1, backlog = 10}

spec :: Spec
spec = describe "gcImpact" $ do
  let ticks = [sample t (if t >= 18 && t <= 22 then 100 else 1000) | t <- [1 .. 40]]

  it "splits the ticks around a drop from all others" $ do
    let g = gcImpact 5 40 [dropAt 20] ticks
    g.drops `shouldBe` 1
    g.within.seconds `shouldBe` 11 -- 15 .. 25
    g.other.seconds `shouldBe` 24 -- 6 .. 14 and 26 .. 40
    g.within.pushRate `shouldBe` Just 1000
    g.other.pushRate `shouldBe` Just 1000

  it "shows a dip around the drop" $ do
    let dip = [sample t (if abs (t - 20) <= 5 then 100 else 1000) | t <- [1 .. 40]]
        g = gcImpact 5 40 [dropAt 20] dip
    g.within.pushRate `shouldBe` Just 100
    g.other.pushRate `shouldBe` Just 1000
    g.within.ackRate `shouldBe` Just 50

  it "ignores the warmup and the time after the writers stopped" $ do
    let g = gcImpact 5 30 [] ticks
    g.within.seconds `shouldBe` 0
    g.other.seconds `shouldBe` 25 -- 6 .. 30
  it "puts every counted tick in 'other' without drops" $ do
    let g = gcImpact 0 40 [] ticks
    g.drops `shouldBe` 0
    (g.within.seconds, g.other.seconds) `shouldBe` (0, 40)
    g.within.pushRate `shouldBe` Nothing

  it "takes the median, the mean of the middle two for an even count" $ do
    let g = gcImpact 0 4 [] [sample 1 10, sample 2 20, sample 3 30, sample 4 1000]
    g.other.pushRate `shouldBe` Just 25

  it "leaves percentiles of seconds without data out, and the maximum of the ack p99" $ do
    let g = gcImpact 0 3 [] [sampleP99 1 (Just 0.002), sampleP99 2 Nothing, sampleP99 3 (Just 0.008)]
    g.other.ackP99 `shouldBe` Just 0.005
    g.other.ackP99Max `shouldBe` Just 0.008
    (gcImpact 0 3 [] [sampleP99 1 Nothing]).other.ackP99Max `shouldBe` Nothing

  it "includes a tick exactly at the edge of the window" $
    (gcImpact 0 40 [dropAt 20] ticks).within.seconds `shouldBe` 11

  it "counts the drops per table and in total" $ do
    let g = gcImpact 0 40 [dropOf "user_notifications" 10, dropOf "team_notifications" 10, dropOf "user_notifications" 30] ticks
    g.drops `shouldBe` 3
    g.dropsByTable `shouldBe` Map.fromList [("team_notifications", 1), ("user_notifications", 2)]

  it "puts a tick within the window of the drop of any table" $ do
    let g = gcImpact 0 40 [dropOf "team_notifications" 20] ticks
    g.within.seconds `shouldBe` 11

  describe "detectDrops" $ do
    let polls = Map.fromList
    it "finds nothing when no oldest partition changed" $
      detectDrops 7 (polls [("a", "p1"), ("b", "p1")]) (polls [("a", "p1"), ("b", "p1")]) `shouldBe` []
    it "finds the tables whose oldest partition changed, in the order of the tables" $
      detectDrops 7 (polls [("a", "p1"), ("b", "p1"), ("c", "p1")]) (polls [("a", "p2"), ("b", "p1"), ("c", "p3")])
        `shouldBe` [dropOf "a" 7, dropOf "c" 7]
    it "finds all six tables dropping in one poll" $ do
      let tables = ["client", "epoch", "local", "remote", "team", "user"]
      length (detectDrops 1 (polls [(t, "p1") | t <- tables]) (polls [(t, "p2") | t <- tables])) `shouldBe` 6
    it "counts a table that lost its last partition as dropped" $
      detectDrops 7 (polls [("a", "p1")]) (polls [("a", "")]) `shouldBe` [dropOf "a" 7]
    it "treats a table missing from one poll as having no partition" $ do
      detectDrops 7 (polls [("a", "p1")]) (polls []) `shouldBe` [dropOf "a" 7]
      detectDrops 7 (polls []) (polls [("a", "p1")]) `shouldBe` [dropOf "a" 7]
      detectDrops 7 (polls [("a", "")]) (polls []) `shouldBe` []
