module FanInPerf.PopulationSpec (spec) where

import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Population
import FanInPerf.Targets hiding (spec)
import Imports
import System.Random (mkStdGen)
import Test.Hspec
import Wire.FanInNotificationsStore (FanInPush (..))

entriesOf :: Int -> [TargetConfig] -> V.Vector Entry
entriesOf clientsPerUser cfgs = fst (mkEntries clientsPerUser (NE.fromList cfgs) (mkStdGen 7))

population :: Int -> Int -> V.Vector Entry -> Population
population n e es = either (error . T.unpack) id (mkPopulation n e es)

-- the only push a single-entry spec can produce, for any seed
pushOf :: V.Vector Entry -> FanInPush
pushOf es = fst (genPush (mkPayload 0) es (mkStdGen 1))

spec :: Spec
spec = do
  describe "mkPopulation" $ do
    it "spreads clients over a user pool; shared streams are allowed" $ do
      let p = population 25 3 (entriesOf 1 [TargetConfig KindUser 10 1])
      all (\c -> length c.streams == 1 && all ((== KindUser) . streamKind) c.streams) p.simClients `shouldBe` True
      length p.simClients `shouldBe` 25
      sum (Map.elems p.subscribers) `shouldBe` 25
      all (`elem` [2, 3]) (Map.elems p.subscribers) `shouldBe` True

    it "puts every client on the single team of a one-team pool" $ do
      let es = entriesOf 1 [TargetConfig KindTeam 1 1]
          p = population 25 3 es
      expectedDeliveries p.subscribers (pushOf es) `shouldBe` 25

    it "gives each client E distinct consecutive epoch streams" $ do
      let p = population 10 3 (entriesOf 1 [TargetConfig KindEpoch 10 1])
      all (\c -> length (nubOrd (map show c.streams)) == 3) p.simClients `shouldBe` True
      sum (Map.elems p.subscribers) `shouldBe` 30

    it "rejects more epochs per client than epoch streams" $
      isLeft (mkPopulation 10 11 (entriesOf 1 [TargetConfig KindEpoch 10 1])) `shouldBe` True

    it "assigns client ids of a clients pool by i div S, and rejects too many clients" $ do
      let es = entriesOf 2 [TargetConfig KindClients 4 1]
      -- 4 clients: all client id 1 of their user; a push to client ids [1,2] reaches 1 subscriber
      expectedDeliveries (population 4 3 es).subscribers (pushOf es) `shouldBe` 1
      -- 8 clients: both client ids subscribed
      expectedDeliveries (population 8 3 es).subscribers (pushOf es) `shouldBe` 2
      isLeft (mkPopulation 9 3 es) `shouldBe` True

    it "rejects the connections kind" $
      isLeft (mkPopulation 5 3 (entriesOf 1 [TargetConfig KindConnections 5 1])) `shouldBe` True

    it "combines kinds per client" $ do
      let p = population 6 2 (entriesOf 1 [TargetConfig KindUser 6 1, TargetConfig KindTeam 2 1, TargetConfig KindEpoch 4 1])
      all (\c -> map streamKind c.streams == [KindUser, KindTeam, KindEpoch, KindEpoch]) p.simClients `shouldBe` True

  describe "expectedDeliveries" $
    it "is 0 for a push to a stream nobody reads" $ do
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
          p = population 20 3 es
          -- stream pools are fixed, so scan pushes until one hits an unread stream
          counts = [expectedDeliveries p.subscribers (fst (genPush (mkPayload 0) es (mkStdGen s))) | s <- [1 .. 200]]
      (0 `elem` counts, all (`elem` [0, 1]) counts) `shouldBe` (True, True)

  describe "coverage" $ do
    it "is the share of pool streams with a reader" $
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
       in coverage (population 20 3 es).subscribers es `shouldBe` [(KindUser, 0.2)]
    it "is 1 when every stream has a reader" $
      let es = entriesOf 1 [TargetConfig KindTeam 1 1, TargetConfig KindUser 10 1]
       in map snd (coverage (population 10 3 es).subscribers es) `shouldBe` [1, 1]
