module FanInPerf.TargetsSpec (spec) where

import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import FanInPerf.Targets
import Imports
import Test.Hspec

spec :: Spec
spec = do
  describe "parseTargetSpec" $ do
    it "parses a single entry with default targets per push" $
      parseTargetSpec "team:10" `shouldBe` Right (TargetEntry KindTeam 10 1 :| [])

    it "parses several entries with targets per push" $
      parseTargetSpec "user:1000x20,team:10"
        `shouldBe` Right (TargetEntry KindUser 1000 20 :| [TargetEntry KindTeam 10 1])

    it "parses every kind" $
      fmap (fmap (.kind)) (parseTargetSpec "user:1,clients:1,team:1,epoch:1,connections:1")
        `shouldBe` Right (KindUser :| [KindClients, KindTeam, KindEpoch, KindConnections])

    it "accepts K == STREAMS" $
      parseTargetSpec "user:5x5" `shouldBe` Right (TargetEntry KindUser 5 5 :| [])

    it "accepts the maximum number of streams" $
      fmap (fmap (.streams)) (parseTargetSpec "team:10000000") `shouldBe` Right (10_000_000 :| [])

    forM_
      [ "",
        "team",
        "team:",
        "team:0",
        "team:10x0",
        "team:10x11",
        "team:-1",
        "team:1x2x3",
        "bogus:10",
        "team:10,team:5",
        "team:abc",
        "team:10,",
        "team:10 ",
        "team:99999999999999999999999",
        "team:99999999999999999999",
        "team:10000001",
        "user:50000000",
        "team:10x99999999999999999999"
      ]
      $ \bad ->
        it ("rejects " <> show bad) $
          parseTargetSpec bad `shouldSatisfy` isLeft

    it "gives a clear message for absurd numbers" $
      parseTargetSpec "team:99999999999999999999"
        `shouldBe` Left "expected a number between 1 and 10000000, got: 99999999999999999999"

  describe "renderTargetSpec" $
    it "round-trips" $
      let spec' = TargetEntry KindUser 1000 20 :| [TargetEntry KindTeam 10 1]
       in parseTargetSpec (renderTargetSpec spec') `shouldBe` Right spec'

  describe "kindName" $
    it "is unique per kind" $
      length (nubOrd (map kindName allKinds)) `shouldBe` length allKinds
