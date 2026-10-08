module FanInPerf.TargetsSpec (spec) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Domain (Domain (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Qualified (qDomain)
import Data.Vector qualified as V
import FanInPerf.Targets hiding (Entry (..))
import FanInPerf.Targets qualified as FIP (Entry (..))
import Imports
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wire.API.MLS.Group (GroupId (..))
import Wire.FanInNotificationsStore (Target (..))

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

  describe "sampleDistinct" $
    prop "returns k distinct indices in [0, n)" $ \(Positive n0) (Positive k0) seed ->
      let n = min 500 n0
          k = 1 + (k0 - 1) `mod` n
          (xs, _) = sampleDistinct n k (mkStdGen seed)
       in length xs == k
            && length (nubOrd xs) == k
            && all (\x -> x >= 0 && x < n) xs

  describe "genTargets" $ do
    let dom = Domain "example.com"
        specs =
          TargetEntry KindUser 50 5
            :| [ TargetEntry KindClients 20 3,
                 TargetEntry KindTeam 4 1,
                 TargetEntry KindEpoch 10 2,
                 TargetEntry KindConnections 30 7
               ]
        perPushOf k = maybe 0 (.perPush) (find ((== k) . (.kind)) specs)

    prop "pushes have one kind, K distinct targets, keys from the entry's pool" $ \seed ->
      let (entries, g0) = mkEntries 2 specs (mkStdGen seed)
          poolKeys k =
            case V.find ((== k) . (.spec.kind)) entries of
              Nothing -> []
              Just e -> [targetKey (targetAt dom e.pool i) | i <- [0 .. e.spec.streams - 1]]
          pushes = take 200 (unfoldr (Just . genTargets dom entries) g0)
          ok (kind, ts) =
            let keys = map targetKey (NE.toList ts)
             in all ((== kind) . targetKind) ts
                  && length ts == perPushOf kind
                  && length (nubOrd keys) == length keys
                  && all (`elem` poolKeys kind) keys
       in all ok pushes

    it "uses every entry eventually" $
      let (entries, g0) = mkEntries 1 specs (mkStdGen 42)
          kinds = map fst (take 500 (unfoldr (Just . genTargets dom entries) g0))
       in nubOrd kinds `shouldMatchList` allKinds

    it "gives clients targets the configured number of client ids" $
      let (entries, g0) = mkEntries 3 (TargetEntry KindClients 5 2 :| []) (mkStdGen 7)
          ((_, ts), _) = genTargets dom entries g0
       in [length cs | TargetUserClients (_, cs) <- NE.toList ts] `shouldBe` [3, 3]

    it "generates 32-byte binary group ids" $
      let (entries, g0) = mkEntries 1 (TargetEntry KindEpoch 5 1 :| []) (mkStdGen 9)
          ((_, ts), _) = genTargets dom entries g0
       in [BS.length gid.unGroupId | TargetEpoch (gid, _) <- NE.toList ts] `shouldBe` [32]

    it "qualifies connection targets with the local domain" $
      let (entries, g0) = mkEntries 1 (TargetEntry KindConnections 5 1 :| []) (mkStdGen 3)
          ((_, ts), _) = genTargets dom entries g0
       in [qDomain q | TargetConnections q <- NE.toList ts] `shouldBe` [dom]

  describe "mkPayload" $
    it "has roughly the requested size" $
      let size = LBS.length (A.encode (mkPayload 512))
       in size `shouldSatisfy` (\s -> s >= 512 && s < 600)
