module FanInPerf.TargetsSpec (spec) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Qualified (qDomain)
import Data.Vector qualified as V
import FanInPerf.TargetSpecParser (parseTargetSpec)
import FanInPerf.Targets hiding (Entry (..))
import FanInPerf.Targets qualified as FIP (Entry (..))
import Imports
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wire.API.MLS.Group (GroupId (..))
import Wire.FanInNotificationsStore (FanInPush (..), Target (..))

spec :: Spec
spec = do
  describe "renderTargetSpec" $
    it "round-trips" $
      let spec' = TargetConfig KindUser 1000 20 :| [TargetConfig KindTeam 10 1]
       in parseTargetSpec (renderTargetSpec spec') `shouldBe` Right spec'

  describe "kindName" $
    it "is unique per kind" $
      length (nubOrd (map kindName allKinds)) `shouldBe` length allKinds

  describe "sampleDistinct" $
    prop "returns k distinct ascending indices in [0, n)" $ \(Positive n0) (Positive k0) seed ->
      let n = min 500 n0
          k = 1 + (k0 - 1) `mod` n
          (xs, _) = sampleDistinct n k (mkStdGen seed)
       in length xs == k
            && length (nubOrd xs) == k
            && xs == sort xs
            && all (\x -> x >= 0 && x < n) xs

  describe "genTargets" $ do
    let specs =
          TargetConfig KindUser 50 5
            :| [ TargetConfig KindClients 20 3,
                 TargetConfig KindTeam 4 1,
                 TargetConfig KindEpoch 10 2,
                 TargetConfig KindConnections 30 7
               ]
        perPushOf k = maybe 0 (.perPush) (find ((== k) . (.kind)) specs)

    prop "pushes have one kind, K distinct targets, keys from the entry's pool" $ \seed ->
      let (entries, g0) = mkEntries 2 specs (mkStdGen seed)
          poolKeys k =
            case V.find ((== k) . (.spec.kind)) entries of
              Nothing -> []
              Just e -> [targetKey (targetAt e.pool i) | i <- [0 .. e.spec.streams - 1]]
          pushes = take 200 (unfoldr (Just . genPush (mkPayload 0) entries) g0)
          ok push =
            let ts = push.targets
                kind = pushKind push
                keys = map targetKey ts
             in all ((== kind) . targetKind) ts
                  && length ts == perPushOf kind
                  && length (nubOrd keys) == length keys
                  && all (`elem` poolKeys kind) keys
       in all ok pushes

    prop "targets within a push are in ascending pool order" $ \seed ->
      let (entries, g0) = mkEntries 3 specs (mkStdGen seed)
          poolKeys k = case V.find ((== k) . (.spec.kind)) entries of
            Nothing -> []
            Just e -> [targetKey (targetAt e.pool i) | i <- [0 .. e.spec.streams - 1]]
          pushes = take 200 (unfoldr (Just . genPush (mkPayload 0) entries) g0)
          ascending push =
            let ts = push.targets
                kind = pushKind push
                positions = mapMaybe (\t -> elemIndex (targetKey t) (poolKeys kind)) ts
             in positions == sort positions && length positions == length ts
       in all ascending pushes

    it "uses every entry eventually" $
      let (entries, g0) = mkEntries 1 specs (mkStdGen 42)
          kinds = map pushKind (take 500 (unfoldr (Just . genPush (mkPayload 0) entries) g0))
       in nubOrd kinds `shouldMatchList` allKinds

    it "gives clients targets the configured number of client ids" $
      let (entries, g0) = mkEntries 3 (TargetConfig KindClients 5 2 :| []) (mkStdGen 7)
          (ts, _) = genTargets entries g0
       in [length cs | TargetUserClients (_, cs) <- NE.toList ts] `shouldBe` [3, 3]

    it "generates 32-byte binary group ids" $
      let (entries, g0) = mkEntries 1 (TargetConfig KindEpoch 5 1 :| []) (mkStdGen 9)
          (ts, _) = genTargets entries g0
       in [BS.length gid.unGroupId | TargetEpoch (gid, _) <- NE.toList ts] `shouldBe` [32]

    it "qualifies connection targets with the local domain" $
      let (entries, g0) = mkEntries 1 (TargetConfig KindConnections 5 1 :| []) (mkStdGen 3)
          (ts, _) = genTargets entries g0
       in [qDomain q | TargetConnections q <- NE.toList ts] `shouldBe` [localDomain]

  describe "mkPayload" $
    it "has roughly the requested size" $
      let size = LBS.length (A.encode (mkPayload 512))
       in size `shouldSatisfy` (\s -> s >= 512 && s < 600)
