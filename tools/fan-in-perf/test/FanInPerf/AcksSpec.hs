module FanInPerf.AcksSpec (spec) where

import Data.Id
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import FanInPerf.Acks
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wire.FanInNotificationsStore (Stream (..))

user, team :: Stream
user = StreamUser (Id (UUID.fromWords 5 6 7 8))
team = StreamTeam (Id (UUID.fromWords 1 2 3 4))

nid :: Word32 -> UUID
nid = UUID.fromWords 0 0 0

-- | Delivers in order, ignoring due acks.
deliverAll :: Int -> [(Stream, Word32)] -> AckState -> AckState
deliverAll n xs st = foldl' (\s (str, i) -> snd (deliver n str (nid i) s)) st xs

byStream :: [Pending] -> [Pending]
byStream = sortOn (show . (.stream))

spec :: Spec
spec = do
  describe "deliver" $ do
    it "acks every notification with N = 1" $ do
      let (due, st) = deliver 1 user (nid 1) emptyAckState
      due `shouldBe` [Pending user (nid 1) 1]
      unacked st `shouldBe` 0

    it "is due nothing below N and counts over all streams" $ do
      let (d1, s1) = deliver 3 user (nid 1) emptyAckState
          (d2, s2) = deliver 3 team (nid 2) s1
      (d1, d2, unacked s2) `shouldBe` ([], [], 2)

    it "at the N-th returns every pending stream with its maximum id and clears the state" $ do
      let st = deliverAll 4 [(user, 1), (team, 5), (user, 3)] emptyAckState
          (due, st') = deliver 4 team (nid 2) st
      byStream due `shouldBe` byStream [Pending user (nid 3) 2, Pending team (nid 5) 2]
      st' `shouldBe` emptyAckState

  describe "restoreFailed" $ do
    it "keeps a failed ack's id pending but starts counting from zero" $ do
      let st = restoreFailed [Pending user (nid 9) 3] emptyAckState
      unacked st `shouldBe` 0
      let (d1, st1) = deliver 2 team (nid 5) st
      d1 `shouldBe` []
      byStream (fst (deliver 2 team (nid 6) st1)) `shouldBe` byStream [Pending user (nid 9) 0, Pending team (nid 6) 2]
    it "merges with new deliveries of the stream: higher id, only the new count" $ do
      let st = restoreFailed [Pending user (nid 9) 3] (deliverAll 10 [(user, 4)] emptyAckState)
      unacked st `shouldBe` 1
      fst (deliver 3 user (nid 5) st) `shouldBe` []
      fst (deliver 2 user (nid 5) st) `shouldBe` [Pending user (nid 9) 2]

  prop "unacked equals the deliveries since the last ack point" $ \(Positive n0) (xs :: [Bool]) ->
    let n = 1 + n0 `mod` 10
        step (st, k) b =
          let (due, st') = deliver n (if b then user else team) (nid 1) st
           in (st', if null due then k + 1 else 0)
        (final, since) = foldl' step (emptyAckState, 0 :: Int) xs
     in unacked final == since
