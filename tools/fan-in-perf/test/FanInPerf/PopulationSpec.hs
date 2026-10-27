module FanInPerf.PopulationSpec (spec) where

import Data.Containers.ListUtils (nubOrd)
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.NotificationsStore (FanInPush (..), Stream (..), Target (..))
import FanInPerf.Population
import FanInPerf.Targets hiding (spec)
import Imports
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

entriesOf :: Int -> [TargetConfig] -> V.Vector Entry
entriesOf clientsPerUser cfgs = fst (mkEntries clientsPerUser (NE.fromList cfgs) (mkStdGen 7))

-- | @population clients epochsPerClient clientsPerUser entries@
population :: Int -> Int -> Int -> V.Vector Entry -> Population
population n e c es = either (error . T.unpack) id (mkPopulation n e c es (mkStdGen 3))

allUsers :: Population -> [SimUser]
allUsers p = [u | t <- p.teams, u <- toList t.members] <> p.personalUsers

streamsOf :: Population -> [[Stream]]
streamsOf p = map (readerStreams p.kinds) (readers p)

-- the only push a single-entry spec can produce, for any seed
pushOf :: V.Vector Entry -> FanInPush
pushOf es = fst (genPush (mkPayload 0) es (mkStdGen 1))

spec :: Spec
spec = do
  describe "mkPopulation" $ do
    it "spreads clients over a user pool; shared streams are allowed" $ do
      let p = population 25 3 1 (entriesOf 1 [TargetConfig KindUser 10 1])
      all (\ss -> length ss == 1 && all ((== KindUser) . streamKind) ss) (streamsOf p) `shouldBe` True
      length (readers p) `shouldBe` 25
      sum (Map.elems p.subscribers) `shouldBe` 25
      all (`elem` [2, 3]) (Map.elems p.subscribers) `shouldBe` True

    it "puts every client on the single team of a one-team pool" $ do
      let es = entriesOf 1 [TargetConfig KindTeam 1 1]
          p = population 25 3 1 es
      expectedDeliveries p.subscribers (pushOf es) `shouldBe` 25

    it "gives each user E distinct consecutive epoch streams" $ do
      let p = population 10 3 1 (entriesOf 1 [TargetConfig KindEpoch 10 1])
      all (\ss -> length (nubOrd (map show ss)) == 3) (streamsOf p) `shouldBe` True
      sum (Map.elems p.subscribers) `shouldBe` 30

    it "rejects more epochs per client than epoch streams" $
      isLeft (mkPopulation 10 11 1 (entriesOf 1 [TargetConfig KindEpoch 10 1]) (mkStdGen 3)) `shouldBe` True

    it "numbers the clients of a user 1..k, and rejects more than --clients-per-user" $ do
      let es = entriesOf 2 [TargetConfig KindClients 4 1]
      -- 4 clients: one per user (client id 1); a push to client ids [1,2] reaches 1 subscriber
      expectedDeliveries (population 4 3 2 es).subscribers (pushOf es) `shouldBe` 1
      -- 8 clients: both client ids subscribed
      expectedDeliveries (population 8 3 2 es).subscribers (pushOf es) `shouldBe` 2
      isLeft (mkPopulation 9 3 2 es (mkStdGen 3)) `shouldBe` True

    it "rejects the connections kind" $
      isLeft (mkPopulation 5 3 1 (entriesOf 1 [TargetConfig KindConnections 5 1]) (mkStdGen 3)) `shouldBe` True

    it "combines kinds per client: user, client, team, epochs" $ do
      let p = population 6 2 1 (entriesOf 1 [TargetConfig KindUser 6 1, TargetConfig KindClients 6 1, TargetConfig KindTeam 2 1, TargetConfig KindEpoch 4 1])
      all (\ss -> map streamKind ss == [KindUser, KindClients, KindTeam, KindEpoch, KindEpoch]) (streamsOf p) `shouldBe` True

    it "counts N subscribers per kind and E x N for epochs" $ do
      let p = population 30 2 3 (entriesOf 3 [TargetConfig KindUser 10 1, TargetConfig KindClients 10 1, TargetConfig KindTeam 4 1, TargetConfig KindEpoch 9 1])
      sum (Map.elems p.subscribers) `shouldBe` 30 * (1 + 1 + 1 + 2)

    it "puts every user in exactly one team; all its clients share team and epochs" $ do
      let p = population 40 2 4 (entriesOf 4 [TargetConfig KindUser 10 1, TargetConfig KindTeam 3 1, TargetConfig KindEpoch 7 1])
          uids = [u.uid | t <- p.teams, u <- toList t.members]
      (length uids, length (nubOrd uids), null p.personalUsers) `shouldBe` (10, 10, True)
      map (length . (.members)) p.teams `shouldBe` [4, 3, 3]
      all (\u -> length u.clients == 4 && length u.epochs == 2) (allUsers p) `shouldBe` True

    it "creates ceil(N / clients-per-user) unique synthetic users without user or clients targets" $ do
      let p = population 25 1 2 (entriesOf 2 [TargetConfig KindTeam 2 1, TargetConfig KindEpoch 50 1])
      length (allUsers p) `shouldBe` 13
      length (nubOrd (map (.uid) (allUsers p))) `shouldBe` 13
      length (readers p) `shouldBe` 25

    it "accepts user and clients pools of different sizes (consume)" $
      length (readers (population 10 1 1 (entriesOf 1 [TargetConfig KindUser 10 1, TargetConfig KindClients 5 1]))) `shouldBe` 10

    prop "identities are unique, 1..k per user, and match the user and client streams" $ \(Positive n0) (Positive s0) (Positive c0) ->
      let n = min 300 n0
          s = min 50 s0
          c = 1 + c0 `mod` 4
          es = entriesOf c [TargetConfig KindUser s 1, TargetConfig KindClients s 1, TargetConfig KindTeam 3 1, TargetConfig KindEpoch 7 1]
       in case mkPopulation n 2 c es (mkStdGen 3) of
            Left _ -> n > s * c -- only the capacity check may reject
            Right p ->
              let rs = readers p
                  ids = [(r.user.uid, r.cid) | r <- rs]
                  matches r = \case
                    StreamUser u -> u == r.user.uid
                    StreamClient u cid -> (u, cid) == (r.user.uid, r.cid)
                    _ -> True
               in length rs == n
                    && length (nubOrd ids) == n
                    && all (\r -> all (matches r) (readerStreams p.kinds r)) rs
                    && all (\u -> toList u.clients == map ClientId [1 .. fromIntegral (length u.clients)]) (allUsers p)

  describe "expectedDeliveries" $
    it "is 0 for a push to a stream nobody reads" $ do
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
          p = population 20 3 1 es
          -- stream pools are fixed, so scan pushes until one hits an unread stream
          counts = [expectedDeliveries p.subscribers (fst (genPush (mkPayload 0) es (mkStdGen s))) | s <- [1 .. 200]]
      (0 `elem` counts, all (`elem` [0, 1]) counts) `shouldBe` (True, True)

  describe "expectedFromCounts" $ do
    let es = entriesOf 1 [TargetConfig KindUser 100 1]
        p = population 20 3 1 es
        readUsers = [r.user.uid | r <- readers p]
        readUser = fromMaybe (error "no readers") (listToMaybe readUsers)
        unreadUser = fromMaybe (error "every pool user has a reader") (listToMaybe [u | u <- poolUsers es, u `notElem` readUsers])
        poolUsers entries = concat [V.toList v | Entry _ (UserPool v) <- V.toList entries]
    it "is rows times readers of the stream, summed over streams" $ do
      let readersOf = Map.findWithDefault 0 (streamKey (StreamUser readUser)) p.subscribers
      readersOf `shouldSatisfy` (>= 1)
      expectedFromCounts p.subscribers [(StreamUser readUser, 7)] `shouldBe` 7 * readersOf
      expectedFromCounts p.subscribers [(StreamUser readUser, 7), (StreamUser readUser, 1)] `shouldBe` 8 * readersOf
    it "ignores rows of streams nobody reads" $ do
      expectedFromCounts p.subscribers [(StreamUser unreadUser, 5)] `shouldBe` 0
      subscribedRows p.subscribers [(StreamUser unreadUser, 5), (StreamUser readUser, 2)] `shouldBe` 2
    it "equals the sum of expectedDeliveries of the pushes that wrote the rows" $ do
      let pushes = [fst (genPush (mkPayload 0) es (mkStdGen sd)) | sd <- [1 .. 50]]
          rows =
            Map.fromListWith
              (\(st, a) (_, b) -> (st, a + b))
              [(streamKey st, (st, 1 :: Int)) | push <- pushes, TargetUser u <- push.targets, let st = StreamUser u]
      expectedFromCounts p.subscribers (Map.elems rows) `shouldBe` sum (map (expectedDeliveries p.subscribers) pushes)

  describe "coverage" $ do
    it "is the share of pool streams with a reader" $
      let es = entriesOf 1 [TargetConfig KindUser 100 1]
       in coverage (population 20 3 1 es).subscribers es `shouldBe` [(KindUser, 0.2)]
    it "is 1 when every stream has a reader" $
      let es = entriesOf 1 [TargetConfig KindTeam 1 1, TargetConfig KindUser 10 1]
       in map snd (coverage (population 10 3 1 es).subscribers es) `shouldBe` [1, 1]
