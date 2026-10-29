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

-- | @populationE onlineUsers epochsPerUser clientsPerUser entries@
populationE :: Maybe Int -> Int -> Int -> V.Vector Entry -> Either Text Population
populationE u e c es = mkPopulation u e c es (mkStdGen 3)

population :: Maybe Int -> Int -> Int -> V.Vector Entry -> Population
population u e c es = either (error . T.unpack) id (populationE u e c es)

-- | The writers' pool of 100 users, of which 20 are online: 80 % of the written
-- streams have no reader.
writerEntries :: V.Vector Entry
writerEntries = entriesOf 1 [TargetConfig KindUser 100 1]

partialReaders :: Population
partialReaders = population (Just 20) 3 1 writerEntries

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
    it "makes every user of a user pool a reader with C clients" $ do
      let p = population Nothing 3 2 (entriesOf 2 [TargetConfig KindUser 10 1])
      all (\ss -> length ss == 1 && all ((== KindUser) . streamKind) ss) (streamsOf p) `shouldBe` True
      readerCount p `shouldBe` 20
      -- both clients of a user read the user's stream
      Map.elems p.subscribers `shouldBe` replicate 10 2

    it "runs experiments A-F with user:2000x1: 2,000 users, one client (id 1) each, no teams" $ do
      let p = population Nothing 3 1 (entriesOf 1 [TargetConfig KindUser 2000 1])
      readerCount p `shouldBe` 2000
      null p.teams `shouldBe` True
      all (\u -> toList u.clients == [ClientId 1]) (allUsers p) `shouldBe` True
      all (== 1) (Map.elems p.subscribers) `shouldBe` True

    it "puts every client on the single team of a one-team pool" $ do
      let es = entriesOf 1 [TargetConfig KindTeam 1 1]
          p = population (Just 25) 3 1 es
      expectedDeliveries p.subscribers (pushOf es) `shouldBe` 25

    it "gives each user E distinct consecutive epoch streams" $ do
      let p = population (Just 10) 3 1 (entriesOf 1 [TargetConfig KindEpoch 10 1])
      all (\ss -> length (nubOrd (map show ss)) == 3) (streamsOf p) `shouldBe` True
      sum (Map.elems p.subscribers) `shouldBe` 30

    it "rejects more epochs per user than epoch streams" $
      isLeft (populationE (Just 10) 11 1 (entriesOf 1 [TargetConfig KindEpoch 10 1])) `shouldBe` True

    it "numbers the clients of a user 1..C: every client id of a clients push has a reader" $ do
      let es = entriesOf 2 [TargetConfig KindClients 4 1]
      expectedDeliveries (population Nothing 3 2 es).subscribers (pushOf es) `shouldBe` 2

    it "rejects the connections kind" $
      isLeft (populationE (Just 5) 3 1 (entriesOf 1 [TargetConfig KindConnections 5 1])) `shouldBe` True

    it "combines kinds per client: user, client, team, epochs" $ do
      let p = population Nothing 2 1 (entriesOf 1 [TargetConfig KindUser 6 1, TargetConfig KindClients 6 1, TargetConfig KindTeam 2 1, TargetConfig KindEpoch 4 1])
      all (\ss -> map streamKind ss == [KindUser, KindClients, KindTeam, KindEpoch, KindEpoch]) (streamsOf p) `shouldBe` True

    it "counts one subscriber per reader and kind, E per reader for epochs" $ do
      let p = population Nothing 2 3 (entriesOf 3 [TargetConfig KindUser 10 1, TargetConfig KindClients 10 1, TargetConfig KindTeam 4 1, TargetConfig KindEpoch 9 1])
      readerCount p `shouldBe` 30
      sum (Map.elems p.subscribers) `shouldBe` 30 * (1 + 1 + 1 + 2)

    it "puts every user in exactly one team; all its clients share team and epochs" $ do
      let p = population Nothing 2 4 (entriesOf 4 [TargetConfig KindUser 10 1, TargetConfig KindTeam 3 1, TargetConfig KindEpoch 7 1])
          uids = [u.uid | t <- p.teams, u <- toList t.members]
      (length uids, length (nubOrd uids), null p.personalUsers) `shouldBe` (10, 10, True)
      map (length . (.members)) p.teams `shouldBe` [4, 3, 3]
      all (\u -> length u.clients == 4 && length u.epochs == 2) (allUsers p) `shouldBe` True

    it "creates --online-users unique synthetic users with C clients each without user or clients targets" $ do
      let p = population (Just 13) 1 2 (entriesOf 2 [TargetConfig KindTeam 2 1, TargetConfig KindEpoch 50 1])
      length (allUsers p) `shouldBe` 13
      length (nubOrd (map (.uid) (allUsers p))) `shouldBe` 13
      readerCount p `shouldBe` 26

    it "needs --online-users without a user or clients pool" $ do
      let es = entriesOf 1 [TargetConfig KindTeam 2 1]
      isLeft (populationE Nothing 3 1 es) `shouldBe` True
      isRight (populationE (Just 5) 3 1 es) `shouldBe` True

    it "makes the first --online-users users of a pool online (default: the whole pool), with C clients each" $ do
      let all' = population Nothing 3 2 writerEntries
          some = population (Just 20) 3 2 writerEntries
          users p = map (.uid) (allUsers p)
      readerCount all' `shouldBe` 200
      readerCount some `shouldBe` 40
      users some `shouldBe` take 20 (users all')
      all (== 2) (Map.elems some.subscribers) `shouldBe` True

    it "rejects more online users than the pool has" $ do
      isRight (populationE (Just 100) 3 1 writerEntries) `shouldBe` True
      isLeft (populationE (Just 101) 3 1 writerEntries) `shouldBe` True

    it "rejects more than maxReaders readers" $ do
      let es = entriesOf 1 [TargetConfig KindTeam 2 1]
      isRight (populationE (Just (maxReaders `div` 2)) 3 2 es) `shouldBe` True
      isLeft (populationE (Just (maxReaders `div` 2 + 1)) 3 2 es) `shouldBe` True
      isLeft (populationE (Just maxBound) 3 2 es) `shouldBe` True -- U x C overflows Int
      -- with a pool: the online users count, not the pool size
      let c = maxReaders `div` 100
          pool100 c' = entriesOf c' [TargetConfig KindUser 100 1, TargetConfig KindClients 100 1]
      isRight (populationE (Just 100) 3 c (pool100 c)) `shouldBe` True
      isLeft (populationE (Just 100) 3 (c + 1) (pool100 (c + 1))) `shouldBe` True

    it "defaults to as many pool users as maxReaders allows" $ do
      let c = maxReaders `div` 100 + 1
      readerCount (population Nothing 3 c (entriesOf c [TargetConfig KindUser 100 1])) `shouldBe` (maxReaders `div` c) * c

    prop "identities are unique, 1..C per user, and match the user and client streams" $ \(Positive s0) (Positive c0) mu0 ->
      let s = 1 + s0 `mod` 50
          c = 1 + c0 `mod` 4
          onlineUsers = (\(Positive u0) -> 1 + u0 `mod` s) <$> mu0
          nOnline = fromMaybe s onlineUsers
          es = entriesOf c [TargetConfig KindUser s 1, TargetConfig KindClients s 1, TargetConfig KindTeam 3 1, TargetConfig KindEpoch 7 1]
          firstPoolUsers = take nOnline (concat [V.toList v | Entry _ (UserPool v) <- V.toList es])
       in case populationE onlineUsers 2 c es of
            Left _ -> False
            Right p ->
              let rs = readers p
                  ids = [(r.user.uid, r.cid) | r <- rs]
                  matches r = \case
                    StreamUser u -> u == r.user.uid
                    StreamClient u cid -> (u, cid) == (r.user.uid, r.cid)
                    _ -> True
               in readerCount p == nOnline * c
                    && length (nubOrd ids) == nOnline * c
                    && sort (map (.uid) (allUsers p)) == sort firstPoolUsers
                    && all (\r -> all (matches r) (readerStreams p.kinds r)) rs
                    && all (\u -> toList u.clients == map ClientId [1 .. fromIntegral c]) (allUsers p)

    prop "makes U synthetic users with C clients each without a user or clients pool" $ \(Positive u0) (Positive c0) ->
      let u = 1 + u0 `mod` 50
          c = 1 + c0 `mod` 4
       in case populationE (Just u) 2 c (entriesOf c [TargetConfig KindTeam 3 1, TargetConfig KindEpoch 7 1]) of
            Left _ -> False
            Right p ->
              readerCount p == u * c
                && length (nubOrd (map (.uid) (allUsers p))) == u
                && length (nubOrd [(r.user.uid, r.cid) | r <- readers p]) == u * c

  describe "expectedDeliveries" $
    it "is 0 for a push to a stream nobody reads" $ do
      -- stream pools are fixed, so scan pushes until one hits an unread stream
      let counts = [expectedDeliveries partialReaders.subscribers (fst (genPush (mkPayload 0) writerEntries (mkStdGen s))) | s <- [1 .. 200]]
      (0 `elem` counts, all (`elem` [0, 1]) counts) `shouldBe` (True, True)

  describe "expectedFromCounts" $ do
    let es = writerEntries
        p = partialReaders
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
      coverage partialReaders.subscribers writerEntries `shouldBe` [(KindUser, 0.2)]
    it "is 1 when every stream has a reader" $
      let es = entriesOf 1 [TargetConfig KindTeam 1 1, TargetConfig KindUser 10 1]
       in map snd (coverage (population Nothing 3 1 es).subscribers es) `shouldBe` [1, 1]
