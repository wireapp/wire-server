module FanInPerf.ConsumeSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Aeson qualified as A
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Consume
import FanInPerf.ConsumeStats
import FanInPerf.Options (ReaderMode (..))
import FanInPerf.Population (SimUser (SimUser))
import FanInPerf.Population qualified as Pop
import FanInPerf.Targets (TargetKind (..))
import Imports
import Polysemy
import Polysemy.Error
import Test.Hspec
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres (genNotificationId, notificationIdMillis)

type Rows = IORef [(Stream, [(UUID, A.Value)])]

data Call = InitCall UserId ClientId Stream | AckCall UserId ClientId Stream UUID
  deriving (Eq, Show)

-- | In-memory store; every fetch fails while the flag is set; ack calls are logged (newest first).
fakeStore :: (Member (Embed IO) r, Member (Error Text) r) => IORef [Call] -> Rows -> IORef Bool -> InterpreterFor FanInNotificationsStore r
fakeStore calls rows failing = interpret $ \case
  PushViaFanIn _ -> pure ()
  FetchNotifications s cursor limit -> do
    down <- readIORef failing
    when down (throw (T.pack "store down"))
    rs <- fromMaybe [] . lookup s <$> readIORef rows
    pure (take limit (filter ((> cursor) . fst) rs))
  GetLastNotificationId s -> embed (lastId s)
  InitAcks u c s -> modifyIORef calls (InitCall u c s :) >> embed (lastId s)
  AckNotification u c s i -> do
    down <- readIORef failing
    when down (throw (T.pack "store down"))
    modifyIORef calls (AckCall u c s i :)
  where
    lastId :: Stream -> IO (Maybe UUID)
    lastId s = fmap fst . listToMaybe . reverse . fromMaybe [] . lookup s <$> readIORef rows

runFakeWith :: IORef [Call] -> Rows -> IORef Bool -> Sem '[FanInNotificationsStore, Error Text, Embed IO] a -> IO a
runFakeWith calls rows failing = runM . runError . fakeStore calls rows failing >=> either (fail . T.unpack) pure

runFake :: Rows -> IORef Bool -> Sem '[FanInNotificationsStore, Error Text, Embed IO] a -> IO a
runFake rows failing act = newIORef [] >>= \calls -> runFakeWith calls rows failing act

-- | Ascending ids (sorted: UUIDv7 need not be monotonic within one millisecond).
newIds :: Int -> IO [(UUID, A.Value)]
newIds n = sort <$> replicateM n ((\i -> (i.toUUID, A.Null)) <$> genNotificationId @())

teamId :: TeamId
teamId = Id (UUID.fromWords 1 2 3 4)

team :: Stream
team = StreamTeam teamId

uid0 :: UserId
uid0 = Id (UUID.fromWords 9 9 9 9)

reader0 :: Pop.Reader
reader0 = Pop.Reader (Just teamId) (SimUser uid0 [] (ClientId 7 :| [])) (ClientId 7)

user :: Stream
user = StreamUser (Id (UUID.fromWords 5 6 7 8))

setup :: [(UUID, A.Value)] -> IO (Rows, IORef Bool)
setup rs = (,) <$> newIORef [(team, rs)] <*> newIORef False

sub0 :: Subscription
sub0 = newSubscription KindTeam team UUID.nil

pass :: Rows -> IORef Bool -> Int -> ReaderStats -> [Subscription] -> IO ([Subscription], Bool, [Text])
pass rows failing limit = passW rows failing limit 0

-- | 'pass' with a safety window in ms.
passW :: Rows -> IORef Bool -> Int -> Word64 -> ReaderStats -> [Subscription] -> IO ([Subscription], Bool, [Text])
passW rows failing limit window stats subs = do
  r <- runFake rows failing (readerPass @Text id limit window stats subs)
  pure (r.subs, r.more, r.errors)

-- | Synthetic UUIDv7: ms timestamp, version 7, variant bits and a sequence
-- number in the low bits (deterministic, unlike the clock).
v7 :: Word64 -> Word32 -> UUID
v7 ms n = UUID.fromWords (fromIntegral (ms `shiftR` 16)) (fromIntegral ((ms .&. 0xFFFF) `shiftL` 16) .|. 0x7000) 0x80000000 n

-- | Passes until no stream returns a full page (at most @n@); returns the subs
-- and the number of passes.
untilIdle :: Int -> IO ([Subscription], Bool, [Text]) -> ([Subscription] -> IO ([Subscription], Bool, [Text])) -> IO ([Subscription], Int)
untilIdle n first next = first >>= go 1
  where
    go i (subs, more, _)
      | more && i < n = next subs >>= go (i + 1)
      | otherwise = pure (subs, i)

deliveredCount :: ReaderStats -> IO Int
deliveredCount stats = totalDelivered <$> readReaderSnapshot stats

spec :: Spec
spec = do
  describe "startPositions" $ do
    it "NoAcks: starts at the latest notification, without touching acks" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      calls <- newIORef []
      subs <- runFakeWith calls rows failing (startPositions NoAcks (Set.singleton KindTeam) reader0)
      map (.cursor) subs `shouldBe` [fst (last rs)]
      readIORef calls `shouldReturn` []
    it "Acks: resets the reader's acks to the latest notification and starts there" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      calls <- newIORef []
      subs <- runFakeWith calls rows failing (startPositions (Acks 10) (Set.singleton KindTeam) reader0)
      map (.cursor) subs `shouldBe` [fst (last rs)]
      readIORef calls `shouldReturn` [InitCall uid0 (ClientId 7) team]
    it "starts at the nil UUID for a stream nothing was written to" $ do
      (rows, failing) <- setup []
      subs <- runFake rows failing (startPositions (Acks 10) (Set.singleton KindTeam) reader0)
      map (.cursor) subs `shouldBe` [UUID.nil]

  describe "millisLowerBound" $ do
    it "has the given ms timestamp and lies below every UUIDv7 of that ms" $ do
      [(i, _)] <- newIds 1
      let ms = notificationIdMillis i
      notificationIdMillis (millisLowerBound ms) `shouldBe` ms
      millisLowerBound ms < i `shouldBe` True
      millisLowerBound (ms + 1) > i `shouldBe` True
      millisLowerBound 0 `shouldBe` UUID.nil

  describe "lagNs" $ do
    it "is the time since the notification was created" $ do
      [(i, _)] <- newIds 1
      lagNs (notificationIdMillis i + 5) i `shouldBe` 5_000_000
    it "is 0 for a notification created after 'now' (clock skew), not a huge Word64" $ do
      [(i, _)] <- newIds 1
      lagNs (notificationIdMillis i - 1) i `shouldBe` 0
      lagNs 0 i `shouldBe` 0

  describe "readerPass" $ do
    it "reports the delivered ids with their stream in page order" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      stats <- newReaderStats
      r <- runFake rows failing (readerPass @Text id 10 0 stats [sub0])
      r.delivered `shouldBe` [(team, i) | (i, _) <- rs]
    it "delivers a page, advances the cursor and records lag" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      stats <- newReaderStats
      (subs, more, errs) <- pass rows failing 10 stats [sub0]
      map (.cursor) subs `shouldBe` [fst (last rs)]
      (more, errs) `shouldBe` (False, [])
      s <- readReaderSnapshot stats
      (totalDelivered s, totalFetches s, VU.sum (lagBuckets s)) `shouldBe` (3, 1, 3)

    it "records the lag of a notification from the future in the lowest bucket" $ do
      -- UUIDv7 with the maximal 48-bit ms timestamp
      let future = UUID.fromWords 0xFFFFFFFF 0xFFFF7000 0x80000000 0
      (rows, failing) <- setup [(future, A.Null)]
      stats <- newReaderStats
      _ <- pass rows failing 10 stats [sub0]
      s <- readReaderSnapshot stats
      VU.head (lagBuckets s) `shouldBe` 1

    it "signals a full page, and pages through a backlog without duplicates or gaps" $ do
      rs <- newIds 5
      (rows, failing) <- setup rs
      stats <- newReaderStats
      (s1, more1, _) <- pass rows failing 2 stats [sub0]
      (s2, more2, _) <- pass rows failing 2 stats s1
      (s3, more3, _) <- pass rows failing 2 stats s2
      (more1, more2, more3) `shouldBe` (True, True, False)
      map (.cursor) s3 `shouldBe` [fst (last rs)]
      s <- readReaderSnapshot stats
      totalDelivered s `shouldBe` 5

    it "delivers only newer notifications on the next pass" $ do
      rs <- newIds 2
      (rows, failing) <- setup rs
      stats <- newReaderStats
      (s1, _, _) <- pass rows failing 10 stats [sub0]
      more <- newIds 1
      modifyIORef rows (map (\(k, v) -> (k, v <> more)))
      _ <- pass rows failing 10 stats s1
      s <- readReaderSnapshot stats
      (totalDelivered s, totalEmptyFetches s) `shouldBe` (3, 0)

    it "counts a failed fetch, keeps the cursor and does not claim a full page" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      writeIORef failing True
      stats <- newReaderStats
      (subs, more, errs) <- pass rows failing 10 stats [sub0]
      map (.cursor) subs `shouldBe` [UUID.nil]
      more `shouldBe` False
      length errs `shouldBe` 1
      s <- readReaderSnapshot stats
      (totalFetchErrors s, totalDelivered s) `shouldBe` (1, 0)

    it "never delivers a notification older than the cursor (RFC cursor-model hazard)" $ do
      -- A takes the older id, B the newer one; B commits and is read first, A commits late.
      [(a, _), (b, _)] <- newIds 2
      (rows, failing) <- setup [(b, A.Null)]
      stats <- newReaderStats
      (s1, _, _) <- pass rows failing 10 stats [sub0]
      writeIORef rows [(team, [(a, A.Null), (b, A.Null)])]
      _ <- pass rows failing 10 stats s1
      s <- readReaderSnapshot stats
      totalDelivered s `shouldBe` 1 -- 'a' is lost; the run reports it as `missing`
  describe "readerPass with a safety window" $ do
    it "delivers a late-committed older notification within the window exactly once" $ do
      let a = v7 1000 1
          b = v7 1005 1
      (rows, failing) <- setup [(b, A.Null)]
      stats <- newReaderStats
      (s1, _, _) <- passW rows failing 10 10 stats [sub0]
      writeIORef rows [(team, [(a, A.Null), (b, A.Null)])]
      (s2, _, _) <- passW rows failing 10 10 stats s1
      deliveredCount stats `shouldReturn` 2
      map (.cursor) s2 `shouldBe` [b]
      -- further passes deliver nothing twice
      (s3, _, _) <- passW rows failing 10 10 stats s2
      _ <- passW rows failing 10 10 stats s3
      deliveredCount stats `shouldReturn` 2

    it "still loses a notification committed later than the window" $ do
      let a = v7 1000 1
          b = v7 1050 1
      (rows, failing) <- setup [(b, A.Null)]
      stats <- newReaderStats
      (s1, _, _) <- passW rows failing 10 10 stats [sub0]
      writeIORef rows [(team, [(a, A.Null), (b, A.Null)])]
      _ <- passW rows failing 10 10 stats s1
      deliveredCount stats `shouldReturn` 1

    it "never re-fetches below the start position" $ do
      let old = v7 1000 1
          new = v7 1001 1
      (rows, failing) <- setup [(old, A.Null)]
      stats <- newReaderStats
      subs <- runFake rows failing (startPositions NoAcks (Set.singleton KindTeam) reader0)
      modifyIORef rows (map (\(k, v) -> (k, v <> [(new, A.Null)])))
      _ <- passW rows failing 10 1000 stats subs
      deliveredCount stats `shouldReturn` 1

    it "pages past more already-seen rows than the fetch limit (no stall, no spin)" $ do
      let backlog = [(v7 (1000 + i) 1, A.Null) | i <- [0 .. 4]]
      (rows, failing) <- setup backlog
      stats <- newReaderStats
      (s1, n1) <- untilIdle 10 (passW rows failing 2 100 stats [sub0]) (passW rows failing 2 100 stats)
      deliveredCount stats `shouldReturn` 5
      n1 `shouldBe` 3
      -- the window now holds 5 seen rows (> limit 2); new and late rows follow
      let late = (v7 1002 99, A.Null)
          newer = [(v7 (1005 + i) 1, A.Null) | i <- [0 .. 2]]
      writeIORef rows [(team, sort (late : backlog <> newer))]
      (s2, n2) <- untilIdle 10 (passW rows failing 2 100 stats s1) (passW rows failing 2 100 stats)
      deliveredCount stats `shouldReturn` 9
      n2 `shouldSatisfy` (< 10)
      map (.cursor) s2 `shouldBe` [v7 1007 1]
      -- idle passes over a window of only seen rows deliver nothing and end
      (_, n3) <- untilIdle 10 (passW rows failing 2 100 stats s2) (passW rows failing 2 100 stats)
      deliveredCount stats `shouldReturn` 9
      n3 `shouldSatisfy` (< 10)
  describe "readerRound" $ do
    it "keeps cursors advanced before another stream's fetch threw" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      stats <- newReaderStats
      let userSub = newSubscription KindUser user UUID.nil
          fetchOne sub
            | sub.stream == team = Right <$> runFake rows failing (readerPass @Text id 10 0 stats [sub])
            | otherwise = throwIO (ErrorCall "boom")
      r <- readerRound @Text id stats fetchOne [sub0, userSub]
      let (subs, more, errs) = (r.subs, r.more, r.errors)
      map (.cursor) subs `shouldBe` [fst (last rs), UUID.nil]
      (more, length errs) `shouldBe` (False, 1)
      s <- readReaderSnapshot stats
      (totalDelivered s, totalFetchErrors s) `shouldBe` (3, 1)
      -- the next pass delivers nothing twice
      _ <- readerRound @Text id stats fetchOne r.subs
      s' <- readReaderSnapshot stats
      totalDelivered s' `shouldBe` 3

    it "counts an error result as fetch error and keeps the cursor" $ do
      stats <- newReaderStats
      r <- readerRound @Text id stats (\_ -> pure (Left "down")) [sub0]
      let (subs, more, errs) = (r.subs, r.more, r.errors)
      (map (.cursor) subs, more, errs) `shouldBe` ([UUID.nil], False, ["reader: team: down"])
      s <- readReaderSnapshot stats
      totalFetchErrors s `shouldBe` 1

  describe "isolated" $ do
    it "passes a result through" $
      isolated @Text id (\_ -> pure 0) (pure (Right (1 :: Int))) `shouldReturn` 1
    it "turns an error result and a synchronous exception into the failure branch" $ do
      isolated @Text id (pure . T.length) (pure (Left "down")) `shouldReturn` 4
      -- displayException of a SomeException may append a backtrace: match the message only
      msg <- isolated @Text id pure (throwIO (ErrorCall "boom"))
      msg `shouldSatisfy` T.isInfixOf "boom"
