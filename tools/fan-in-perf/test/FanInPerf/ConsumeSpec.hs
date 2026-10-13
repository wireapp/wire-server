module FanInPerf.ConsumeSpec (spec) where

import Data.Aeson qualified as A
import Data.Id
import Data.Text qualified as T
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Data.Vector.Unboxed qualified as VU
import FanInPerf.Consume
import FanInPerf.ConsumeStats
import FanInPerf.Population (Client (..))
import FanInPerf.Targets (TargetKind (..))
import Imports
import Polysemy
import Polysemy.Error
import Test.Hspec
import Wire.FanInNotificationsStore
import Wire.FanInNotificationsStore.Postgres (genNotificationId, notificationIdMillis)

type Rows = IORef [(Stream, [(UUID, A.Value)])]

-- | In-memory store; every fetch fails while the flag is set.
fakeStore :: (Member (Embed IO) r, Member (Error Text) r) => Rows -> IORef Bool -> InterpreterFor FanInNotificationsStore r
fakeStore rows failing = interpret $ \case
  PushViaFanIn _ -> pure ()
  FetchNotifications s cursor limit -> do
    down <- readIORef failing
    when down (throw (T.pack "store down"))
    rs <- fromMaybe [] . lookup s <$> readIORef rows
    pure (take limit (filter ((> cursor) . fst) rs))
  GetLastNotificationId s -> do
    rs <- fromMaybe [] . lookup s <$> readIORef rows
    pure (fst <$> listToMaybe (reverse rs))

runFake :: Rows -> IORef Bool -> Sem '[FanInNotificationsStore, Error Text, Embed IO] a -> IO a
runFake rows failing = runM . runError . fakeStore rows failing >=> either (fail . T.unpack) pure

-- | Ascending ids (sorted: UUIDv7 need not be monotonic within one millisecond).
newIds :: Int -> IO [(UUID, A.Value)]
newIds n = sort <$> replicateM n ((\i -> (i.toUUID, A.Null)) <$> genNotificationId @())

team :: Stream
team = StreamTeam (Id (UUID.fromWords 1 2 3 4))

setup :: [(UUID, A.Value)] -> IO (Rows, IORef Bool)
setup rs = (,) <$> newIORef [(team, rs)] <*> newIORef False

sub0 :: Sub
sub0 = Sub {kind = KindTeam, stream = team, cursor = UUID.nil}

pass :: Rows -> IORef Bool -> Int -> ReaderStats -> [Sub] -> IO ([Sub], Bool, [Text])
pass rows failing limit stats subs = runFake rows failing (readerPass @Text limit stats subs)

spec :: Spec
spec = do
  describe "loadCursors" $ do
    it "starts at the latest notification of a stream" $ do
      rs <- newIds 3
      (rows, failing) <- setup rs
      subs <- runFake rows failing (loadCursors (Client [team]))
      map (.cursor) subs `shouldBe` [fst (last rs)]
    it "starts at the nil UUID for a stream nothing was written to" $ do
      (rows, failing) <- setup []
      subs <- runFake rows failing (loadCursors (Client [team]))
      map (.cursor) subs `shouldBe` [UUID.nil]

  describe "lagNs" $ do
    it "is the time since the notification was created" $ do
      [(i, _)] <- newIds 1
      lagNs (notificationIdMillis i + 5) i `shouldBe` 5_000_000
    it "is 0 for a notification created after 'now' (clock skew), not a huge Word64" $ do
      [(i, _)] <- newIds 1
      lagNs (notificationIdMillis i - 1) i `shouldBe` 0
      lagNs 0 i `shouldBe` 0

  describe "readerPass" $ do
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
