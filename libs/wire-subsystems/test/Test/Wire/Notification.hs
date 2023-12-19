module Test.Wire.Notification (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Range (Range, fromRange)
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Writer (Writer, runWriter, tell)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Wire.API.Team.Member (HardTruncationLimit)
import Wire.Notification (GundeckAPIAccess (PushV2), PushTo (..), PushToUser, chunkPushes, pushImpl, removeIfLargeFanout, _pushRecipients)
import Wire.Sem.Concurrency.IO (performConcurrency)
import Data.Containers.ListUtils (nubOrdOn)

spec :: Spec
spec = describe "notification subsystem behaves correctly" do
  it "sends all notifications" allNotificationsSent
  it "respects maximum fanout limit" maximumFanoutlimitRespected
  describe "chunks notficiations correctly" notificationsCorrectlyChunked

runGundeckAPIAccessMock :: Member (Writer [V2.Push]) r => Sem (GundeckAPIAccess : r) a -> Sem r a
runGundeckAPIAccessMock = interpret \case
  PushV2 pushes -> tell pushes

allNotificationsSent :: Expectation
allNotificationsSent = do
  pushes :: [PushToUser] <- generate (arbitrary `suchThat` \l -> nubOrdOn pushJson l == l)

  mockPushes :: ([V2.Push], ()) <-
    runFinal
      . performConcurrency
      . runWriter
      . runGundeckAPIAccessMock
      $ pushImpl pushes
  print mockPushes
  length (nubOrdOn V2._pushPayload (fst mockPushes)) `shouldBe` length pushes

-- TODO: this doesn't really make sense; it is similar to the actual implementation and is basically testing filter
maximumFanoutlimitRespected :: Property
maximumFanoutlimitRespected = property \(range :: Range 1 HardTruncationLimit Int32) (pushes :: [PushTo Int]) ->
  all (\PushTo {_pushRecipients} -> length _pushRecipients <= fromIntegral (fromRange range)) (removeIfLargeFanout range pushes)

normalisePush :: PushTo a -> [PushTo a]
normalisePush p =
  map
    (\r -> p {_pushRecipients = r :| []})
    (toList (_pushRecipients p))

chunkSize :: [PushTo a] -> Natural
chunkSize = getSum . foldMap (Sum . fromIntegral . length . _pushRecipients)

notificationsCorrectlyChunked :: Spec
notificationsCorrectlyChunked = do
  it "allows empty push" $ property \limit ->
    chunkPushes limit [] === ([] :: [[PushTo ()]])
  it "produces no empty chunks" $ property \limit (pushes :: [PushTo Int]) ->
    not (any null (chunkPushes limit pushes))
  it "allows concatenation if number was non-zero" $ property \(Positive limit) (pushes :: [PushTo Int]) ->
    (chunkPushes limit pushes >>= reverse >>= normalisePush)
      === (pushes >>= normalisePush)
  it "respects the chunkSize limit" $ property \limit (pushes :: [PushTo Int]) ->
    all ((<= limit) . chunkSize) (chunkPushes limit pushes)
