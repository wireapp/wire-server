module Test.Wire.Notification (spec) where

import Data.Containers.ListUtils (nubOrdOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Range (Range, fromRange)
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Async (asyncToIOFinal)
import Polysemy.Writer (tell, writerToIOFinal)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assertWith, monadicIO, pre)
import Test.QuickCheck.Monadic qualified as QC
import Wire.API.Team.Member (HardTruncationLimit)
import Wire.Notification (GundeckAPIAccess (PushV2), PushTo (..), chunkPushes, pushImpl, removeIfLargeFanout, _pushRecipients)

spec :: Spec
spec = describe "notification subsystem behaves correctly" do
  it "sends all notifications" allNotificationsSent
  it "respects maximum fanout limit" maximumFanoutlimitRespected
  describe "chunks notficiations correctly" notificationsCorrectlyChunked

runGundeckAPIAccessMock :: Member (Final IO) r => Sem (GundeckAPIAccess : r) a -> Sem r [V2.Push]
runGundeckAPIAccessMock =
  fmap fst . writerToIOFinal . reinterpret \case
    PushV2 pushes -> tell pushes

allNotificationsSent :: Property
allNotificationsSent = property \(NonEmpty pushes) -> monadicIO do
  pre (nubOrdOn pushJson pushes == pushes)
  pre (nubOrdOn id pushes == pushes)
  -- FIXME: the 16 should be reflecting the actual fanout limit
  pre (all (\p -> length p._pushRecipients <= 16) pushes)

  mockPushes <-
    nubOrdOn V2._pushPayload <$> QC.run do
      runFinal
        . asyncToIOFinal
        . runGundeckAPIAccessMock
        $ pushImpl pushes

  let c =
        unlines
          [ "Expected:",
            show pushes,
            "\nwith length:",
            show (length pushes),
            "\nBut got:",
            show mockPushes,
            "\nwith length:",
            show (length mockPushes)
          ]
  assertWith (length pushes == length mockPushes) c

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
