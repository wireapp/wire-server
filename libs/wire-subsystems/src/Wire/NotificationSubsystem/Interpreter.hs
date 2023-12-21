module Wire.NotificationSubsystem.Interpreter where

import Control.Lens (set, (.~))
import Data.Aeson
import Data.Id
import Data.List.NonEmpty (nonEmpty)
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Range (Range, fromRange, unsafeRange)
import Data.Set qualified as Set
import Gundeck.Types hiding (Push (..), Recipient, newPush)
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Async (Async, sequenceConcurrently)
import Polysemy.Input
import Wire.API.Team.Member
import Wire.GundeckAPIAccess
import Wire.NotificationSubsystem

-- | We interpret this using 'GundeckAPIAccess' so we can mock it out for testing.
runNotificationSubsystemGundeck ::
  ( Member (GundeckAPIAccess) r,
    Member Async r,
    Member (Input NotificationSubsystemConfig) r
  ) =>
  Sem (NotificationSubsystem : r) a ->
  Sem r a
runNotificationSubsystemGundeck = interpret $ \case
  Push ps -> pushImpl ps

data NotificationSubsystemConfig = NotificationSubsystemConfig
  { fanoutLimit :: Range 1 HardTruncationLimit Int32,
    chunkSize :: Natural
  }

-- TODO: write a test for listtype
pushImpl ::
  forall r.
  ( Member (GundeckAPIAccess) r,
    Member (Input NotificationSubsystemConfig) r,
    Member (Async) r
  ) =>
  [PushToUser] ->
  Sem r ()
pushImpl ps = do
  currentFanoutLimit <- inputs fanoutLimit
  pushChunkSize <- inputs chunkSize

  let pushes :: [[V2.Push]] =
        mkPushes pushChunkSize $
          removeIfLargeFanout currentFanoutLimit ps
  void $
    sequenceConcurrently $
      pushV2 <$> pushes

removeIfLargeFanout :: Range n m Int32 -> [PushTo user] -> [PushTo user]
removeIfLargeFanout limit =
  filter \PushTo {_pushRecipients} ->
    length _pushRecipients <= fromIntegral (fromRange limit)

mkPushes :: Natural -> [PushToUser] -> [[V2.Push]]
mkPushes chunkSize = map (map toV2Push) . chunkPushes chunkSize

{-# INLINE [1] toV2Push #-}
toV2Push :: PushToUser -> V2.Push
toV2Push p =
  (V2.newPush p.pushOrigin (unsafeRange (Set.fromList recipients)) pload)
    & V2.pushOriginConnection .~ _pushConn p
    & V2.pushTransient .~ _pushTransient p
    & maybe id (set V2.pushNativePriority) p._pushNativePriority
  where
    pload :: List1 Object
    pload = List1.singleton (pushJson p)
    recipients :: [V2.Recipient]
    recipients = map toRecipient $ toList p._pushRecipients
    toRecipient :: RecipientBy UserId -> V2.Recipient
    toRecipient r =
      (recipient r._recipientUserId p._pushRoute)
        { V2._recipientClients = r._recipientClients
        }

{-# INLINE [1] chunkPushes #-}
chunkPushes :: Natural -> [PushTo a] -> [[PushTo a]]
chunkPushes maxRecipients
  | maxRecipients > 0 = go 0 []
  | otherwise = const []
  where
    go _ [] [] = []
    go _ acc [] = [acc]
    go n acc (y : ys)
      | n >= maxRecipients = acc : go 0 [] (y : ys)
      | otherwise =
          let totalLength = (n + fromIntegral (length y._pushRecipients))
           in if totalLength > maxRecipients
                then
                  let (y1, y2) = splitPush (maxRecipients - n) y
                   in go maxRecipients (y1 : acc) (y2 : ys)
                else go totalLength (y : acc) ys

    -- n must be strictly > 0 and < length (_pushRecipients p)
    splitPush :: Natural -> PushTo a -> (PushTo a, PushTo a)
    splitPush n p =
      let (r1, r2) = splitAt (fromIntegral n) (toList p._pushRecipients)
       in (p {_pushRecipients = fromJust $ nonEmpty r1}, p {_pushRecipients = fromJust $ nonEmpty r2})
