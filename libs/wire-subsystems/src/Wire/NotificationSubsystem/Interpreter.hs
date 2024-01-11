module Wire.NotificationSubsystem.Interpreter where

import Control.Lens (set, (.~))
import Data.Aeson
import Data.List.NonEmpty (nonEmpty)
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Proxy
import Data.Range
import Data.Set qualified as Set
import Gundeck.Types hiding (Push (..), Recipient, newPush)
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Async (Async, sequenceConcurrently)
import Polysemy.Input
import Wire.API.Team.Member
import Wire.GundeckAPIAccess (GundeckAPIAccess)
import Wire.GundeckAPIAccess qualified as GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Internal
import Wire.Sem.Delay

-- | We interpret this using 'GundeckAPIAccess' so we can mock it out for testing.
runNotificationSubsystemGundeck ::
  ( Member (GundeckAPIAccess) r,
    Member Async r,
    Member Delay r
  ) =>
  NotificationSubsystemConfig ->
  Sem (NotificationSubsystem : r) a ->
  Sem r a
runNotificationSubsystemGundeck cfg = interpret $ \case
  PushNotifications ps -> runInputConst cfg $ pushImpl ps
  PushNotificationsSlowly ps -> runInputConst cfg $ pushSlowlyImpl ps
  UserDeleted uid -> GundeckAPIAccess.userDeleted uid
  UnregisterPushClient uid cid -> GundeckAPIAccess.unregisterPushClient uid cid
  GetPushTokens uid -> GundeckAPIAccess.getPushTokens uid

data NotificationSubsystemConfig = NotificationSubsystemConfig
  { fanoutLimit :: Range 1 HardTruncationLimit Int32,
    chunkSize :: Natural,
    -- | Microseconds
    slowPushDelay :: Int
  }

defaultNotificationSubsystemConfig :: NotificationSubsystemConfig
defaultNotificationSubsystemConfig =
  NotificationSubsystemConfig defaultFanoutLimit defaultChunkSize defaultSlowPushDelay

defaultFanoutLimit :: Range 1 HardTruncationLimit Int32
defaultFanoutLimit = toRange (Proxy @HardTruncationLimit)

defaultChunkSize :: Natural
defaultChunkSize = 128

defaultSlowPushDelay :: Int
defaultSlowPushDelay = 20_000

pushImpl ::
  forall r.
  ( Member (GundeckAPIAccess) r,
    Member (Input NotificationSubsystemConfig) r,
    Member (Async) r
  ) =>
  [Push] ->
  Sem r ()
pushImpl ps = do
  currentFanoutLimit <- inputs fanoutLimit
  pushChunkSize <- inputs chunkSize

  let pushes :: [[V2.Push]] =
        mkPushes pushChunkSize $
          removeIfLargeFanout currentFanoutLimit ps
  void $
    sequenceConcurrently $
      GundeckAPIAccess.pushV2 <$> pushes

removeIfLargeFanout :: Range n m Int32 -> [Push] -> [Push]
removeIfLargeFanout limit =
  filter \Push {_pushRecipients} ->
    length _pushRecipients <= fromIntegral (fromRange limit)

mkPushes :: Natural -> [Push] -> [[V2.Push]]
mkPushes chunkSize = map (map toV2Push) . chunkPushes chunkSize

{-# INLINE [1] toV2Push #-}
toV2Push :: Push -> V2.Push
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
    toRecipient :: Recipient -> V2.Recipient
    toRecipient r =
      (recipient r._recipientUserId p._pushRoute)
        { V2._recipientClients = r._recipientClients
        }

{-# INLINE [1] chunkPushes #-}
chunkPushes :: Natural -> [Push] -> [[Push]]
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
    splitPush :: Natural -> Push -> (Push, Push)
    splitPush n p =
      let (r1, r2) = splitAt (fromIntegral n) (toList p._pushRecipients)
       in (p {_pushRecipients = fromJust $ nonEmpty r1}, p {_pushRecipients = fromJust $ nonEmpty r2})

pushSlowlyImpl ::
  ( Member Delay r,
    Member (Input NotificationSubsystemConfig) r,
    Member GundeckAPIAccess r,
    Member Async r
  ) =>
  [Push] ->
  Sem r ()
pushSlowlyImpl ps =
  for_ ps \p -> do
    delay =<< inputs slowPushDelay
    pushImpl [p]
