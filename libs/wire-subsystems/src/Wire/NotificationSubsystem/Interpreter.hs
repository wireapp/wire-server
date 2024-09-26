module Wire.NotificationSubsystem.Interpreter where

import Bilge (RequestId)
import Control.Concurrent.Async (Async)
import Control.Lens (set, (.~))
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id (ClientId, UserId, idToText)
import Data.List.NonEmpty (nonEmpty)
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Proxy
import Data.Range
import Data.Set qualified as Set
import Data.Text.Encoding
import Data.Time.Clock.DiffTime
import Imports
import Network.AMQP
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Async (async, sequenceConcurrently)
import Polysemy.Async qualified as P
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class as Log
import System.Timeout (timeout)
import Wire.API.Notification (userNotificationExchangeName)
import Wire.API.Push.V2 hiding (Push (..), Recipient, newPush)
import Wire.API.Push.V2 qualified as V2
import Wire.API.Team.Member
import Wire.GundeckAPIAccess (GundeckAPIAccess)
import Wire.GundeckAPIAccess qualified as GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Error
import Wire.Sem.Delay

-- | We interpret this using 'GundeckAPIAccess' so we can mock it out for testing.
runNotificationSubsystemGundeck ::
  ( Member GundeckAPIAccess r,
    Member P.Async r,
    Member Delay r,
    Member (Final IO) r,
    Member P.TinyLog r,
    Member (Embed IO) r,
    Member (Error NotificationSubsystemError) r
  ) =>
  NotificationSubsystemConfig ->
  Sem (NotificationSubsystem : r) a ->
  Sem r a
runNotificationSubsystemGundeck cfg = interpret $ \case
  PushNotifications ps -> runInputConst cfg $ pushImpl ps
  PushNotificationsSlowly ps -> runInputConst cfg $ pushSlowlyImpl ps
  PushNotificationAsync ps -> runInputConst cfg $ pushAsyncImpl ps
  CleanupUser uid -> GundeckAPIAccess.userDeleted uid
  UnregisterPushClient uid cid -> GundeckAPIAccess.unregisterPushClient uid cid
  GetPushTokens uid -> GundeckAPIAccess.getPushTokens uid
  SetUpUserNotificationQueues chan uid cid -> setUpUserNotificationQueuesImpl chan uid cid

data NotificationSubsystemConfig = NotificationSubsystemConfig
  { fanoutLimit :: Range 1 HardTruncationLimit Int32,
    chunkSize :: Natural,
    slowPushDelay :: DiffTime,
    requestId :: RequestId
  }

defaultNotificationSubsystemConfig :: RequestId -> NotificationSubsystemConfig
defaultNotificationSubsystemConfig reqId =
  NotificationSubsystemConfig defaultFanoutLimit defaultChunkSize defaultSlowPushDelay reqId

defaultFanoutLimit :: Range 1 HardTruncationLimit Int32
defaultFanoutLimit = toRange (Proxy @HardTruncationLimit)

defaultChunkSize :: Natural
defaultChunkSize = 128

defaultSlowPushDelay :: DiffTime
defaultSlowPushDelay = millisecondsToDiffTime 20

pushAsyncImpl ::
  forall r.
  ( Member GundeckAPIAccess r,
    Member (Input NotificationSubsystemConfig) r,
    Member P.Async r,
    Member (Final IO) r,
    Member P.TinyLog r
  ) =>
  Push ->
  Sem r (Async (Maybe ()))
pushAsyncImpl p = async $ do
  reqId <- inputs requestId
  errorToIOFinal @SomeException (fromExceptionSem @SomeException $ pushImpl [p]) >>= \case
    Left e ->
      P.err $
        Log.msg (Log.val "Error while pushing notifications")
          . Log.field "requestId" reqId
          . Log.field "error" (displayException e)
    Right _ -> pure ()

pushImpl ::
  forall r.
  ( Member GundeckAPIAccess r,
    Member (Input NotificationSubsystemConfig) r,
    Member P.Async r
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
      (recipient r.recipientUserId p._pushRoute)
        { V2._recipientClients = r.recipientClients
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
    Member P.Async r
  ) =>
  [Push] ->
  Sem r ()
pushSlowlyImpl ps =
  for_ ps \p -> do
    delay =<< inputs (diffTimeToFullMicroseconds . slowPushDelay)
    pushImpl [p]

setUpUserNotificationQueuesImpl ::
  ( Member (Embed IO) r,
    Member P.TinyLog r,
    Member (Error NotificationSubsystemError) r
  ) =>
  MVar Channel ->
  UserId ->
  ClientId ->
  Sem r ()
setUpUserNotificationQueuesImpl chanMVar uid cid = do
  let qName = idToText uid
  let cidText = decodeUtf8 $ toByteString' cid
  let routingKeys =
        [ qName,
          qName <> "." <> cidText
        ]
  mChan <- liftIO $ timeout 1_000_000 $ readMVar chanMVar
  case mChan of
    Just chan -> liftIO $ do
      void $ declareQueue chan newQueue {queueName = qName}
      for_ routingKeys $ bindQueue chan qName userNotificationExchangeName
    Nothing -> do
      P.err $ Log.msg (Log.val "RabbitMQ connection error")
      throw NotificationSubsystemConnectionError
