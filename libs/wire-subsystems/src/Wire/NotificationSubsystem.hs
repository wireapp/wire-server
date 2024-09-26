{-# LANGUAGE TemplateHaskell #-}

module Wire.NotificationSubsystem where

import Control.Concurrent.Async (Async)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)))
import Imports
import Network.AMQP
import Polysemy
import Wire.API.Push.V2 hiding (Push (..), Recipient, newPush)
import Wire.Arbitrary

data Recipient = Recipient
  { recipientUserId :: UserId,
    recipientClients :: RecipientClients
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving (Arbitrary) via GenericUniform Recipient

data Push = Push
  { _pushConn :: Maybe ConnId,
    _pushTransient :: Bool,
    _pushRoute :: Route,
    _pushNativePriority :: Maybe Priority,
    pushOrigin :: Maybe UserId,
    _pushRecipients :: NonEmpty Recipient,
    pushJson :: Object,
    _pushApsData :: Maybe ApsData
  }
  deriving stock (Eq, Generic, Show)
  deriving (Arbitrary) via GenericUniform Push

makeLenses ''Push

-- | This subsystem governs mechanisms to send notifications to users.
data NotificationSubsystem m a where
  -- | Bulk push notifications
  PushNotifications :: [Push] -> NotificationSubsystem m ()
  -- | Bulk push notifications, but slowly. This should be used when there are
  -- many notifications to be sent which could cause too much resource usage.
  PushNotificationsSlowly :: [Push] -> NotificationSubsystem m ()
  -- | Bulk push notifications, but async. This should be used when failure to
  -- send notifications is not critical.
  --
  -- See 'Polysemy.Async' to know more about the 'Maybe'
  PushNotificationAsync :: Push -> NotificationSubsystem m (Async (Maybe ()))
  CleanupUser :: UserId -> NotificationSubsystem m ()
  UnregisterPushClient :: UserId -> ClientId -> NotificationSubsystem m ()
  GetPushTokens :: UserId -> NotificationSubsystem m [PushToken]
  SetUpUserNotificationQueues :: MVar Channel -> UserId -> ClientId -> NotificationSubsystem m ()

makeSem ''NotificationSubsystem

newPush1 :: Maybe UserId -> Object -> NonEmpty Recipient -> Push
newPush1 from e rr =
  Push
    { _pushConn = Nothing,
      _pushTransient = False,
      _pushRoute = RouteAny,
      _pushNativePriority = Nothing,
      _pushApsData = Nothing,
      pushJson = e,
      pushOrigin = from,
      _pushRecipients = rr
    }

newPush :: Maybe UserId -> Object -> [Recipient] -> Maybe Push
newPush _ _ [] = Nothing
newPush u e (r : rr) = Just $ newPush1 u e (r :| rr)

newPushLocal :: UserId -> Object -> [Recipient] -> Maybe Push
newPushLocal uid = newPush (Just uid)

newPushLocal1 :: UserId -> Object -> NonEmpty Recipient -> Push
newPushLocal1 uid = newPush1 (Just uid)
