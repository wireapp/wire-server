{-# LANGUAGE TemplateHaskell #-}

module Wire.NotificationSubsystem where

import Control.Concurrent.Async (Async)
import Data.Aeson
import Data.Default
import Data.Id
import Imports
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
  { conn :: Maybe ConnId,
    transient :: Bool,
    route :: Route,
    nativePriority :: Maybe Priority,
    origin :: Maybe UserId,
    recipients :: [Recipient],
    json :: Object,
    apsData :: Maybe ApsData,
    isCellsEvent :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (Arbitrary) via GenericUniform Push

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
  SetupConsumableNotifications :: UserId -> ClientId -> NotificationSubsystem m ()

makeSem ''NotificationSubsystem

instance Default Push where
  def =
    Push
      { conn = Nothing,
        transient = False,
        route = RouteAny,
        nativePriority = Nothing,
        apsData = Nothing,
        json = mempty,
        origin = Nothing,
        recipients = [],
        isCellsEvent = False
      }

newPushLocal :: UserId -> Push
newPushLocal uid = def {origin = Just uid}
