{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gundeck.Types.Notification
  ( -- * Notification
    Notification (..),
    NotificationId,

    -- * NotificationTarget
    NotificationTarget,
    target,
    targetUser,
    targetClients,

    -- * QueuedNotification
    QueuedNotification,
    queuedNotification,
    queuedNotificationId,
    queuedNotificationPayload,
    QueuedNotificationList,
    queuedNotificationList,
    queuedNotifications,
    queuedHasMore,
    queuedTime,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.Json.Util
import Data.List1
import Data.Time.Clock (UTCTime)
import Imports

-------------------------------------------------------------------------------
-- Notification

data Notification
  = Notification
      { ntfId :: !NotificationId,
        ntfTransient :: !Bool,
        ntfPayload :: !(List1 Object)
      }
  deriving (Eq, Show)

type NotificationId = Id Notification

instance FromJSON Notification where
  parseJSON = withObject "notification" $ \o ->
    Notification <$> o .: "id"
      <*> o .:? "transient" .!= False
      <*> o .: "payload"

instance ToJSON Notification where
  toJSON (Notification i t p) =
    object
      [ "id" .= i,
        "transient" .= t,
        "payload" .= p
      ]

--------------------------------------------------------------------------------
-- NotificationTarget

data NotificationTarget
  = NotificationTarget
      { _targetUser :: !UserId,
        _targetClients :: ![ClientId]
      }
  deriving (Eq, Show)

makeLenses ''NotificationTarget

target :: UserId -> NotificationTarget
target u = NotificationTarget u []

instance FromJSON NotificationTarget where
  parseJSON = withObject "NotificationTarget" $ \o ->
    NotificationTarget <$> o .: "user"
      <*> o .: "clients"

instance ToJSON NotificationTarget where
  toJSON (NotificationTarget u cs) =
    object
      [ "user" .= u,
        "clients" .= cs
      ]

--------------------------------------------------------------------------------
-- QueuedNotification

data QueuedNotification
  = QueuedNotification
      { _queuedNotificationId :: !NotificationId,
        _queuedNotificationPayload :: !(List1 Object)
      }
  deriving (Eq, Show)

queuedNotification :: NotificationId -> List1 Object -> QueuedNotification
queuedNotification = QueuedNotification

makeLenses ''QueuedNotification

data QueuedNotificationList
  = QueuedNotificationList
      { _queuedNotifications :: [QueuedNotification],
        _queuedHasMore :: !Bool,
        _queuedTime :: !(Maybe UTCTime)
      }

queuedNotificationList :: [QueuedNotification] -> Bool -> Maybe UTCTime -> QueuedNotificationList
queuedNotificationList = QueuedNotificationList

makeLenses ''QueuedNotificationList

instance FromJSON QueuedNotification where
  parseJSON = withObject "QueuedNotification" $ \o ->
    QueuedNotification <$> o .: "id"
      <*> o .: "payload"

instance ToJSON QueuedNotification where
  toJSON (QueuedNotification i p) =
    object
      [ "id" .= i,
        "payload" .= p
      ]

instance FromJSON QueuedNotificationList where
  parseJSON = withObject "QueuedNotificationList" $ \o ->
    QueuedNotificationList <$> o .: "notifications"
      <*> o .:? "has_more" .!= False
      <*> o .:? "time"

instance ToJSON QueuedNotificationList where
  toJSON (QueuedNotificationList ns more t) =
    object
      ( "notifications" .= ns
          # "has_more" .= more
          # "time" .= t
          # []
      )
