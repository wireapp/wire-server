{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
