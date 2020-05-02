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

module Wire.API.Notification
  ( NotificationId,
    Event,

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
import qualified Data.Aeson as JSON
import Data.Aeson ((.!=), (.:), (.:?), (.=), FromJSON (parseJSON), ToJSON (toJSON))
import Data.Id
import Data.Json.Util ((#))
import Data.List1
import Data.Time.Clock (UTCTime)
import Imports

type NotificationId = Id QueuedNotification

type Event = JSON.Object

--------------------------------------------------------------------------------
-- QueuedNotification

data QueuedNotification = QueuedNotification
  { _queuedNotificationId :: !NotificationId,
    _queuedNotificationPayload :: !(List1 Event)
  }
  deriving (Eq, Show)

queuedNotification :: NotificationId -> List1 Event -> QueuedNotification
queuedNotification = QueuedNotification

makeLenses ''QueuedNotification

data QueuedNotificationList = QueuedNotificationList
  { _queuedNotifications :: [QueuedNotification],
    _queuedHasMore :: !Bool,
    _queuedTime :: !(Maybe UTCTime)
  }

queuedNotificationList :: [QueuedNotification] -> Bool -> Maybe UTCTime -> QueuedNotificationList
queuedNotificationList = QueuedNotificationList

makeLenses ''QueuedNotificationList

instance FromJSON QueuedNotification where
  parseJSON = JSON.withObject "QueuedNotification" $ \o ->
    QueuedNotification <$> o .: "id"
      <*> o .: "payload"

instance ToJSON QueuedNotification where
  toJSON (QueuedNotification i p) =
    JSON.object
      [ "id" .= i,
        "payload" .= p
      ]

instance FromJSON QueuedNotificationList where
  parseJSON = JSON.withObject "QueuedNotificationList" $ \o ->
    QueuedNotificationList <$> o .: "notifications"
      <*> o .:? "has_more" .!= False
      <*> o .:? "time"

instance ToJSON QueuedNotificationList where
  toJSON (QueuedNotificationList ns more t) =
    JSON.object
      ( "notifications" .= ns
          # "has_more" .= more
          # "time" .= t
          # []
      )
