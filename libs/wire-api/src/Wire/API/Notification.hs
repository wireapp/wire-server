{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
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

    -- * Swagger
    modelEvent,
    modelNotification,
    modelNotificationList,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as JSON
import Data.Id
import Data.Json.Util ((#))
import Data.List1
import qualified Data.Swagger.Build.Api as Doc
import Data.Time.Clock (UTCTime)
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

type NotificationId = Id QueuedNotification

-- FUTUREWORK:
-- This definition is very opaque, but we know some of the structure already
-- (e.g. visible in 'modelEvent'). Can we specify it in a better way?
type Event = JSON.Object

modelEvent :: Doc.Model
modelEvent = Doc.defineModel "NotificationEvent" $ do
  Doc.description "A single event"
  Doc.property "type" Doc.string' $
    Doc.description "Event type"

--------------------------------------------------------------------------------
-- QueuedNotification

data QueuedNotification = QueuedNotification
  { _queuedNotificationId :: NotificationId,
    _queuedNotificationPayload :: List1 Event
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform QueuedNotification)

queuedNotification :: NotificationId -> List1 Event -> QueuedNotification
queuedNotification = QueuedNotification

makeLenses ''QueuedNotification

modelNotification :: Doc.Model
modelNotification = Doc.defineModel "Notification" $ do
  Doc.description "A single notification"
  Doc.property "id" Doc.bytes' $
    Doc.description "Notification ID"
  Doc.property "payload" (Doc.array (Doc.ref modelEvent)) $
    Doc.description "List of events"

instance ToJSON QueuedNotification where
  toJSON (QueuedNotification i p) =
    JSON.object
      [ "id" .= i,
        "payload" .= p
      ]

instance FromJSON QueuedNotification where
  parseJSON = JSON.withObject "QueuedNotification" $ \o ->
    QueuedNotification
      <$> o .: "id"
      <*> o .: "payload"

data QueuedNotificationList = QueuedNotificationList
  { _queuedNotifications :: [QueuedNotification],
    _queuedHasMore :: Bool,
    _queuedTime :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform QueuedNotificationList)

queuedNotificationList :: [QueuedNotification] -> Bool -> Maybe UTCTime -> QueuedNotificationList
queuedNotificationList = QueuedNotificationList

makeLenses ''QueuedNotificationList

modelNotificationList :: Doc.Model
modelNotificationList = Doc.defineModel "NotificationList" $ do
  Doc.description "Zero or more notifications"
  Doc.property "notifications" (Doc.array (Doc.ref modelNotification)) $
    Doc.description "Notifications"
  Doc.property "has_more" Doc.bool' $
    Doc.description "Whether there are still more notifications."

instance ToJSON QueuedNotificationList where
  toJSON (QueuedNotificationList ns more t) =
    JSON.object
      ( "notifications" .= ns
          # "has_more" .= more
          # "time" .= t
          # []
      )

instance FromJSON QueuedNotificationList where
  parseJSON = JSON.withObject "QueuedNotificationList" $ \o ->
    QueuedNotificationList
      <$> o .: "notifications"
      <*> o .:? "has_more" .!= False
      <*> o .:? "time"
