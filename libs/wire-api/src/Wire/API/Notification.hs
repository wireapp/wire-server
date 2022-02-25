{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as Aeson
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty)
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Time.Clock (UTCTime)
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

type NotificationId = Id QueuedNotification

-- FUTUREWORK:
-- This definition is very opaque, but we know some of the structure already
-- (e.g. visible in 'modelEvent'). Can we specify it in a better way?
type Event = Aeson.Object

modelEvent :: Doc.Model
modelEvent = Doc.defineModel "NotificationEvent" $ do
  Doc.description "A single event"
  Doc.property "type" Doc.string' $
    Doc.description "Event type"

--------------------------------------------------------------------------------
-- QueuedNotification

data QueuedNotification = QueuedNotification
  { _queuedNotificationId :: NotificationId,
    _queuedNotificationPayload :: NonEmpty Event
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform QueuedNotification)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema QueuedNotification)

queuedNotification :: NotificationId -> NonEmpty Event -> QueuedNotification
queuedNotification = QueuedNotification

instance ToSchema QueuedNotification where
  schema =
    object "QueuedNotification" $
      QueuedNotification
        <$> _queuedNotificationId .= field "id" schema
        <*> _queuedNotificationPayload .= field "payload" (nonEmptyArray jsonObject)

makeLenses ''QueuedNotification

modelNotification :: Doc.Model
modelNotification = Doc.defineModel "Notification" $ do
  Doc.description "A single notification"
  Doc.property "id" Doc.bytes' $
    Doc.description "Notification ID"
  Doc.property "payload" (Doc.array (Doc.ref modelEvent)) $
    Doc.description "List of events"

data QueuedNotificationList = QueuedNotificationList
  { _queuedNotifications :: [QueuedNotification],
    _queuedHasMore :: Bool,
    _queuedTime :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform QueuedNotificationList)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema QueuedNotificationList)

queuedNotificationList :: [QueuedNotification] -> Bool -> Maybe UTCTime -> QueuedNotificationList
queuedNotificationList = QueuedNotificationList

modelNotificationList :: Doc.Model
modelNotificationList = Doc.defineModel "NotificationList" $ do
  Doc.description "Zero or more notifications"
  Doc.property "notifications" (Doc.array (Doc.ref modelNotification)) $
    Doc.description "Notifications"
  Doc.property "has_more" Doc.bool' $
    Doc.description "Whether there are still more notifications."

instance ToSchema QueuedNotificationList where
  schema =
    object "QueuedNotificationList" $
      QueuedNotificationList
        <$> _queuedNotifications .= field "notifications" (array schema)
        <*> _queuedHasMore .= fmap (fromMaybe False) (optField "has_more" schema)
        <*> _queuedTime .= maybe_ (optField "time" utcTimeSchema)

makeLenses ''QueuedNotificationList
