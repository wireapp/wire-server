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
    isValidNotificationId,
    RawNotificationId (..),
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
    GetNotificationsResponse (..),
    userNotificationExchangeName,
    userNotificationDlxName,
    userNotificationDlqName,
    RabbitMqClientId (..),
    clientNotificationQueueName,
    userRoutingKey,
    temporaryRoutingKey,
    clientRoutingKey,
  )
where

import Control.Lens (makeLenses, (.~))
import Control.Lens.Operators ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types qualified as Aeson
import Data.Bits
import Data.ByteString.Conversion
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.SOP
import Data.Schema
import Data.Text.Encoding
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Imports
import Servant
import Wire.API.Routes.MultiVerb
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

type NotificationId = Id QueuedNotification

-- FUTUREWORK:
-- This definition is very opaque, but we know some of the structure already
-- (e.g. visible in 'modelEvent'). Can we specify it in a better way?
type Event = Aeson.Object

-- | Schema for an `Event` object.
--
-- This is basically a schema for a JSON object with some pre-defined structure.
eventSchema :: ValueSchema NamedSwaggerDoc Event
eventSchema = mkSchema sdoc Aeson.parseJSON (Just . Aeson.toJSON)
  where
    sdoc :: NamedSwaggerDoc
    sdoc =
      swaggerDoc @Aeson.Object
        & S.schema . S.title ?~ "Event"
        & S.schema . S.description ?~ "A single notification event"
        & S.schema . S.properties
          .~ InsOrdHashMap.fromList
            [ ( "type",
                S.Inline (S.toSchema (Proxy @Text) & S.description ?~ "Event type")
              )
            ]

isValidNotificationId :: NotificationId -> Bool
isValidNotificationId (Id uuid) =
  -- check that the version bits are set to 1
  case UUID.toWords uuid of
    (_, w, _, _) -> (w `shiftR` 12) .&. 0xf == 1

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
    objectWithDocModifier "QueuedNotification" queuedNotificationDoc $
      QueuedNotification
        <$> _queuedNotificationId
          .= field "id" schema
        <*> _queuedNotificationPayload
          .= fieldWithDocModifier "payload" payloadDoc (nonEmptyArray eventSchema)
    where
      queuedNotificationDoc = description ?~ "A single notification"
      payloadDoc d = d & description ?~ "List of events"

makeLenses ''QueuedNotification

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

instance ToSchema QueuedNotificationList where
  schema =
    objectWithDocModifier "QueuedNotificationList" queuedNotificationListDoc $
      QueuedNotificationList
        <$> _queuedNotifications
          .= fieldWithDocModifier "notifications" notificationsDoc (array schema)
        <*> _queuedHasMore
          .= fmap (fromMaybe False) (optFieldWithDocModifier "has_more" hasMoreDoc schema)
        <*> _queuedTime
          .= maybe_ (optField "time" utcTimeSchema)
    where
      queuedNotificationListDoc = description ?~ "Zero or more notifications"
      notificationsDoc = description ?~ "Notifications"
      hasMoreDoc = description ?~ "Whether there are still more notifications."

makeLenses ''QueuedNotificationList

newtype RawNotificationId = RawNotificationId {unRawNotificationId :: ByteString}
  deriving stock (Eq, Show, Generic)

instance FromHttpApiData RawNotificationId where
  parseUrlPiece = pure . RawNotificationId . encodeUtf8

instance ToParamSchema RawNotificationId where
  toParamSchema _ = toParamSchema (Proxy @Text)

data GetNotificationsResponse
  = GetNotificationsWithStatusNotFound QueuedNotificationList
  | GetNotificationsSuccess QueuedNotificationList

instance AsUnion '[Respond 404 "Notification list" QueuedNotificationList, Respond 200 "Notification list" QueuedNotificationList] GetNotificationsResponse where
  toUnion (GetNotificationsSuccess xs) = S (Z (I xs))
  toUnion (GetNotificationsWithStatusNotFound xs) = Z (I xs)
  fromUnion (S (Z (I xs))) = GetNotificationsSuccess xs
  fromUnion (Z (I xs)) = GetNotificationsWithStatusNotFound xs
  fromUnion (S (S x)) = case x of {}

--------------------------------------------------------------------------------
-- RabbitMQ exchanges and queues

-- | The name of the RabbitMQ exchange to which user notifications are published.
userNotificationExchangeName :: Text
userNotificationExchangeName = "user-notifications"

-- | The name of the RabbitMQ dead letter exchange for user notifications.
userNotificationDlxName :: Text
userNotificationDlxName = "dead-user-notifications"

-- | The name of the RabbitMQ queue for dead-lettered user notifications.
userNotificationDlqName :: Text
userNotificationDlqName = "dead-user-notifications"

data RabbitMqClientId
  = RabbitMqClientId ClientId
  | RabbitMqTempId Text

instance ToByteString RabbitMqClientId where
  builder (RabbitMqClientId cid) = builder cid
  builder (RabbitMqTempId temp) = builder temp

clientNotificationQueueName :: UserId -> RabbitMqClientId -> Text
clientNotificationQueueName uid cid =
  "user-notifications." <> userRoutingKey uid <> "." <> rabbitMqClientToText cid

userRoutingKey :: UserId -> Text
userRoutingKey = idToText

clientRoutingKey :: UserId -> ClientId -> Text
clientRoutingKey uid cid = userRoutingKey uid <> "." <> clientToText cid

temporaryRoutingKey :: UserId -> Text
temporaryRoutingKey uid = userRoutingKey uid <> ".temporary"

rabbitMqClientToText :: RabbitMqClientId -> Text
rabbitMqClientToText (RabbitMqClientId cid) = clientToText cid
rabbitMqClientToText (RabbitMqTempId temp) = "temp-" <> temp
