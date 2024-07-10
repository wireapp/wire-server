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

module Wire.API.Internal.Notification
  ( -- * Notification
    Notification (..),
    NotificationId,

    -- * NotificationTarget
    NotificationTarget,
    target,
    targetUser,
    targetClients,

    -- * QueuedNotification (re-export)
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
import Data.List1
import Data.OpenApi qualified as Swagger
import Data.Schema qualified as S
import Imports
import Wire.API.Notification

-------------------------------------------------------------------------------
-- Notification

data Notification = Notification
  { ntfTransient :: !Bool,
    ntfPayload :: !(List1 Object)
  }
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via S.Schema Notification

instance S.ToSchema Notification where
  schema =
    S.object "Notification" $
      Notification
        <$> ntfTransient S..= (fromMaybe False <$> S.optField "transient" S.schema)
        <*> (toNonEmpty . ntfPayload) S..= fmap List1 (S.field "payload" (S.nonEmptyArray S.jsonObject))

--------------------------------------------------------------------------------
-- NotificationTarget

data NotificationTarget = NotificationTarget
  { _targetUser :: !UserId,
    _targetClients :: ![ClientId]
  }
  deriving (Eq, Show)

makeLenses ''NotificationTarget

target :: UserId -> NotificationTarget
target u = NotificationTarget u []

instance FromJSON NotificationTarget where
  parseJSON = withObject "NotificationTarget" $ \o ->
    NotificationTarget
      <$> o .: "user"
      <*> o .: "clients"

instance ToJSON NotificationTarget where
  toJSON (NotificationTarget u cs) =
    object
      [ "user" .= u,
        "clients" .= cs
      ]
