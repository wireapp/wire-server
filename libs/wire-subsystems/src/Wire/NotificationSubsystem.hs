{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.NotificationSubsystem where

import Control.Concurrent.Async (Async)
import Control.Lens (view)
import Data.Aeson
import Data.Default
import Data.Id
import Imports
import Polysemy
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Galley.Notifications (ConversationUpdate)
import Wire.API.Push.V2 hiding (Push (..), Recipient, newPush)
import Wire.API.Push.V2 qualified as PushV2
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as Mem
import Wire.Arbitrary
import Wire.StoredConversation (LocalMember (..))

data Recipient = Recipient
  { recipientUserId :: UserId,
    recipientClients :: RecipientClients
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving (Arbitrary) via GenericUniform Recipient

userRecipient :: UserId -> Recipient
userRecipient u = Recipient u PushV2.RecipientClientsAll

membersToRecipients :: Maybe UserId -> [TeamMember] -> [Recipient]
membersToRecipients Nothing = map (userRecipient . view Mem.userId)
membersToRecipients (Just u) = map userRecipient . filter (/= u) . map (view Mem.userId)

localMemberToRecipient :: LocalMember -> Recipient
localMemberToRecipient = userRecipient . (.id_)

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

data LocalConversationUpdate = LocalConversationUpdate
  { lcuEvent :: Event,
    lcuUpdate :: ConversationUpdate
  }
  deriving (Show)

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
