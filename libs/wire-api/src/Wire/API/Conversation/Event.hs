{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

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

module Wire.API.Conversation.Event
  ( -- * Event
    Event (..),
    EventType (..),
    EventData (..),
    parseEventData, -- TODO: needed?

    -- * Event data helpers
    SimpleMember (..),
    SimpleMembers (..),
    Connect (..),
    MemberUpdateData (..),
    OtrMessage (..),

    -- * Swagger
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util ((#), ToJSONObject (toJSONObject), toUTCTimeMillis)
import Data.Time
import Imports
import URI.ByteString ()
import Wire.API.Conversation
import qualified Wire.API.Conversation.Code as Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing (TypingData)
import Wire.API.User (UserIdList)

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { evtType :: !EventType,
    evtConv :: !ConvId,
    evtFrom :: !UserId,
    evtTime :: !UTCTime,
    evtData :: !(Maybe EventData)
  }
  deriving (Eq, Generic)

instance ToJSONObject Event where
  toJSONObject e =
    HashMap.fromList
      [ "type" .= evtType e,
        "conversation" .= evtConv e,
        "from" .= evtFrom e,
        "time" .= toUTCTimeMillis (evtTime e),
        "data" .= evtData e
      ]

instance ToJSON Event where
  toJSON = Object . toJSONObject

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    t <- o .: "type"
    d <- o .: "data"
    Event t <$> o .: "conversation"
      <*> o .: "from"
      <*> o .: "time"
      <*> parseEventData t d

data EventType
  = MemberJoin
  | MemberLeave
  | MemberStateUpdate
  | ConvRename
  | ConvAccessUpdate
  | ConvMessageTimerUpdate
  | ConvCodeUpdate
  | ConvCodeDelete
  | ConvCreate
  | ConvConnect
  | ConvDelete
  | ConvReceiptModeUpdate
  | OtrMessageAdd
  | Typing
  deriving (Eq, Show, Generic)

instance ToJSON EventType where
  toJSON MemberJoin = String "conversation.member-join"
  toJSON MemberLeave = String "conversation.member-leave"
  toJSON MemberStateUpdate = String "conversation.member-update"
  toJSON ConvRename = String "conversation.rename"
  toJSON ConvAccessUpdate = String "conversation.access-update"
  toJSON ConvMessageTimerUpdate = String "conversation.message-timer-update"
  toJSON ConvCodeUpdate = String "conversation.code-update"
  toJSON ConvCodeDelete = String "conversation.code-delete"
  toJSON ConvCreate = String "conversation.create"
  toJSON ConvDelete = String "conversation.delete"
  toJSON ConvConnect = String "conversation.connect-request"
  toJSON ConvReceiptModeUpdate = String "conversation.receipt-mode-update"
  toJSON Typing = String "conversation.typing"
  toJSON OtrMessageAdd = String "conversation.otr-message-add"

instance FromJSON EventType where
  parseJSON (String "conversation.member-join") = return MemberJoin
  parseJSON (String "conversation.member-leave") = return MemberLeave
  parseJSON (String "conversation.rename") = return ConvRename
  parseJSON (String "conversation.access-update") = return ConvAccessUpdate
  parseJSON (String "conversation.message-timer-update") = return ConvMessageTimerUpdate
  parseJSON (String "conversation.code-update") = return ConvCodeUpdate
  parseJSON (String "conversation.code-delete") = return ConvCodeDelete
  parseJSON (String "conversation.member-update") = return MemberStateUpdate
  parseJSON (String "conversation.create") = return ConvCreate
  parseJSON (String "conversation.delete") = return ConvDelete
  parseJSON (String "conversation.connect-request") = return ConvConnect
  parseJSON (String "conversation.receipt-mode-update") = return ConvReceiptModeUpdate
  parseJSON (String "conversation.typing") = return Typing
  parseJSON (String "conversation.otr-message-add") = return OtrMessageAdd
  parseJSON x = fail $ "No event-type: " <> show (encode x)

-- FUTUREWORK(federation, #1213):
-- A lot of information in the events can contain remote IDs, but the
-- receiver might be on another backend, so mapped IDs don't work for them.
data EventData
  = EdMembersJoin !SimpleMembers
  | EdMembersLeave !UserIdList
  | EdConnect !Connect
  | EdConvReceiptModeUpdate !ConversationReceiptModeUpdate
  | EdConvRename !ConversationRename
  | EdConvAccessUpdate !ConversationAccessUpdate
  | EdConvMessageTimerUpdate !ConversationMessageTimerUpdate
  | EdConvCodeUpdate !Code.ConversationCode
  | EdMemberUpdate !MemberUpdateData
  | EdConversation !Conversation
  | EdTyping !TypingData
  | EdOtrMessage !OtrMessage
  deriving (Eq, Show, Generic)

-- This instance doesn't take the event type into account.
-- It should only be used as part of serializing a whole 'Event'.
instance ToJSON EventData where
  toJSON (EdMembersJoin x) = toJSON x
  toJSON (EdMembersLeave x) = toJSON x
  toJSON (EdConnect x) = toJSON x
  toJSON (EdConvRename x) = toJSON x
  toJSON (EdConvAccessUpdate x) = toJSON x
  toJSON (EdConvMessageTimerUpdate x) = toJSON x
  toJSON (EdConvCodeUpdate x) = toJSON x
  toJSON (EdConvReceiptModeUpdate x) = toJSON x
  toJSON (EdMemberUpdate x) = toJSON x
  toJSON (EdConversation x) = toJSON x
  toJSON (EdTyping x) = toJSON x
  toJSON (EdOtrMessage x) = toJSON x

parseEventData :: EventType -> Value -> Parser (Maybe EventData)
parseEventData MemberJoin v = Just . EdMembersJoin <$> parseJSON v
parseEventData MemberLeave v = Just . EdMembersLeave <$> parseJSON v
parseEventData MemberStateUpdate v = Just . EdMemberUpdate <$> parseJSON v
parseEventData ConvRename v = Just . EdConvRename <$> parseJSON v
parseEventData ConvAccessUpdate v = Just . EdConvAccessUpdate <$> parseJSON v
parseEventData ConvMessageTimerUpdate v = Just . EdConvMessageTimerUpdate <$> parseJSON v
parseEventData ConvCodeUpdate v = Just . EdConvCodeUpdate <$> parseJSON v
parseEventData ConvCodeDelete _ = pure Nothing
parseEventData ConvConnect v = Just . EdConnect <$> parseJSON v
parseEventData ConvCreate v = Just . EdConversation <$> parseJSON v
parseEventData ConvReceiptModeUpdate v = Just . EdConvReceiptModeUpdate <$> parseJSON v
parseEventData Typing v = Just . EdTyping <$> parseJSON v
parseEventData OtrMessageAdd v = Just . EdOtrMessage <$> parseJSON v
parseEventData ConvDelete _ = pure Nothing

--------------------------------------------------------------------------------
-- Event data helpers

newtype SimpleMembers = SimpleMembers
  { mMembers :: [SimpleMember]
  }
  deriving (Eq, Show, Generic)

instance ToJSON SimpleMembers where
  toJSON e =
    object
      [ "user_ids" .= fmap smId (mMembers e),
        "users" .= mMembers e
      ]

instance FromJSON SimpleMembers where
  parseJSON = withObject "simple-members-payload" $ \o -> do
    users <- o .:? "users" -- This is to make migration easier and not dependent on deployment ordering
    membs <- case users of
      Just mems -> pure mems
      Nothing -> do
        ids <- o .:? "user_ids"
        case ids of
          Just userIds -> pure $ fmap (\u -> SimpleMember u roleNameWireAdmin) userIds
          Nothing -> fail "Not possible!"
    pure $ SimpleMembers membs

data SimpleMember = SimpleMember
  { smId :: !UserId,
    smConvRoleName :: !RoleName
  }
  deriving (Eq, Show, Generic)

instance ToJSON SimpleMember where
  toJSON m =
    object
      [ "id" .= smId m,
        "conversation_role" .= smConvRoleName m
      ]

instance FromJSON SimpleMember where
  parseJSON = withObject "simple member object" $ \o ->
    SimpleMember <$> o .: "id"
      <*> o .:? "conversation_role" .!= roleNameWireAdmin

data Connect = Connect
  { cRecipient :: !UserId,
    cMessage :: !(Maybe Text),
    cName :: !(Maybe Text),
    cEmail :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance ToJSON Connect where
  toJSON c =
    object
      [ "recipient" .= cRecipient c,
        "message" .= cMessage c,
        "name" .= cName c,
        "email" .= cEmail c
      ]

instance FromJSON Connect where
  parseJSON = withObject "connect" $ \o ->
    Connect <$> o .: "recipient"
      <*> o .:? "message"
      <*> o .:? "name"
      <*> o .:? "email"

-- | Outbound member updates. When a user A acts upon a user B,
-- then a user event is generated where B's user ID is set
-- as misTarget.
-- Used for events (sent over the websocket, etc.).  See also
-- 'MemberUpdate' and 'OtherMemberUpdate'.
data MemberUpdateData = MemberUpdateData
  { -- | Target user of this action, should not be optional anymore.
    -- <https://github.com/zinfra/backend-issues/issues/1309>
    misTarget :: !(Maybe UserId),
    misOtrMuted :: !(Maybe Bool),
    misOtrMutedStatus :: !(Maybe MutedStatus),
    misOtrMutedRef :: !(Maybe Text),
    misOtrArchived :: !(Maybe Bool),
    misOtrArchivedRef :: !(Maybe Text),
    misHidden :: !(Maybe Bool),
    misHiddenRef :: !(Maybe Text),
    misConvRoleName :: !(Maybe RoleName)
  }
  deriving (Eq, Show, Generic)

instance ToJSON MemberUpdateData where
  toJSON m =
    object $
      "target" .= misTarget m
        # "otr_muted" .= misOtrMuted m
        # "otr_muted_status" .= misOtrMutedStatus m
        # "otr_muted_ref" .= misOtrMutedRef m
        # "otr_archived" .= misOtrArchived m
        # "otr_archived_ref" .= misOtrArchivedRef m
        # "hidden" .= misHidden m
        # "hidden_ref" .= misHiddenRef m
        # "conversation_role" .= misConvRoleName m
        # []

instance FromJSON MemberUpdateData where
  parseJSON = withObject "member-update event data" $ \m ->
    MemberUpdateData <$> m .:? "target"
      -- NOTE: This is really not a maybe and should
      --       be made compulsory 28 days after the next
      --       release to prod to guaratee that no events
      --       out there do not contain id.
      --       Making it compulsory now creates a bit of
      --       a fragile parser
      <*> m .:? "otr_muted"
      <*> m .:? "otr_muted_status"
      <*> m .:? "otr_muted_ref"
      <*> m .:? "otr_archived"
      <*> m .:? "otr_archived_ref"
      <*> m .:? "hidden"
      <*> m .:? "hidden_ref"
      <*> m .:? "conversation_role"

data OtrMessage = OtrMessage
  { otrSender :: !ClientId,
    otrRecipient :: !ClientId,
    otrCiphertext :: !Text,
    otrData :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance ToJSON OtrMessage where
  toJSON m =
    object $
      "sender" .= otrSender m
        # "recipient" .= otrRecipient m
        # "text" .= otrCiphertext m
        # "data" .= otrData m
        # []

instance FromJSON OtrMessage where
  parseJSON = withObject "otr-message" $ \o ->
    OtrMessage <$> o .: "sender"
      <*> o .: "recipient"
      <*> o .: "text"
      <*> o .:? "data"
