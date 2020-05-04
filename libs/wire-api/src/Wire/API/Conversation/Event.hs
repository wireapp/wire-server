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
  ( -- * Galley conversation types
    Connect (..),

    -- * Events
    Event (..),
    EventType (..),
    EventData (..),
    UserIdList (..),
    SimpleMember (..),
    SimpleMembers (..),
    MemberUpdateData (..),
    OtrMessage (..),
    parseEventData,

    -- * Other galley types
    ConversationRename (..),
    ConversationAccessUpdate (..),
    ConversationReceiptModeUpdate (..),
    ConversationMessageTimerUpdate (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util ((#), ToJSONObject (toJSONObject), toUTCTimeMillis)
import Data.Misc
import Data.Time
import Imports
import URI.ByteString ()
import Wire.API.Conversation
import qualified Wire.API.Conversation.Code as Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing (TypingData)

-- Conversations ------------------------------------------------------------

newtype ConversationRename = ConversationRename
  { cupName :: Text
  }

deriving instance Eq ConversationRename

deriving instance Show ConversationRename

data ConversationAccessUpdate = ConversationAccessUpdate
  { cupAccess :: [Access],
    cupAccessRole :: AccessRole
  }
  deriving (Eq, Show)

data ConversationReceiptModeUpdate = ConversationReceiptModeUpdate
  { cruReceiptMode :: !ReceiptMode
  }
  deriving (Eq, Show)

data ConversationMessageTimerUpdate = ConversationMessageTimerUpdate
  { -- | New message timer
    cupMessageTimer :: !(Maybe Milliseconds)
  }
  deriving (Eq, Show)

-- Events -------------------------------------------------------------------

-- FUTUREWORK(federation, #1213):
-- Conversation and user ID can be remote IDs, but the receiver might be on
-- another backend, so mapped IDs don't work for them.
data Event = Event
  { evtType :: !EventType,
    evtConv :: !ConvId,
    evtFrom :: !UserId,
    evtTime :: !UTCTime,
    evtData :: !(Maybe EventData)
  }
  deriving (Eq, Generic)

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

data OtrMessage = OtrMessage
  { otrSender :: !ClientId,
    otrRecipient :: !ClientId,
    otrCiphertext :: !Text,
    otrData :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

newtype SimpleMembers = SimpleMembers
  { mMembers :: [SimpleMember]
  }
  deriving (Eq, Show, Generic)

data SimpleMember = SimpleMember
  { smId :: !UserId,
    smConvRoleName :: !RoleName
  }
  deriving (Eq, Show, Generic)

-- | This datatype replaces the old `Members` datatype,
-- which has been replaced by `SimpleMembers`. This is
-- needed due to backwards compatible reasons since old
-- clients will break if we switch these types. Also, this
-- definition represents better what information it carries
newtype UserIdList = UserIdList
  { mUsers :: [UserId]
  }
  deriving (Eq, Show, Generic)

data Connect = Connect
  { cRecipient :: !UserId,
    cMessage :: !(Maybe Text),
    cName :: !(Maybe Text),
    cEmail :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

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

-- Instances ----------------------------------------------------------------

-- JSON

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

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    t <- o .: "type"
    d <- o .: "data"
    Event t <$> o .: "conversation"
      <*> o .: "from"
      <*> o .: "time"
      <*> parseEventData t d

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

instance ToJSON ConversationAccessUpdate where
  toJSON c =
    object $
      "access" .= cupAccess c
        # "access_role" .= cupAccessRole c
        # []

instance FromJSON ConversationAccessUpdate where
  parseJSON = withObject "conversation-access-update" $ \o ->
    ConversationAccessUpdate <$> o .: "access"
      <*> o .: "access_role"

instance FromJSON ConversationReceiptModeUpdate where
  parseJSON = withObject "conversation-receipt-mode-update" $ \o ->
    ConversationReceiptModeUpdate <$> o .: "receipt_mode"

instance ToJSON ConversationReceiptModeUpdate where
  toJSON c =
    object
      [ "receipt_mode" .= cruReceiptMode c
      ]

instance ToJSON ConversationMessageTimerUpdate where
  toJSON c =
    object
      [ "message_timer" .= cupMessageTimer c
      ]

instance FromJSON ConversationMessageTimerUpdate where
  parseJSON = withObject "conversation-message-timer-update" $ \o ->
    ConversationMessageTimerUpdate <$> o .:? "message_timer"

instance FromJSON ConversationRename where
  parseJSON = withObject "conversation-rename object" $ \c ->
    ConversationRename <$> c .: "name"

instance ToJSON ConversationRename where
  toJSON cu = object ["name" .= cupName cu]

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

instance FromJSON UserIdList where
  parseJSON = withObject "user-ids-payload" $ \o ->
    UserIdList <$> o .: "user_ids"

instance ToJSON UserIdList where
  toJSON e = object ["user_ids" .= mUsers e]

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

instance ToJSON SimpleMembers where
  toJSON e =
    object
      [ "user_ids" .= fmap smId (mMembers e),
        "users" .= mMembers e
      ]

instance FromJSON Connect where
  parseJSON = withObject "connect" $ \o ->
    Connect <$> o .: "recipient"
      <*> o .:? "message"
      <*> o .:? "name"
      <*> o .:? "email"

instance ToJSON Connect where
  toJSON c =
    object
      [ "recipient" .= cRecipient c,
        "message" .= cMessage c,
        "name" .= cName c,
        "email" .= cEmail c
      ]
