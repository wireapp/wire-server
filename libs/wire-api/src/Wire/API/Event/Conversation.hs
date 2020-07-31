{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.Event.Conversation
  ( -- * Event
    Event (..),
    EventType (..),
    EventData (..),

    -- * Event data helpers
    SimpleMember (..),
    SimpleMembers (..),
    Connect (..),
    MemberUpdateData (..),
    OtrMessage (..),

    -- * re-exports
    ConversationReceiptModeUpdate (..),
    ConversationRename (..),
    ConversationAccessUpdate (..),
    ConversationMessageTimerUpdate (..),
    ConversationCode (..),
    Conversation (..),
    TypingData (..),
    UserIdList (..),

    -- * Swagger
    modelEvent,
    modelMemberEvent,
    modelConnectEvent,
    modelConversationReceiptModeUpdateEvent,
    modelConversationNameUpdateEvent,
    modelConversationAccessUpdateEvent,
    modelConversationMessageTimerUpdateEvent,
    modelConversationCodeUpdateEvent,
    modelConversationCodeDeleteEvent,
    modelMemberUpdateEvent,
    modelTypingEvent,
    modelOtrMessageEvent,
    modelMembers,
    modelConnect,
    modelMemberUpdateData,
    modelOtrMessage,
    typeEventType,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject), toUTCTimeMillis, (#))
import qualified Data.Swagger.Build.Api as Doc
import Data.Time
import Imports
import qualified Test.QuickCheck as QC
import URI.ByteString ()
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.Conversation
import Wire.API.Conversation (modelConversationAccessUpdate, modelConversationMessageTimerUpdate, modelConversationReceiptModeUpdate, modelConversationUpdateName)
import Wire.API.Conversation.Code (ConversationCode (..), modelConversationCode)
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing (TypingData (..), modelTyping)
import Wire.API.User (UserIdList (..))

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { evtType :: EventType,
    evtConv :: ConvId,
    evtFrom :: UserId,
    evtTime :: UTCTime,
    evtData :: Maybe EventData
  }
  deriving stock (Eq, Show, Generic)

modelEvent :: Doc.Model
modelEvent = Doc.defineModel "Event" $ do
  Doc.description "Event data"
  Doc.property "type" typeEventType $
    Doc.description "Event type"
  Doc.property "conversation" Doc.bytes' $
    Doc.description "Conversation ID"
  Doc.property "from" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "time" Doc.dateTime' $
    Doc.description "Date and time this event occurred"
  -- This doesn't really seem to work in swagger-ui.
  -- The children/subTypes are not displayed.
  Doc.children
    "type"
    [ modelMemberEvent,
      modelConnectEvent,
      modelConversationReceiptModeUpdateEvent,
      modelConversationNameUpdateEvent,
      modelConversationAccessUpdateEvent,
      modelConversationMessageTimerUpdateEvent,
      modelConversationCodeUpdateEvent,
      modelConversationCodeDeleteEvent,
      modelMemberUpdateEvent,
      modelTypingEvent,
      modelOtrMessageEvent
    ]

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
    Event t
      <$> o .: "conversation"
      <*> o .: "from"
      <*> o .: "time"
      <*> parseEventData t d

instance Arbitrary Event where
  arbitrary = do
    typ <- arbitrary
    Event typ
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genEventData typ

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
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EventType)

typeEventType :: Doc.DataType
typeEventType =
  Doc.string $
    Doc.enum
      [ "conversation.member-join",
        "conversation.member-leave",
        "conversation.member-update",
        "conversation.rename",
        "conversation.access-update",
        "conversation.receipt-mode-update",
        "conversation.message-timer-update",
        "conversation.code-update",
        "conversation.code-delete",
        "conversation.create",
        "conversation.delete",
        "conversation.connect-request",
        "conversation.typing",
        "conversation.otr-message-add"
      ]

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
  = EdMembersJoin SimpleMembers
  | EdMembersLeave UserIdList
  | EdConnect Connect
  | EdConvReceiptModeUpdate ConversationReceiptModeUpdate
  | EdConvRename ConversationRename
  | EdConvAccessUpdate ConversationAccessUpdate
  | EdConvMessageTimerUpdate ConversationMessageTimerUpdate
  | EdConvCodeUpdate ConversationCode
  | EdMemberUpdate MemberUpdateData
  | EdConversation Conversation
  | EdTyping TypingData
  | EdOtrMessage OtrMessage
  deriving stock (Eq, Show, Generic)

modelMemberEvent :: Doc.Model
modelMemberEvent = Doc.defineModel "MemberEvent" $ do
  Doc.description "member event"
  Doc.property "data" (Doc.ref modelMembers) $ Doc.description "members data"

modelConnectEvent :: Doc.Model
modelConnectEvent = Doc.defineModel "ConnectEvent" $ do
  Doc.description "connect event"
  Doc.property "data" (Doc.ref modelConnect) $ Doc.description "connect data"

modelConversationReceiptModeUpdateEvent :: Doc.Model
modelConversationReceiptModeUpdateEvent = Doc.defineModel "ConversationReceiptModeUpdateEvent" $ do
  Doc.description "conversation receipt mode update event"
  Doc.property "data" (Doc.ref modelConversationReceiptModeUpdate) $ Doc.description "conversation receipt mode data"

modelConversationNameUpdateEvent :: Doc.Model
modelConversationNameUpdateEvent = Doc.defineModel "ConversationNameUpdateEvent" $ do
  Doc.description "conversation update event"
  Doc.property "data" (Doc.ref modelConversationUpdateName) $ Doc.description "conversation name"

modelConversationAccessUpdateEvent :: Doc.Model
modelConversationAccessUpdateEvent = Doc.defineModel "ConversationAccessUpdateEvent" $ do
  Doc.description "conversation access update event"
  Doc.property "data" (Doc.ref modelConversationAccessUpdate) $ Doc.description "conversation access data"

modelConversationMessageTimerUpdateEvent :: Doc.Model
modelConversationMessageTimerUpdateEvent = Doc.defineModel "ConversationMessageTimerUpdateEvent" $ do
  Doc.description "conversation message timer update event"
  Doc.property "data" (Doc.ref modelConversationMessageTimerUpdate) $ Doc.description "conversation message timer data"

modelConversationCodeUpdateEvent :: Doc.Model
modelConversationCodeUpdateEvent = Doc.defineModel "ConversationCodeUpdateEvent" $ do
  Doc.description "conversation code update event"
  Doc.property "data" (Doc.ref modelConversationCode) $ Doc.description "conversation code data"

modelConversationCodeDeleteEvent :: Doc.Model
modelConversationCodeDeleteEvent =
  Doc.defineModel "ConversationCodeDeleteEvent" $
    Doc.description "conversation code delete event"

modelMemberUpdateEvent :: Doc.Model
modelMemberUpdateEvent = Doc.defineModel "MemberUpdateEvent" $ do
  Doc.description "member update event"
  Doc.property "data" (Doc.ref modelMemberUpdateData) $ Doc.description "member data"

modelTypingEvent :: Doc.Model
modelTypingEvent = Doc.defineModel "TypingEvent" $ do
  Doc.description "typing event"
  Doc.property "data" (Doc.ref modelTyping) $ Doc.description "typing data"

modelOtrMessageEvent :: Doc.Model
modelOtrMessageEvent = Doc.defineModel "OtrMessage" $ do
  Doc.description "off-the-record message event"
  Doc.property "data" (Doc.ref modelOtrMessage) $ Doc.description "OTR message"

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

genEventData :: EventType -> QC.Gen (Maybe EventData)
genEventData = \case
  MemberJoin -> Just . EdMembersJoin <$> arbitrary
  MemberLeave -> Just . EdMembersLeave <$> arbitrary
  MemberStateUpdate -> Just . EdMemberUpdate <$> arbitrary
  ConvRename -> Just . EdConvRename <$> arbitrary
  ConvAccessUpdate -> Just . EdConvAccessUpdate <$> arbitrary
  ConvMessageTimerUpdate -> Just . EdConvMessageTimerUpdate <$> arbitrary
  ConvCodeUpdate -> Just . EdConvCodeUpdate <$> arbitrary
  ConvCodeDelete -> pure Nothing
  ConvConnect -> Just . EdConnect <$> arbitrary
  ConvCreate -> Just . EdConversation <$> arbitrary
  ConvReceiptModeUpdate -> Just . EdConvReceiptModeUpdate <$> arbitrary
  Typing -> Just . EdTyping <$> arbitrary
  OtrMessageAdd -> Just . EdOtrMessage <$> arbitrary
  ConvDelete -> pure Nothing

--------------------------------------------------------------------------------
-- Event data helpers

newtype SimpleMembers = SimpleMembers
  { mMembers :: [SimpleMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)

-- | Used both for 'SimpleMembers' and 'UserIdList'.
modelMembers :: Doc.Model
modelMembers =
  Doc.defineModel "Members" $
    Doc.property "users" (Doc.unique $ Doc.array Doc.bytes') $
      Doc.description "List of user IDs"

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
  { smId :: UserId,
    smConvRoleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SimpleMember)

instance ToJSON SimpleMember where
  toJSON m =
    object
      [ "id" .= smId m,
        "conversation_role" .= smConvRoleName m
      ]

instance FromJSON SimpleMember where
  parseJSON = withObject "simple member object" $ \o ->
    SimpleMember
      <$> o .: "id"
      <*> o .:? "conversation_role" .!= roleNameWireAdmin

data Connect = Connect
  { cRecipient :: UserId,
    cMessage :: Maybe Text,
    cName :: Maybe Text,
    cEmail :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Connect)

modelConnect :: Doc.Model
modelConnect = Doc.defineModel "Connect" $ do
  Doc.description "user to user connection request"
  Doc.property "recipient" Doc.bytes' $
    Doc.description "The user ID to connect to"
  Doc.property "message" Doc.string' $
    Doc.description "Initial message to send to user"
  Doc.property "name" Doc.string' $
    Doc.description "Name of requestor"
  Doc.property "email" Doc.string' $ do
    Doc.description "E-Mail of requestor"
    Doc.optional

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
    Connect
      <$> o .: "recipient"
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
    --
    -- FUTUREWORK: make it mandatory to guarantee that no events
    -- out there do not contain an ID.
    -- <https://github.com/zinfra/backend-issues/issues/1309>
    misTarget :: Maybe UserId,
    misOtrMuted :: Maybe Bool,
    misOtrMutedStatus :: Maybe MutedStatus,
    misOtrMutedRef :: Maybe Text,
    misOtrArchived :: Maybe Bool,
    misOtrArchivedRef :: Maybe Text,
    misHidden :: Maybe Bool,
    misHiddenRef :: Maybe Text,
    misConvRoleName :: Maybe RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MemberUpdateData)

modelMemberUpdateData :: Doc.Model
modelMemberUpdateData = Doc.defineModel "MemberUpdateData" $ do
  Doc.description "Event data on member updates"
  Doc.property "target" Doc.bytes' $ do
    Doc.description "Target ID of the user that the action was performed on"
    Doc.optional
  Doc.property "otr_muted" Doc.bool' $ do
    Doc.description "Whether to notify on conversation updates"
    Doc.optional
  Doc.property "otr_muted_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)muting"
    Doc.optional
  Doc.property "otr_archived" Doc.bool' $ do
    Doc.description "Whether to notify on conversation updates"
    Doc.optional
  Doc.property "otr_archived_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)archiving"
    Doc.optional
  Doc.property "hidden" Doc.bool' $ do
    Doc.description "Whether the conversation is hidden"
    Doc.optional
  Doc.property "hidden_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)hiding"
    Doc.optional
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role to update to"
    Doc.optional

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
    MemberUpdateData
      <$> m .:? "target"
      <*> m .:? "otr_muted"
      <*> m .:? "otr_muted_status"
      <*> m .:? "otr_muted_ref"
      <*> m .:? "otr_archived"
      <*> m .:? "otr_archived_ref"
      <*> m .:? "hidden"
      <*> m .:? "hidden_ref"
      <*> m .:? "conversation_role"

data OtrMessage = OtrMessage
  { otrSender :: ClientId,
    otrRecipient :: ClientId,
    otrCiphertext :: Text,
    otrData :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OtrMessage)

modelOtrMessage :: Doc.Model
modelOtrMessage = Doc.defineModel "OtrMessage" $ do
  Doc.description "Encrypted message of a conversation"
  Doc.property "sender" Doc.bytes' $
    Doc.description "The sender's client ID"
  Doc.property "recipient" Doc.bytes' $
    Doc.description "The recipient's client ID"
  Doc.property "text" Doc.bytes' $
    Doc.description "The ciphertext for the recipient (Base64 in JSON)"
  Doc.property "data" Doc.bytes' $ do
    Doc.description
      "Extra (symmetric) data (i.e. ciphertext, Base64 in JSON) \
      \that is common with all other recipients."
    Doc.optional

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
    OtrMessage
      <$> o .: "sender"
      <*> o .: "recipient"
      <*> o .: "text"
      <*> o .:? "data"
